import sqlite3 as sq
import os
import inspect
import glob

doc = Document.getCurrentDocument()

seg = doc.getCurrentSegment()
adr = doc.getCurrentAddress()

doc.log("-----------")
doc.log("Documentation for instruction at " + hex(adr))

instr = seg.getInstructionAtAddress(adr)

# not sure why but stringForArchitecture returns <unknown> for arm/v7 (id 4)
if instr.getArchitecture() == 4:
    arch = "arm/v7"
elif instr.getArchitecture() == 5:
    arch = "arm/v8"
else:
    arch = instr.stringForArchitecture(instr.getArchitecture())

doc.log("Architecture: %s" % arch)
doc.log("instruction: " + instr.getInstructionString())
doc.log("instruction length: %d" % instr.getInstructionLength())


class InstructionReference:
    def __init__(self, mnem, arch):
        self.arch = arch
        self.mnem = mnem
        self.ref_term = False
        self.inst_map = {}
        self.last_inst = None
        self.is_loaded = False
        self.do_auto = True

        self.menu_update = None
        self.menu_lookup = None
        self.menu_autorefresh = None
        self.change_arch = None

        self.title = "Instruction Reference"
        self.destroying = False

        self.base_path = os.path.abspath(os.path.expanduser("~/Library/Application Support/Hopper/Scripts"))

        self.archs = self.findManuals()

        self.loadArchitecture(arch)
        self.load_inst(mnem)

    def findManuals(self):
        doc_opts = glob.glob(self.base_path + os.sep + "*.sql")

        if(len(doc_opts) == 0):
            doc.log("Couldn't find any databases in %s" % self.base_path)
            return

        available = []

        for c in doc_opts:
            basefile = os.path.splitext(os.path.basename(c))[0]
            available.append(basefile)

        return available

    def loadArchitecture(self, name):
        # fix up name
        name = name.lower()
        if name == "x86_64" or name == "x86" or name == "i386":
            name = "x86-64"
        elif name.startswith("arm"):
            name = "arm"

        self.arch = name

        path = self.base_path
        dbpath = path + os.sep + name + ".sql"

        if(not os.path.isfile(dbpath)):
            doc.log("Manual not found for architecture: %s" % name)
            return False

        con = sq.connect(":memory:")
        con.text_factory = str
        con.executescript(open(dbpath).read())

        cur = con.cursor()
        cur.execute("SELECT mnem, description FROM instructions")
        con.commit()

        rows = cur.fetchall()
        for row in rows:
            inst = row[0]
            lines = row[1].replace("\r\n", "\n").split("\n")

            if not lines[0].startswith("-R:"):
                lines[0] = inst + ": " + lines[0]
            self.inst_map[inst] = lines

        con.close()

        for (inst, data) in self.inst_map.iteritems():
            if (data[0].startswith("-R:")):
                ref = data[0][3:]

                if(ref in self.inst_map):
                    self.inst_map[inst] = self.inst_map[ref]

        doc.log("Manual loaded for architecture: %s" % name)
        return True

    def cleanInstruction(self, inst):
        inst = inst.upper()
        # hacks for x86
        if(inst[0:1] == 'J' and inst != 'JMP'):
            inst = "Jcc"
        elif(inst[0:4] == "LOOP"):
            inst = "LOOP"
        elif(inst[0:3] == "INT"):
            inst = "INT n"
        elif(inst[0:5] == "FCMOV"):
            inst = "FCMOVcc"
        elif(inst[0:4] == "CMOV"):
            inst = "CMOVcc"
        elif(inst[0:3] == "SET"):
            inst = "SETcc"

        return inst

    def load_inst(self, inst):
        inst = self.cleanInstruction(inst)

        if(inst in self.inst_map):
            text = self.inst_map[inst]

            doc.log('\n'.join(text))

        else:
            doc.log(inst + " not documented.")


ref = InstructionReference(instr.getInstructionString(), arch)
