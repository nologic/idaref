import time, threading
import sqlite3 as sq
import os
import inspect
import glob

class InstructionReference(idaapi.simplecustviewer_t):
    def __init__(self):
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
        self.destroying = False;

        self.base_path = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))

        self.archs = self.findManuals()

        print "available architectures %s" % str(self.archs)

        self.create()
        self.loadArchitecture(self.getIdaArchitecture())

    def create(self):
        if(idaapi.find_tform(self.title) == None):
            if(not idaapi.simplecustviewer_t.Create(self, self.title)):
                print "Unable to open"
                return False

            self.menu_update = self.AddPopupMenu("Update View")
            self.menu_lookup = self.AddPopupMenu("Lookup Instruction")
            self.menu_autorefresh = self.AddPopupMenu("Toggle Auto-refresh")
            self.change_arch = self.AddPopupMenu("Change Architecture")

            self.Show()

            def update():
                if (self.destroying == True or idaapi.find_tform(self.title) == None):
                    return -1
                else:
                    if(self.do_auto):
                        self.update()

                    return 1000

            if('register_timer' in dir(idaapi)):
                idaapi.register_timer(1000, update)

                self.is_loaded = True
            else:
                print "Sorry I can't support auto-refresh in your version of IDA."
                print "Use 'ref.update()' to get documentation for your instruction."
        else:
            print "Already loaded. Please close old instance first."

    def destroy(self):
        self.destroying = True
        self.is_loaded = False
        window = idaapi.find_tform(self.title)

        if(window):
            idaapi.close_tform(window, 0)

    def findManuals(self):
        doc_opts = glob.glob(self.base_path + os.sep + "*.sql")

        if(len(doc_opts) == 0):
            Warning("Couldn't find any databases in " + path)
            return

        available = []
        
        for c in doc_opts:
            basefile = os.path.splitext(os.path.basename(c))[0]            
            available.append(basefile)

        return available

    def askArchitecture(self, availList):
        prompt = ["What platform do you want to use?"]

        i = 1
        for arch in availList:
            prompt.append("%d - %s" % (i, arch))
            i = i + 1

        sel = AskLong(1, "\n".join(prompt))

        if(sel is None):
            return None

        sel = int(sel)

        if(sel > 0 and sel <= len(availList)):
            return availList[sel - 1]

        return None

    def loadArchitecture(self, name):
        # fix up name
        name = name.lower()
        if(name == "metapc"):
            name = "x86-64"

        self.arch = name

        path = self.base_path
        dbpath = path + os.sep + name + ".sql"

        if(not os.path.isfile(dbpath)):
            print "Manual not found for architecture: %s" % name
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

            lines[0] = inst + ": " + lines[0]
            self.inst_map[inst] = lines

        con.close()

        for (inst, data) in self.inst_map.iteritems():
            if(data[0][0:3] == "-R:"):
                ref = data[0][3:]

                if(ref in self.inst_map):
                    self.inst_map[inst] = self.inst_map[ref]

        print "Manual loaded for architecture: %s" % name
        return True

    def getIdaArchitecture(self):
        inf = idaapi.get_inf_structure()

        return inf.procName

    def OnClose(self):
        self.destroying = True
        self.is_loaded = False

        # give clean up a chance, to prevent a crash
        #  because I can't detect in update function
        #  that IDA is closing.
        time.sleep(1)

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

    def update(self, force = False):
        inst = self.cleanInstruction(GetMnem(ScreenEA()))

        if(inst != self.last_inst or force == True):
            self.load_inst(inst)
            
    def load_inst(self, inst, wasLookup = False):
        inst = self.cleanInstruction(inst)

        if(wasLookup == False):
            self.last_inst = inst
        
        self.ClearLines()
        
        if(inst in self.inst_map):
            text = self.inst_map[inst]

            for line in text:
                self.AddLine(line)

        else:
            self.AddLine(inst + " not documented.")

        self.Refresh()
        self.Jump(0, 0)

    def OnPopupMenu(self, menu_id):
        if menu_id == self.menu_update:
            self.update(True)
        elif menu_id == self.menu_lookup:
            inst = AskStr(self.last_inst, "Instruction: ")
            if(inst != None):
                self.load_inst(inst, True)
        elif menu_id == self.menu_autorefresh:
            self.do_auto = not self.do_auto
        elif menu_id == self.change_arch:
            arch = self.askArchitecture(self.archs)

            if(arch != None):
                self.loadArchitecture(arch)
                self.update(True)
        else:
            # Unhandled
            return False
        return True

"""
IDA Pro Plugin Interface
Define an IDA Python plugin required class and function.

Inpired by idarest plugin.
"""

MENU_PATH = 'Edit/Other'
class idaref_plugin_t(idaapi.plugin_t):
    flags = idaapi.PLUGIN_KEEP
    comment = ""

    help = "IdaRef: Presents complete instruction reference for an instruction under cursor"
    wanted_name = "IDA Instruction Reference"
    wanted_hotkey = "Alt-8"

    def _add_menu(self, *args):
        ctx = idaapi.add_menu_item(*args)
        if ctx is None:
            idaapi.msg("Add failed!\n")
            return False
        else:
            self.ctxs.append(ctx)
            return True

    def _add_menus(self):
        ret = []
        ret.append(self._add_menu(MENU_PATH, 'Stop IdaRef', '', 1, self.stop, tuple()))
        ret.append(self._add_menu(MENU_PATH, 'Start IdaRef', '', 1, self.start, tuple()))
        
        if False in ret:
            return idaapi.PLUGIN_SKIP
        else:
            return idaapi.PLUGIN_KEEP


    def init(self):
        self.ctxs = []
        self.ref = None

        ret = self._add_menus()
        idaapi.msg("IdaRef initialized\n")

        return ret

    def start(self, *args):
        idaapi.msg("Starting IdaRef\n")
        
        if(self.ref != None and idaapi.find_tform(ref.title) == None):
            self.stop()

        if(self.ref == None):
            self.ref = InstructionReference()
        else:
            print "IdaRef Already started"

    def stop(self, *args):
        idaapi.msg("Stopping IdaRef\n")

        if(self.ref != None):
            self.ref.destroy()
            self.ref = None
        else:
            print "IdaRef is not running"

    def run(self, arg):
        pass

    def term(self):
        idaapi.msg("Terminating %s\n" % self.wanted_name)
        try:
            self.stop()
        except:
            pass

        for ctx in self.ctxs:
            idaapi.del_menu_item(ctx)

def PLUGIN_ENTRY():
    return idaref_plugin_t()