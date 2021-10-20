import time, threading
import sqlite3 as sq
import os
import inspect
import glob
import idaapi
from idc import print_insn_mnem, get_screen_ea
from ida_kernwin import ask_long, find_widget, close_widget, ask_str


initialized = False
insref_g = None
# we try because of ida versions below 6.8, and write action handlers below
try:

    class StopHandler(idaapi.action_handler_t):
        def __init__(self):
            idaapi.action_handler_t.__init__(self)

        def activate(self, ctx):
            b = idaref_plugin_t()
            b.stop()
            return 1

        # This action is always available.
        def update(self, ctx):
            return idaapi.AST_ENABLE_ALWAYS


except AttributeError:
    pass

try:

    class StartHandler(idaapi.action_handler_t):
        def __init__(self):
            idaapi.action_handler_t.__init__(self)

        def activate(self, ctx):
            b = idaref_plugin_t()
            b.start()
            return 1

        # This action is always available.
        def update(self, ctx):
            return idaapi.AST_ENABLE_ALWAYS


except AttributeError:
    pass


class InstructionReference(idaapi.simplecustviewer_t):
    def __init__(self, owner):
        super(InstructionReference, self).__init__()
        self.owner = owner
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

        self.base_path = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))

        self.archs = self.findManuals()

        print("available architectures %s" % str(self.archs))

        self.create()
        self.loadArchitecture(self.getIdaArchitecture())

    def create(self):
        if find_widget(self.title) == None:
            if not idaapi.simplecustviewer_t.Create(self, self.title):
                print("Unable to open")
                return False

            if idaapi.IDA_SDK_VERSION >= 700:
                self.menu_update = 1
                self.menu_lookup = 2
                self.menu_autorefresh = 3
                self.change_arch = 4

                class Hooks(idaapi.UI_Hooks):
                    class PopupActionHandler(idaapi.action_handler_t):
                        def __init__(self, owner, menu_id):
                            self.owner = owner
                            self.menu_id = menu_id

                        def activate(self, ctx):
                            self.owner.OnPopupMenu(self.menu_id)

                        def update(self, ctx):
                            return idaapi.AST_ENABLE_ALWAYS

                    def __init__(self, form):
                        idaapi.UI_Hooks.__init__(self)
                        self.form = form

                    def finish_populating_widget_popup(self, widget, popup):
                        if self.form.title == idaapi.get_widget_title(widget):
                            idaapi.attach_dynamic_action_to_popup(
                                widget,
                                popup,
                                idaapi.action_desc_t(
                                    None, "Update View", self.PopupActionHandler(self.form, self.form.menu_update), None, None, -1
                                ),
                            )
                            idaapi.attach_dynamic_action_to_popup(
                                widget,
                                popup,
                                idaapi.action_desc_t(
                                    None,
                                    "Lookup Instruction",
                                    self.PopupActionHandler(self.form, self.form.menu_lookup),
                                    None,
                                    None,
                                    -1,
                                ),
                            )
                            idaapi.attach_dynamic_action_to_popup(
                                widget,
                                popup,
                                idaapi.action_desc_t(
                                    None,
                                    "Toggle Auto-refresh",
                                    self.PopupActionHandler(self.form, self.form.menu_autorefresh),
                                    None,
                                    None,
                                    -1,
                                ),
                            )
                            idaapi.attach_action_to_popup(widget, popup, "-", None)
                            idaapi.attach_dynamic_action_to_popup(
                                widget,
                                popup,
                                idaapi.action_desc_t(
                                    None,
                                    "Change Architecture",
                                    self.PopupActionHandler(self.form, self.form.change_arch),
                                    None,
                                    None,
                                    -1,
                                ),
                            )
                            idaapi.attach_action_to_popup(widget, popup, "-", None)

                self.hooks = Hooks(self)
                self.hooks.hook()
            else:
                self.menu_update = self.AddPopupMenu("Update View")
                self.menu_lookup = self.AddPopupMenu("Lookup Instruction")
                self.menu_autorefresh = self.AddPopupMenu("Toggle Auto-refresh")
                self.change_arch = self.AddPopupMenu("Change Architecture")

            self.Show()

            def update():
                if self.destroying == True:
                    return -1
                else:
                    if self.do_auto:
                        self.update()

                    return 200

            if "register_timer" in dir(idaapi):
                idaapi.register_timer(200, update)

                self.is_loaded = True
            else:
                print("Sorry I can't support auto-refresh in your version of IDA.")
                print("Use 'ref.update()' to get documentation for your instruction.")
        else:
            print("Already loaded. Please close old instance first.")

    def destroy(self):
        self.destroying = True
        self.is_loaded = False
        self.hooks.unhook()
        window = find_widget(self.title)

        if window:
            close_widget(window, 0)

    def findManuals(self):
        search_path = os.path.join(self.base_path, "archs", "*.sql")
        doc_opts = glob.glob(search_path)

        if len(doc_opts) == 0:
            Warning("Couldn't find any databases in " + search_path)
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

        sel = ask_long(1, "\n".join(prompt))

        if sel is None:
            return None

        sel = int(sel)

        if sel > 0 and sel <= len(availList):
            return availList[sel - 1]

        return None

    def loadArchitecture(self, name):
        # fix up name
        name = name.lower()
        if name == "metapc":
            name = "x86-64"

        self.arch = name

        path = self.base_path
        dbpath = os.path.join(path, "archs", name + ".sql")

        if not os.path.isfile(dbpath):
            print("Manual not found for architecture: %s" % name)
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

            self.inst_map[inst] = lines

        con.close()

        for (inst, data) in self.inst_map.items():
            data = data[0]

            if data[0:3] == "-R:":
                ref = data[3:]

                if ref in self.inst_map:
                    self.inst_map[inst] = self.inst_map[ref]

        print("Manual loaded for architecture: %s" % name)
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
        if self.arch == "x86-64":
            inst = inst.upper()
            # hacks for x86
            if inst[0:1] == "J" and inst != "JMP":
                inst = "Jcc"
            elif inst[0:4] == "LOOP":
                inst = "LOOP"
            elif inst[0:3] == "INT":
                inst = "INT n"
            elif inst[0:5] == "FCMOV":
                inst = "FCMOVcc"
            elif inst[0:4] == "CMOV":
                inst = "CMOVcc"
            elif inst[0:3] == "SET":
                inst = "SETcc"

        return inst

    def update(self, force=False):
        inst = self.cleanInstruction(print_insn_mnem(get_screen_ea()))

        if inst != self.last_inst or force == True:
            self.load_inst(inst)

    def load_inst(self, inst, wasLookup=False):
        inst = self.cleanInstruction(inst)

        if wasLookup == False:
            self.last_inst = inst

        self.ClearLines()

        if inst not in self.inst_map:
            inst = inst.upper()

        if inst in self.inst_map:
            text = self.inst_map[inst]

            if len(text) > 0:
                self.AddLine(inst + ": " + text[0])
                if len(text) > 1:
                    for line in text[1:]:
                        self.AddLine(line)

        else:
            self.AddLine(inst + " not documented.")

        self.Refresh()
        self.Jump(0, 0)

    def OnPopupMenu(self, menu_id):
        if menu_id == self.menu_update:
            self.update(True)
        elif menu_id == self.menu_lookup:
            inst = ask_str(self.last_inst, 0, "Instruction: ")
            if inst != None:
                self.load_inst(inst, True)
        elif menu_id == self.menu_autorefresh:
            self.do_auto = not self.do_auto
        elif menu_id == self.change_arch:
            arch = self.askArchitecture(self.archs)

            if arch != None:
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

MENU_PATH = "Edit/idaref/"
ALTERNATIVE_MENU_PATH = "Edit/Patch Program/"


class idaref_plugin_t(idaapi.plugin_t):
    flags = idaapi.PLUGIN_KEEP
    comment = ""

    help = "IdaRef: Presents complete instruction reference for an instruction under cursor"
    wanted_name = "IDA Instruction Reference"
    wanted_hotkey = "Alt-8"
    website = "https://github.com/nologic/idaref"

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
        if idaapi.IDA_SDK_VERSION <= 695:
            menu_path = MENU_PATH if idaapi.IDA_SDK_VERSION > 660 else ALTERNATIVE_MENU_PATH
            ret.append(self._add_menu(menu_path, "Stop IdaRef", "", 1, self.stop, tuple()))
            ret.append(self._add_menu(menu_path, "Start IdaRef", "", 1, self.start, tuple()))

        if idaapi.IDA_SDK_VERSION >= 700:
            action_desc = idaapi.action_desc_t(
                "idaref:stop",  # The action name. Must be unique
                "Stop Idaref",  # Action Text
                StopHandler(),  # Action handler
                "",  # Optional shortcut
                "Stop Idaref",  # Action tooltip
            )
            idaapi.register_action(action_desc)
            idaapi.attach_action_to_menu(MENU_PATH, "idaref:stop", idaapi.SETMENU_APP)

            action_desc = idaapi.action_desc_t(
                "idaref:start",  # The action name. Must be unique
                "Start Idaref",  # Action Text
                StartHandler(),  # Action handler
                "",  # Optional shortcut
                "Start Idaref",  # Action tooltip
            )
            idaapi.register_action(action_desc)
            idaapi.attach_action_to_menu(MENU_PATH, "idaref:start", idaapi.SETMENU_APP)

        if False in ret:
            return idaapi.PLUGIN_SKIP
        else:
            return idaapi.PLUGIN_KEEP

    def init(self):
        global initialized
        ret = idaapi.PLUGIN_SKIP
        if initialized == False:
            initialized = True
            self.ctxs = []
            insref_g = None
            ret = self._add_menus()
        idaapi.msg("IdaRef initialized\n")

        return ret

    def start(self, *args):
        global insref_g
        idaapi.msg("Starting IdaRef\n")

        if insref_g != None and find_widget(insref_g.title) == None:
            self.stop()

        if insref_g == None:
            insref_g = InstructionReference(self)
        else:
            print("IdaRef Already started")

    def stop(self, *args):
        global insref_g
        idaapi.msg("Stopping IdaRef\n")

        if insref_g != None:
            insref_g.destroy()
            insref_g = None
        else:
            print("IdaRef is not running")

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
