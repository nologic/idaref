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

		self.create(os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe()))))

	def create(self, path="."):
		doc_opts = glob.glob(path + os.sep + "*.sqlite")
		
		if(len(doc_opts) == 0):
			Warning("Couldn't find any databases in " + path)

		dbpath = doc_opts[0]
		if(len(doc_opts) > 1):
			prompt = ["What platform do you want to use?"]
			i = 1
			for c in doc_opts:
				basefile = os.path.splitext(os.path.basename(c))[0]
				prompt.append("%d - %s" % (i, basefile))
				i = i + 1

			sel = AskLong(1, "\n".join(prompt))
			dbpath = doc_opts[int(sel) - 1]

		print "Using database: " + dbpath

		con = sq.connect(dbpath)
		con.text_factory = str
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
		
		title = "Instruction Reference"

		if(idaapi.find_tform(title) == None):
			if(not idaapi.simplecustviewer_t.Create(self, title)):
				print "Unable to open"
				return False

			self.menu_update = self.AddPopupMenu("Update View")
			self.menu_lookup = self.AddPopupMenu("Lookup Instruction")
			self.menu_autorefresh = self.AddPopupMenu("Toggle Auto-refresh")

			self.Show()

			def update():
				if(self.do_auto):
					self.update()

				return -1 if idaapi.find_tform(title) == None else 1000

			if('register_timer' in dir(idaapi)):
				idaapi.register_timer(1000, update)

				self.is_loaded = True
			else:
				print "Sorry I can't support auto-refresh in your version of IDA."
				print "Use 'ref.update()' to get documentation for your instruction."
		else:
			print "Already loaded. Please close old instance first."

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

	def update(self):
		inst = GetMnem(ScreenEA())

		if(inst != self.last_inst):
			self.load_inst(inst)
			
	def load_inst(self, inst):
		inst = self.cleanInstruction(inst)
		self.last_inst = inst
		
		self.ClearLines()
		
		if(inst in self.inst_map):
			text = self.inst_map[inst]

			for line in text:
				line = line.encode('utf8', 'ignore')
				self.AddLine(line)

		else:
			self.AddLine(inst + " not documented.")

		self.Refresh()
		self.Jump(0, 0)

	def OnPopupMenu(self, menu_id):
		if menu_id == self.menu_update:
		 	self.update()
		elif menu_id == self.menu_lookup:
			inst = AskStr(self.last_inst, "Instruction: ")
			if(inst != None):
				self.load_inst(inst)
		elif menu_id == self.menu_autorefresh:
			self.do_auto = not self.do_auto
		else:
			# Unhandled
			return False
		return True

ref = InstructionReference()

if(ref.is_loaded):
	print "Reference View loaded, ref:" + str(ref)