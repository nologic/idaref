import time, threading
import sqlite3 as sq
import os
import inspect
import glob

class InstructionReference:
	def __init__(self):
		self.ref_term = False
		self.v = None
		self.inst_map = {}
		self.last_inst = None

		self.create(os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe()))))

	def create(self, path="."):
		doc_opts = glob.glob(path + os.sep + "*.sqlite")
		
		prompt = ["What platform do you want to use?"]
		i = 1
		for c in doc_opts:
			basefile = os.path.splitext(os.path.basename(c))[0]
			prompt.append("%d - %s" % (i, basefile))
			i = i + 1

		sel = AskLong(1, "\n".join(prompt))
		dbpath = doc_opts[int(sel) - 1]

		con = sq.connect(dbpath)
		cur = con.cursor()
		cur.execute("SELECT mnem, description FROM instructions")
		con.commit()

		rows = cur.fetchall()
		for row in rows:
			inst = row[0]
			lines = row[1].split("\n")

			self.inst_map[inst] = lines

		con.close()

		for (inst, data) in self.inst_map.iteritems():
			if(data[0][0:3] == "-R:"):
				ref = data[0][3:]

				if(ref in self.inst_map):
					self.inst_map[inst] = self.inst_map[ref]
		
		title = "Instruction Reference"

		self.v = idaapi.find_tform(title)
		if(self.v == None):
			self.v = idaapi.simplecustviewer_t()
			self.v.Create(title)

		self.v.Show()

		threading.Timer(1, lambda: self.update()).start()

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
		inst = self.cleanInstruction(GetMnem(ScreenEA()))

		if(inst != self.last_inst):
			self.last_inst = inst
			
			self.v.ClearLines()
			#self.v.Refresh()
			
			if(inst in self.inst_map):
				text = self.inst_map[inst]

				for line in text:
					line = line.encode('utf8')
					self.v.AddLine(line)

			else:
				self.v.AddLine(inst + " not documented.")

			self.v.Refresh()

		if(not self.ref_term):
			threading.Timer(1, lambda: self.update()).start()

	def terminate(self):
		self.ref_term = True

ref = InstructionReference()

print "Reference View loaded, ref:" + str(ref)