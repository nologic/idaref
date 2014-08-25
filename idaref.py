import time, threading
import sqlite3 as sq
import os

class InstructionReference:
	def __init__(self):
		self.ref_term = False
		self.v = None
		self.db_dir = None
		self.inst_map = {}
		self.last_inst = None

	def create(self, path="."):
		self.db_dir = path
		dbpath = self.db_dir + os.sep + 'asm.sqlite'

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
			
		self.v = idaapi.simplecustviewer_t()
		self.v.Create("Instruction Reference")
		self.v.Show()

		threading.Timer(1, lambda: self.update()).start()

	def update(self):
		inst = GetMnem(ScreenEA()).upper()

		if(inst != self.last_inst):
			self.last_inst = inst
			
			self.v.ClearLines()
			self.v.Refresh()
			
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