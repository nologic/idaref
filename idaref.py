import time, threading
import sqlite3 as sq
import os

ref_term = False
v = None
db_dir = None
inst_map = {}

def create(path="."):
	global v
	global db_dir

	db_dir = path
	dbpath = db_dir + os.sep + 'asm.sqlite'

	con = sq.connect(dbpath)
	cur = con.cursor()
	cur.execute("SELECT mnem, description FROM instructions")
	con.commit()

	rows = cur.fetchall()
	for row in rows:
		inst = row[0]
		lines = row[1].split("\n")
		inst_map[inst] = lines

	con.close()

	print len(inst_map)
		
	v = idaapi.simplecustviewer_t()
	v.Create("Instruction Reference")
	v.Show()

	threading.Timer(1, update).start()

def update():
	v.ClearLines()
	v.Refresh
	inst = GetMnem(ScreenEA())

	text = inst_map[inst.upper()]

	for line in text:
		line = line.encode('utf8')
		v.AddLine(line)

	if(text == None):
		v.AddLine(inst + " not documented.")

	v.Refresh()

	if(not ref_term):
		threading.Timer(1, update).start()

def terminate():
	global ref_term

	ref_term = True

print "Reference View loaded"