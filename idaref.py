import time, threading
import sqlite3 as sq
import os

ref_term = False
v = None
db_dir = None

def create(path="."):
	global v
	global db_dir

	db_dir = path
	v = idaapi.simplecustviewer_t()
	v.Create("Instruction Reference")
	v.Show()

	threading.Timer(1, update).start()

def update():
	dbpath = db_dir + os.sep + 'asm.sqlite'

	con = sq.connect(dbpath)
	cur = con.cursor()

	v.ClearLines()
	v.Refresh()
	
	inst = GetMnem(ScreenEA())

	cur.execute("SELECT description FROM instructions WHERE mnem=:mnem LIMIT 1", {"mnem": inst.upper()})
	con.commit()

	rows = cur.fetchall()
	for row in rows:
		for line in row[0].split("\n"):
			line = line.encode('utf-8')
			v.AddLine(line)

		v.AddLine("+++++++")

	if(len(rows) == 0):
		v.AddLine(inst + " not documented.")

	v.Refresh()
	con.close()

	if(not ref_term):
		threading.Timer(1, update).start()

def terminate():
	global ref_term

	ref_term = True

print "Reference View loaded"