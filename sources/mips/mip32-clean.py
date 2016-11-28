import re
import sqlite3 as sq

def shift_left(lines):
	spaceLen = 999999

	for line in lines:
		if(len(line.strip()) > 0):
			m = re.search(r'([ ]+)?.*', line)
			spaceLen = min(len(m.group(1)), spaceLen)

	for i in range(0, len(lines)):
		lines[i] = lines[i].encode('utf8', 'ignore')
		if(len(line) > 0):
			lines[i] = lines[i][spaceLen:]

	return lines

data = open("MIPS_32.txt").read()

outlines = []
skip = 0

for line in data.split("\n"):
	if(skip > 0):
		skip -= 1
	elif(line.find("MIPS32TM") >= 0):
		while(len(outlines) > 0 and len(outlines[-1].strip()) == 0):
			outlines.pop()

		skip = 1
	else:
		outlines.append(line)

con = sq.connect("mips32.sqlite")
con.text_factory = str
cur = con.cursor()
cur.execute("CREATE TABLE IF NOT EXISTS instructions (platform TEXT, mnem TEXT, description TEXT)")
con.commit()

instsAdded = {}
curInst = None
instData = []
for line in outlines:
	line = line.replace('\r', '')

	print "'%s'" % line
	line = line.encode('utf8', 'ignore')

	if(skip > 0):
		skip -= 1
		continue

	if(len(line) > 0 and line[0] != ' '):
		if(line.find("(cont.)") > 0):
			skip = 2
			continue
		else:
			#close instruction
			if(curInst != None):
				insts = re.split("[, ]+", curInst)
				added = 0
				for inst in insts:
					if(inst[0] == '('):
						break
					else:
						if(inst not in instsAdded):
							instsAdded[inst] = True
							if(added > 0):
								cur.execute("INSERT INTO instructions VALUES (?, ?, ?)", ("MIPS32", inst, "-R:%s" % insts[0]))
							else:
								instData = shift_left(instData)
								cur.execute("INSERT INTO instructions VALUES (?, ?, ?)", ("MIPS32", inst, "\n".join(instData)))
								added += 1
		print "'%s'" % line
		# start new inst
		m = re.search(r'([a-zA-Z0-9\.]+).*?', line)
		curInst = m.group(1)
		instData = []
	else:
		instData.append(line)

con.commit()
con.close()

print instsAdded