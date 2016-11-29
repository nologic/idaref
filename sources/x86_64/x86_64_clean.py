import re
import sqlite3 as sq
import pprint

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

pp = pprint.PrettyPrinter(depth=4)

data = open("intel_instructions.txt").read()

outlines = []
skip = 0

skipBlank = False

for line in data.split("\n"):
	if(skipBlank and len(line.strip()) == 0):
		pass
	elif(skipBlank and len(line.strip()) > 0):
		skipBlank = False
		outlines.append("")
		outlines.append(line)
	elif(line.find("Vol. ") > 0):
		while(outlines[len(outlines) - 1] == ''):
			outlines.pop()
	elif(line.find("INSTRUCTION SET REFERENCE") >= 0):
		skipBlank = True
	else:
		outlines.append(line)

instLines = []
instBuffer = []

for line in outlines:
	instBuffer.append(line)
	opInd = line.find("Opcode")

	if(opInd >= 0 and opInd < 10):
		# create a new buffer
		lastBuffer = instBuffer
		instBuffer = []

		instLines.append(instBuffer)

		# copy lines from old buffer
		while len(lastBuffer) > 0:
			lastBuffLine = lastBuffer.pop()

			if(lastBuffLine == ''):
				break
			else:
				instBuffer.append(lastBuffLine)

		if(len(instBuffer) < 2):
			instLines.pop()

			while(len(instBuffer) > 0):
				lastBuffer.append(instBuffer.pop())

			instBuffer = lastBuffer
		else:
			instBuffer.reverse()



con = sq.connect("x86_64.sqlite")
con.text_factory = str
cur = con.cursor()
cur.execute("CREATE TABLE IF NOT EXISTS instructions (platform TEXT, mnem TEXT, description TEXT)")
con.commit()

for docLines in instLines:
	title = docLines[0]

	dash = title.find('-')
	inst = title[0:dash].strip()
	instRefs = inst.split('/')
	print instRefs

	print "INSERT INTO instructions VALUES (?, ?, ?)", ("x86_64", instRefs[0], "\n".join(docLines)[0:20])
	cur.execute("INSERT INTO instructions VALUES (?, ?, ?)", ("x86_64", instRefs[0], "\n".join(docLines)))

	for iref in instRefs[1:]:
		iref = iref.strip()
		print "INSERT INTO instructions VALUES (?, ?, ?)", ("x86_64", iref, "-R:%s" % instRefs[0])
		cur.execute("INSERT INTO instructions VALUES (?, ?, ?)", ("x86_64", iref, "-R:%s" % instRefs[0]))

con.commit()
con.close()




