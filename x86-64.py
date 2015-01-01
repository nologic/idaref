import os
import sqlite3

input = 'x86-64.sql'
output = 'x86-64.sqlite'
try:
    os.remove(output)
except:
    pass
con = sqlite3.connect(output)
f = open(input)
sql = f.read()
f.close()
con.executescript(sql)
con.close()
print 'all done!'