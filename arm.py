import os
import sqlite3

input = 'arm.sql'
output = 'arm.sqlite'
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
