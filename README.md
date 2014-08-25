IdaRef
======
IDA Pro Full Instruction Reference Plugin - It's like auto-comments but useful.

I'm generally pretty good at figuring out what various Intel instructions do.
But, once in a while I need to either know some precise detail (i.e. exact 
side effects of SUB) or come across a rare instruction. Then I break my train
of thought and have to dig out the reference manual. Which got me thinking: 
<i>Why can't IDA just give me the full documentation?</i>

Enter IdaRef:
![](https://raw.githubusercontent.com/nologic/idaref/master/screenshot/idaref.png)
The plugin will monitor the location for your cursor (ScreenEA) and display the full
documentation of the instruction. At the moment it only supports x86-64, however
adding support for other architectures is relatively easy.

Usage
-----
Simply checkout or download the repository and make sure that the sqlite database
file is in the same directory as the 'idaref' python script.

Execute the python script via File->Script File... or ALT+F7. This will open a new
view in your IDA workspace. In this view is where the text will be displayed. Now
click on the instruction you're curious about and within a second the documentation
will populate the view.

If you don't like automatic loading of the reference then run this command. It will 
disable the automatic refresh.

    ref.terminate()

If the automatic refresh doesn't work (I've had problems under Wine) then you can
force a refresh to reload the text by calling update. This will work even after
terminate was called.

    ref.update()
    
Internals
---------
Upon loading the script will look for SQlite databases in the same directory as the 
itself. The naming convention for the database files is [arch name].sqlite. The 
[arch name] will be presented to the user as choice.

The database has a table called 'instructions' and two columns called 'mnem' and
'description'. The instructions are looked up case insensitive (upper case) by the
mnem value. The text from description is displayed verbatim in the view.

To add support for more architectures simply create a new database with those
columns and place it in the the script directory.

    import sqlite3 as sq
    con = sq.connect("asm.sqlite")
    con.text_factory = str
    cur = con.cursor()
    cur.execute("CREATE TABLE IF NOT EXISTS instructions (platform TEXT, mnem TEXT, description TEXT)")
    con.commit()
    
When working with x86, I noticed that many instructions point to the same documentation.
So, the plugin supports single level referencing. Just place '-R:[new instruction]' into
description to redirect the loading. 'new instruction' the the target. So, when loading 
the script will detect the link and load the new target automatically.

    cur.execute("INSERT INTO instructions VALUES (?, ?, ?)", ("x86", inst, "-R:%s" % first_inst))
    
Skeletons in the closet
-----------------------
* I found that IDA's GUI is not as stable as I'd want it to be. The frequent 
refreshes can make it crash. It caused me problems under Wine and RDP in the
past. I've reduced the amount of work that the GUI has to do and it has been
working well for me.

* The documentation database was created using a rather hackish screen scraping
technique by the x86doc project which I forked. So, there are probably some 
strange characters or tags in the text. At least, it is a mechanical process
so I expect that the information is correct relative to the original Intel PDF.

Enjoy!
------
