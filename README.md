IdaRef
======
IDA Pro Full Instruction Reference Plugin - It's like auto-comments but useful.

I'm generally pretty good at figuring out what various Intel instructions do.
But, once in a while I need to either know some precise detail (i.e. exact 
side effects of SUB) or come across a rare instruction. Then I break my train
of thought and have dig out the reference manual. Which got me thinking: 
<i>Why can't IDA just give me the full documentation?</i>

Enter IdaRef:
![](https://raw.githubusercontent.com/nologic/idaref/master/screenshot/idaref.png)
The plugin will monitor the location for your cursor (ScreenEA) and display the full
documentation of the instruction. At the moment it only supports x86-64, however
support for other architectures is relatively easy.

Usage
-----
Simply checkout or download the repository and make sure that the sqlite database
file is in the same directory as the python script.

Execute the python script via File->Script File... or ALT+F7. This will open a new
view in your IDA workspace. This is where the text will be displayed.

If you don't like automatic loading of the reference the run this command. It will 
disable the automatic refresh.

    ref.terminate()

If the automatic refresh doesn't work (I've had problems under Wine) then you can
force a refresh to reload the text by calling update. This will work even after
terminate was called.

    ref.update()
    
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
