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
    
Enjoy!
