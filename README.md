qsrun
=====

-----

####Version 0.4




Lightweight mouse driven overlay for launching anything.

qsrun is a lightweight launcher that uses a grid of 9 buttons to launch scripts or programs. 

The binary is included for those that don't want to install ghc and all required libraries just to run one program.

After downloading and optionally compiling(install ghc and then run `ghc --make -threaded qsrun.hs`) , place both qsrun.glade and commands.txt into $HOME/.config/qsrun/. For most Linux systems this would be /home/username/.config/qsrun/.  

Each line within commands.txt is treated as an entire command just like entering it in a shell. Only the first 9 lines will be displayed (as there are only 9 buttons), if you want to have more, then make one of the lines as follows: 

`node filename.txt`

Where filename.txt is a file within $HOME/.config/qsrun/.
When clicking on that button, it will replace all of the buttons with the commands from the new file. 
For example, my commands.txt has the last two nodes as:

`node dev.txt`
and
`node system.txt`

Within dev.txt I have commands that are really scripts in my path that launch development environments for various projects. system.txt has commands like `sudo shutdown now`, `sudo pm-suspend` etc.

Custom labels work, just change use a `-` to show a label, e.g: `labelhere - command`.
Simply add the qsrun executable to your path (or run it however you like) and everything should work.
