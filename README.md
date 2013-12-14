qsrun
=====

-----

####Version 0.2




Lightweight mouse driven overlay for launching anything.

qsrun is a lightweight launcher that uses a grid of 9 buttons to launch scripts or programs. 
When run without any command line arguments, the GUI is used to control the program.
If you want to use it in a shell, see below.

```
qsrun add filename command
qsrun remove filename commandnumber
qsrun view filename
qsrun run filename commandnumber
```

Add and remove work as you would expect, remove takes a filename and a command number(which you can retrieve by doing `qsrun view filename`)  and removes it from the file.
Run takes a filename and command number and runs the appropriate command in the given file. 

####Setup


After downloading and optionally compiling(install ghc and then run `ghc --make -threaded qsrun.hs`), place both qsrun.glade and commands.txt into $HOME/.config/qsrun/. For most Linux systems this would be /home/username/.config/qsrun/. You don't have to modify the commands.txt file using qsrun, you can use vi(m)/nano etc. Each line is treated as an entire command just like entering it in a shell. The first 9 lines are displayed on the 9 buttons in GUI mode, so any commands after that can only be accessed (for now) by using `qsrun run commands.txt 10`. 

Simply add the qsrun executable to your path (or run it however you like) and everything <i>should</i>work.
