qsrun
=====

-----

####Version 0.1




lightweight mouse driven overlay for launching anything.

qsrun for now doesn't have a gui, but you can interface with it by using preset modes such as 
```
qsrun add filename command
qsrun remove filename commandnumber
qsrun view filename
qsrun run filename commandnumber
```

Add and remove work as you would expect, remove takes a filename and a command number(which you can retrieve by doing `qsrun view filename`)  and removes it from the file.
Run takes a filename and command number and runs the appropriate command in the given file. 

Just run `qsrun` without any arguments to enter interactive mode.
