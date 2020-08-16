# ZipSH

A shell for navigating zip files. Can execute some custom zip commands as well as *any* regular shell commands.

Custom commands include:
- ls
- pwd
- cd
- cat

Anything else is going to be executed as a regular shell command. Arguments prefixed with `§` will be extracted to a temporary directory and copied back when the command is done. 

Example: You want to edit a file called `myfile.hs` inside some zip file. You can just use vim *almost* as you would when editing any regular file:
```
prophet@[myarchive.zip]:~$ ls
myfile.hs
prophet@[myarchive.zip]:~$ cat myfile.hs
main = putStrLn "Hello, World!"
prophet@[myarchive.zip]:~$ vim §myfile.hs
```

**NOTE:** Empty Directories inside archives are not supported, since Haskell's `zip` library, which this project uses, does not support them either 
