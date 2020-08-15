#TODO
* Open Subdirs when invoking zipSH
    - IE: `zipSH myarchive.zip/mydir1/mydir2`
* Abort if zip file name has a file extension that is not `*.zip` 
    or is a directory
* History
* Auto Completion
    - files in archive
    - auto completion for default commands
* Config File
    - prompt
    - history length
    - color configuration 
* Generic abstraction for custom commands
    - flags
* `ls`
    - Error message if directory does not exist
    - default behaviour for files
    - Hide files starting with `.` unless invoked with `-a` or `-A`
* usage
* `mkdir`
    - Might need to create temporary hidden file
    - `-p` flag 
* `touch`
* `cp`
    - `cp` from archive to same archive
    - `cp` from archive to filesystem
    - `cp` from filesystem to archive
    - `cp` from archive to different archive
    - `-r`
* `mv`
    - `mv` from archive to same archive
    - `mv` from archive to filesystem
    - `mv` from filesystem to archive
    - `mv` from archive to different archive
* `rm`
    - `-r`
* `compress`
    - syntax: `compress <filename> [None | Store | Deflate | BZip2]`
* shell integration for custom commands
* auto-extraction (`§`) for directories