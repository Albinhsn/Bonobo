# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## TODO LLVM backend

* Cleanup map func stuff

* Just like refactor a alot xD
    * Figure out what operations you need to do often and create functions for them
        * This shouldn't really be hard since you do it in IR already

* Refactor index holy shit

* Typecheck index
* Error if out of bounds
    * Array indexing that is
* Check if it's even an integer?
* Compile time type check and runtime out of bounds check

* MapExpr
    * Create functions(?) "native" to maps
        * get keys
        * get values
        * assign new key

* Think about second pass over ast
    * Typecheck stuff there is easier?

* Array of maps

* Declare internal functions to use/support arrays/maps
    * Append

* A lot of type checking missed

* Improve error handling
    * Throw different error when arr[int] != [[1,2,3]]

* Fix recursive copying of arrays

* can't do a = [2];

* Create a stl
    * i.e. read import statements and import correct file
    * String module
        * Split
        * 
    * file I/O
    * read from stdin


* GC
    * Second pass to figure out when to GC?
        * This is only malloced arrays


## TODO x86_64 backend

* Cry


## TODO VM 

* Why do you send array size if you don't expand the array anyway

* builtin:
    loop over map/array    
    split on char
