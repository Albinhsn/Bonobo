# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## TODO LLVM backend

* malloc every array :)
    * create the global thing, malloc the size, then memcpy it

* append
    * append literal

* The road to generic is by creating the neccessary function during compilation in llvm
    * also let's us add funcs whenever we need to not everything all the time
        * also let's us call the functions the same?
    * create a function like createOrCallX with the type

* MapExpr
    * assign new key
        * figure out if key exists
            * otherwise append both keys and values

* Create a stl
    * i.e. read import statements and import correct file
    * String module
        * Split
    * file I/O
    * read from stdin
    * key in / find 

* ToDo refactor
    * fix array stuff
        * should be easy to either check or do the thing you need to do with structure
            * i.e. check which one, get pointer to this etc
    * Clear distinction between library and internal funcs
    * Nice to make functions "generic"

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
