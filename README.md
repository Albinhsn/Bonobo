# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## TODO LLVM backend


* Figure out if variables in compiler done properly 
    * Typecheck internalfuncs params?
        * is this done in snd pass?
    * is params even checked for normal func calls?

* Fix recursive copying of arrays
    * for failed test

* append
    * is just a realloc and then add an item to the end?

* MapExpr
    * assign new key

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
