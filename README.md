# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## TODO LLVM backend

* Im freeee

* GC
    * Second pass to figure out when to GC?
        * you can figure this out when you fix evaluations
            * i.e create GC statements, and check if(gc) 
        * This is only malloced arrays

* ToDo refactor
    * Just send around item not allocation to it
    * fix array stuff
        * fix index stuff now?
        * should be easy to either check or do the thing you need to do with structure
            * i.e. check which one, get pointer to this etc
    * Clear distinction between library and internal funcs
        * depends on the information required
            * i.e. compile or runtime
    * remove internalfuncs essentially?



## TODO x86_64 backend

* Cry


## TODO VM 

* Why do you send array size if you don't expand the array anyway

* builtin:
    loop over map/array    
    split on char
