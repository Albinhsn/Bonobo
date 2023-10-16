# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## TODO LLVM backend

* Im freeee

* Create a stl
    * Split
        * split(str, str)
            * splitC(ptr, len, ptr len)
                * -> (ptr, len), malloced array of ptrs to new memcpyd strings

* check redeclaration of func, struct or var?

* Fix test for readfile

* GC
    * Second pass to figure out when to GC?
        * you can figure this out when you fix evaluations
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
