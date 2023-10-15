# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## TODO LLVM backend

* ToDo refactor
    * fix array stuff
        * should be easy to either check or do the thing you need to do with structure
            * i.e. check which one, get pointer to this etc

* Fix recursive copying of arrays
    * wait why?
        * The flakey test obv

* snd pass map?

* Figure out if variables in compiler done properly 

* Clear distinction between library and internal funcs

* Typecheck internalfuncs params?

* append
    * is just a realloc and then add an item to the end?

* MapExpr
    * assign new key

* Create a stl
    * i.e. read import statements and import correct file
    * String module
        * Split
        * 
    * file I/O
    * read from stdin
    * key in 

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
