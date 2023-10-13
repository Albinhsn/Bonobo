# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## Things i'd like to solve in a snd pass
* Is it possible to type check everything before we start compiling it
    * if true is good for when we create the next backend

* Type check items
    * map and array

* Type check variable 
    * return from func etc

## TODO LLVM backend

* Error if out of bounds
    * Array indexing that is

* MapExpr
    * Create functions(?) "native" to maps
        * get keys
            * .keys
        * get values
            * .values
        * assign new key

* Declare internal functions to use/support arrays/maps
    * Append
        * is just a realloc and then add an item to the end?

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
