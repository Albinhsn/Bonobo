# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## TODO LLVM backend

* Error if array out of bounds

* MapExpr
    * Create functions(?) "native" to maps
        * get keys
            * .keys
        * get values
            * .values
        * assign new key
        * .size()

* ArrayExpr
    * is just a realloc and then add an item to the end?
    * .size()

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
