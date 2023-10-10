# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## ToDo

* Split compileIndex into two funcs, one to load and one to store

* Assign to index
    * Prolly need to copy and not send index by reference

* Actually free memory?

* errorAt function

* Assign to struct?

## Bugs/Poorly implemented stuff LLVM backend

* A lot of type checking missed

* Improve error handling
    * Store smth about line, errorAt func
    * Throw different error when arr[int] != [[1,2,3]]

## TODO LLVM backend

* MapExpr
    * Create a struct for map, x2 array for keys/values
    * Create functions(?) "native" to maps
    * Index map 

* Declare internal functions to use/support arrays/maps

* Write c++ data structures and functions to create a standard library
    * Or just write functions in bonobo

* Be able to create and import functions from other module?

## TODO x86_64 backend

* Cry


## TODO VM 

* Why do you send array size if you don't expand the array anyway

* builtin:
    loop over map/array    
    split on char
