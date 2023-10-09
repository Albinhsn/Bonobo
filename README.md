# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## ToDo

* Assign to index

## Bugs/Poorly implemented stuff LLVM backend

* Prolly need to copy and not send index by reference

* A lot of type checking missed

* Improve error handling
    * Store smth about line, errorAt func

* Actually free memory?

## TODO LLVM backend

* Declare internal functions to use/support arrays/maps

* MapExpr
    * Create a struct for map, x2 array for keys/values
    * Create function "native" to maps

* Index map 

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
