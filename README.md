# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design

## ToDo

* Refactor
    * What can't i reason about
    * Things that look ugly probably are, rewrite them in that case

* Figure out if we store the allocation ptr inside the array of ptrs

* Figure out if we stop returning llvm::Value * and create a type for it
    * i.e. Allocation/Variable
        * llvm::AllocaInst/llvm::Value
        * Variable/VarType etc

* A lot of type checking missed

* Improve error handling
    * Throw different error when arr[int] != [[1,2,3]]

* can't do a = [2];

## Bugs/Poorly implemented stuff LLVM backend


## TODO LLVM backend

* MapExpr
    * Create a struct for map, x2 array for keys/values
    * Create functions(?) "native" to maps
    * Index map 

* Declare internal functions to use/support arrays/maps
    * Append
    * Get keys
    * Get values
    * Error if out of bounds

* Assign from index of array, take copy not reference
    * Needs to happen when we allocate atleast

* Write c++ data structures and functions to create a standard library
    * Or just write functions in bonobo


* Mark and Sweep GC

## TODO x86_64 backend

* Cry


## TODO VM 

* Why do you send array size if you don't expand the array anyway

* builtin:
    loop over map/array    
    split on char
