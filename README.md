# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design


## ToDo

* Improve readability
    * Look for places to create utility funcs
        * Create one/two liners for things 
            * like lookup on first ptr etc
            * get size of struct
            * CreateString
            * lookover concat string
* Every allocation can be stored as a variable?

* String/Struct array
    * Index them

## Bugs/Poorly implemented stuff LLVM backend

* Break from within if


## TODO LLVM backend

* MapExpr
    * Index map 
    
* Support 2d Arrays
    * Check recursively when compiler gives array_expr it's items to support 2d arrays

* itemType in ArrayDeclaration?
    * Create the array and check its type 
    * Then return it and check it's validity for the stmt
    * if creating == [1,2] it doesn't matter

* Write c functions to create a standard library
    * Or just write functions in bonobo

* i++
    * Inc/Dec expr

## TODO x86_64 backend

* Cry


## TODO VM 

* Why do you send array size if you don't expand the array anyway

* builtin:
    loop over map/array    
    split on char
