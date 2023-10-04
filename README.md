# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design


## ToDo

* Something is just wrong with struct array 

* Struct array
    * Index them

* 3D array and more

## Bugs/Poorly implemented stuff LLVM backend

* Break from within if
    * Store a ptr to the basic block you merge with
        * if none exist you can't break anyway
            * need to be a struct cos you have multiple loops

* a[][] doesn't work

## TODO LLVM backend

* MapExpr
    * Index map 
    
* Typecheck 2d Arrays
    * Check recursively when compiler gives array_expr it's items to support 2d arrays

* itemType in ArrayDeclaration?
    * Create the array and check its type 
    * Then return it and check it's validity for the stmt
    * if creating == [1,2] it doesn't matter
    * Maybe just store the same way you do with string, ala you store allocations and their type

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
