# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design


## Bugs/Poorly implemented stuf/Poorly implemented stuff  LLVM backend

* Scoping rules with variables
    * Check whether creating smth in a if remains

* Support 2d Arrays
    * Check recursively when compiler gives array_expr it's items to support 2d arrays

* itemType in ArrayDeclaration?
    * Create the array and check its type 
    * Then return it and check it's validity for the stmt
    * if creating == [1,2] it doesn't matter

* Different binary expr for fp

* Comparison op for anything other then int

* Don't change \n prior to scanning, find better place to do it


## TODO LLVM backend

* Index

* Structs
    * Declare
    * Instance
    * Property
    * Return from func
    * Array of:
        struct

* MapExpr

* Break

* Write c functions to create a standard library

## TODO x86_64 backend

* Cry


## TODO VM 

* Why do you send array size if you don't expand the array anyway

* builtin:
    loop over map/array    
    split on char
