# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design


## Bugs/Poorly implemented stuf/Poorly implemented stuff  LLVM backend


* Support 2d Arrays
    * Check recursively when compiler gives array_expr it's items to support 2d arrays

* itemType in ArrayDeclaration?
    * Create the array and check its type 
    * Then return it and check it's validity for the stmt
    * if creating == [1,2] it doesn't matter

* Scoping rules with variables
    * Check whether creating smth in a if remains

* Don't change \n prior to scanning, find better place to do it

* Update tests to include
    * BinaryExpr with double
    * ComparisonExpr with double


## TODO LLVM backend

* Index

* Structs
    * Declare
    * Instance
    * Property
    * Return from func
    * Array of:
        struct
* Create a String and an Array type

* String concat

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
