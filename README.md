# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design


## Bugs/Poorly implemented stuf/Poorly implemented stuff  LLVM backend

* Util func for creating cast if possible and then loading

## TODO LLVM backend

* MapExpr

* Break

* Index
    * Array
    * String
    * Map

* Support 2d Arrays
    * Check recursively when compiler gives array_expr it's items to support 2d arrays

* itemType in ArrayDeclaration?
    * Create the array and check its type 
    * Then return it and check it's validity for the stmt
    * if creating == [1,2] it doesn't matter


* Write c functions to create a standard library

## TODO x86_64 backend

* Cry


## TODO VM 

* Why do you send array size if you don't expand the array anyway

* builtin:
    loop over map/array    
    split on char
