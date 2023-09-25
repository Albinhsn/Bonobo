# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design


## Bugs/Poorly implemented stuf/Poorly implemented stuff  LLVM backend

* Just check for bugs with comments

* Struct lookup in compiler "foo.bar";

* Support 2d Arrays
    * Check recursively when compiler gives array_expr it's items to support 2d arrays

* Check always return something or error if you don't?

* Scoping rules with variables

* Some better error handling
    * Need to figure out where we're not type checking etc 

* Don't change \n prior to scanning, find better place to do it

* itemType in ArrayDeclaration?
    * How does an array know what type it has?
        * It doesn't after you calculated what it contains;
            * Do so and then check its validity?

* Can't redeclare var/func, check both


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
