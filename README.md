# Bonobo

Toy implementation of a simple language in order to learn C/C++, memory management, LLVM, x86_64 assembly and language design


## Bugs LLVM backend

* Check always return something
    * Check correct return type

* if/else/merge things

## TODO LLVM backend

* Do some sort of big test suite anyway with printf, currently breaking stuff to easily

* Need to figure out where we're not type checking etc 

* Some better error handling

* itemType in ArrayDeclaration?
    * How does an array know what type it has?

* Structs

* MapExpr

* Index

* Write c functions to create a standard library

## TODO x86_64 backend

* Cry


## TODO VM 

* Why do you send array size if you don't expand the array anyway

* builtin:
    loop over map/array    
    split on char
