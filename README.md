# Bonobo

Toy implementation of a simple language in order to learn C++, memory management, x86_64 assembly and language design

## TODO

Test suite
    Compiler
        statements
            expressionStatements
            map declaration
            array declaration
        expressions
            parsePrecedence
            
Make sure you free on interpreter error

Aggressively refactor to minimize code 
    if error just kick em out
    create an array type?
    figure out how to remove locals
    figure out if you need everthing through every path etc
    minimize chains of pointers if possible
        vm - objects
    change OP_STRUCT_ARG in compiler

tabs/spaces > {}

setup ci for timed tests

write formal grammar

Builtins:
    len/size

