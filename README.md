# Bonobo

Toy implementation of a simple language in order to learn C++, memory management, x86_64 assembly and language design

## TODO

Test suite
    Compiler
        statements
            loops 
            functions
            struct declarations
            var declarations
            if 
            print
            return
            expressionStatements
            map declaration
            array declaration
        expressions
            parsePrecedence
            

Aggressively refactor to minimize code 
    identifierConstant
    writeChunks
    remove emitReturn
    change OP_STRUCT_ARG in compiler
    AS_OBJ, casting value <-> obj
    change vector of pointers to struct of c array and length
        vm - objects
    look over ci notes/challenges for optimizations
    don't use push_back
    minimize chains of pointers if possible
    figure out if you need everthing through every path etc

profile -> Optimize some stuff :)

tabs/spaces > {}

setup ci for timed tests

write formal grammar

Builtins:
    len/size

