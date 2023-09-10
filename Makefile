CFLAGS = -std=c++17 -O2 

LDFLAGS = 

DEBUGFLAGS = -g -fsanitize=address,undefined

FILES = main.cpp memory.cpp debug.cpp table.cpp object.cpp value.cpp compiler.cpp scanner.cpp

compile: 
	cd src/ && g++ $(CFLAGS) -o main $(FILES)

