FILES = main.cpp compiler.cpp scanner.cpp debug.cpp llvm.cpp library.cpp stmt.cpp expr.cpp

c: 
	cd src/ && clang++ -c `llvm-config --cxxflags` $(FILES) && clang++ -o main compiler.o library.o debug.o scanner.o main.o stmt.o expr.o llvm.o `llvm-config --ldflags --system-libs --libs core` && ./main ../input 

c1: 
	cd src/ && clang++ -c -static-libsan -g -fsanitize=address `llvm-config --cxxflags` $(FILES)

c2:
	cd src/ && clang++ -static-libsan -g -fsanitize=address -o main compiler.o library.o debug.o scanner.o main.o stmt.o expr.o llvm.o `llvm-config --ldflags --system-libs --libs core` && ./main ../input 

t:
	cd test/ && clang++ -c `llvm-config --cxxflags` TestSolo.cpp ../src/library.cpp ../src/compiler.cpp ../src/debug.cpp ../src/scanner.cpp ../src/llvm.cpp ../src/stmt.cpp ../src/expr.cpp && clang++ -o main llvm.o scanner.o compiler.o library.o debug.o TestSolo.o expr.o stmt.o `llvm-config --ldflags --system-libs --libs core`  && ./main

bt: 
	cmake -S . -B build && cmake --build build && cd build && ctest --output-on-failure -V

b: 
	cmake -S . -B build && cmake --build build
