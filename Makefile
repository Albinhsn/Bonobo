FILES = main.cpp compiler.cpp scanner.cpp debug.cpp

c: 
	cd src/ && clang++ -c `llvm-config --cxxflags` $(FILES) && clang++ -o main compiler.o debug.o scanner.o main.o `llvm-config --ldflags --system-libs --libs core` && ./main ../input 

t:
	cd test/ && clang++ -c `llvm-config --cxxflags` TestSolo.cpp ../src/compiler.cpp ../src/debug.cpp ../src/scanner.cpp && clang++ -o main scanner.o compiler.o debug.o TestSolo.o `llvm-config --ldflags --system-libs --libs core`  && ./main

bt: 
	cmake -S . -B build && cmake --build build && cd build && ctest --output-on-failure -V

b: 
	cmake -S . -B build && cmake --build build
