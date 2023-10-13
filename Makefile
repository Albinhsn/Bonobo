FILES = main.cpp compiler.cpp scanner.cpp debug.cpp llvm.cpp library.cpp

c: 
	cd src/ && clang++ -c `llvm-config --cxxflags` $(FILES) && clang++ -o main compiler.o library.o debug.o scanner.o main.o llvm.o `llvm-config --ldflags --system-libs --libs core` && ./main ../input 

t:
	cd test/ && clang++ -c `llvm-config --cxxflags` TestSolo.cpp ../src/library.cpp ../src/compiler.cpp ../src/debug.cpp ../src/scanner.cpp ../src/llvm.cpp && clang++ -o main llvm.o scanner.o compiler.o library.o debug.o TestSolo.o `llvm-config --ldflags --system-libs --libs core`  && ./main

bt: 
	cmake -S . -B build && cmake --build build && cd build && ctest --output-on-failure -V

b: 
	cmake -S . -B build && cmake --build build
