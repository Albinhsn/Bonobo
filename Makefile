LLVMFLAGS =  -std=c++17  -fno-exceptions -funwind-tables -fno-rtti -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -L/usr/local/lib -lLLVMCore -lLLVMRemarks -lLLVMBitstreamReader -lLLVMBinaryFormat -lLLVMTargetParser -lLLVMSupport -lLLVMDemangle

FILES = main.cpp compiler.cpp scanner.cpp debug.cpp

LLFLILES = ../../src/main.cpp ../../src/compiler.cpp ../../src/scanner.cpp ../../src/debug.cpp 

r:
	cd src/ && lli out.ll < result.txt

c: 
	cd src/ && clang++ -c `llvm-config --cxxflags` $(FILES) && clang++ -o main compiler.o debug.o scanner.o main.o `llvm-config --ldflags --system-libs --libs core` && ./main ../input 

c2: 
	cd llvmex/ && clang++ -c `llvm-config --cxxflags` build.cpp && clang++ -o main build.o `llvm-config --ldflags --system-libs --libs core` && ./main

cc:
		cd ./llvmex && clang++ -S -emit-llvm -o test.ll test.cpp

t:
	cd test/ && clang++ -c `llvm-config --cxxflags` TestSolo.cpp ../src/compiler.cpp ../src/debug.cpp ../src/scanner.cpp && clang++ -o main scanner.o compiler.o debug.o TestSolo.o `llvm-config --ldflags --system-libs --libs core`  && ./main

tt: 
	cd ./llvmex && clang++ -c `llvm-config --cxxflags` test.cpp && clang++ -o main test.o `llvm-config --ldflags --system-libs --libs core` && ./main ../input 

ex: 
	cd ./llvmex/ && clang++ -o main $(LLVMFLAGS) $(file) && ./main && lli out.ll

bt: 
	cmake -S . -B build && cmake --build build && cd build && ctest --output-on-failure -V

b: 
	cmake -S . -B build && cmake --build build


# t: 
# 	cd build && ctest --output-on-failure -V


