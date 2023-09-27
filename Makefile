LLVMFLAGS =  -std=c++17  -fno-exceptions -funwind-tables -fno-rtti -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -L/usr/local/lib -lLLVMCore -lLLVMRemarks -lLLVMBitstreamReader -lLLVMBinaryFormat -lLLVMTargetParser -lLLVMSupport -lLLVMDemangle

FILES = main.cpp compiler.cpp scanner.cpp debug.cpp

LLFLILES = ../../src/main.cpp ../../src/compiler.cpp ../../src/scanner.cpp ../../src/debug.cpp 


c: 
	cd src/ && clang++ -c `llvm-config --cxxflags` $(FILES) && clang++ -o main compiler.o debug.o scanner.o main.o `llvm-config --ldflags --system-libs --libs all` && ./main ../input 


tt: 
	cd ./llvmex && clang++ -o main $(LLVMFLAGS) test.cpp && ./main

ex: 
	cd ./llvmex/ && clang++ -o main $(LLVMFLAGS) $(file) && ./main && lli out.ll

bt: 
	cmake -S . -B build && cmake --build build && cd build && ctest --output-on-failure -V

b: 
	cmake -S . -B build && cmake --build build

t: 
	cd build && ctest --output-on-failure -V


