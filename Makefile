LLVMFLAGS =  -std=c++17  -fno-exceptions -funwind-tables -fno-rtti -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -L/usr/local/lib -lLLVMCore -lLLVMRemarks -lLLVMBitstreamReader -lLLVMBinaryFormat -lLLVMTargetParser -lLLVMSupport -lLLVMDemangle

FILES = main.cpp compiler.cpp scanner.cpp debug.cpp

LLFLILES = ../../src/main.cpp ../../src/compiler.cpp ../../src/scanner.cpp ../../src/debug.cpp 

c: 
	cd src/ && clang++ -o main $(LLVMFLAGS) $(FILES) && ./main ../input

f:
	cd src/ && clang++ -o main $(LLVMFLAGS) $(FILES) && ./main ../test/llvm/$(file) && lli out.ll > ../result.txt

llt: 
	cd ./test/llvm/ && clang++ -o main $(LLVMFLAGS) ../../src/compiler.cpp ../../src/scanner.cpp ../../src/debug.cpp TestLLVM.cpp && ./main

ex: 
	cd ./llvmex/ && clang++ -o main $(LLVMFLAGS) $(file) && ./main && lli out.ll

bt: 
	cmake -S . -B build && cmake --build build && cd build && ctest --output-on-failure -V

bt: 
	cmake -S . -B build && cmake --build build && cd build && ctest --output-on-failure -V

t: 
	cd build && ctest --output-on-failure -V
