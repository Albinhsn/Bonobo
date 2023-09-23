LLVMFLAGS =  -std=c++17  -fno-exceptions -funwind-tables -fno-rtti -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -L/home/arla/.local/llvm-project/build/lib -lLLVMCore -lLLVMRemarks -lLLVMBitstreamReader -lLLVMBinaryFormat -lLLVMTargetParser -lLLVMSupport -lLLVMDemangle

FILES = main.cpp compiler.cpp scanner.cpp debug.cpp

c: 
	cd src/ && clang++ -o main $(LLVMFLAGS) $(FILES) && ./main ../input

r:
	cd src/ && llc -filetype=obj -o out.o out.ll && clang -no-pie -o out out.o && ./out

s:
	cd src/ && clang++ -o main $(LLVMFLAGS) $(FILES) && ./main ../suite


ex: 
	cd ./llvmex/ && clang++ -o main $(LLVMFLAGS) $(file) && ./main && lli out.ll

bt: 
	cmake -S . -B build && cmake --build build && cd build && ctest --output-on-failure -V
t: 
	cd build && ctest --output-on-failure -V
