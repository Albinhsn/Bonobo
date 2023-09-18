CFLAGS = -std=c++17 -O2 

LDFLAGS = 

LLVMFLAGS =  -std=c++17  -fno-exceptions -funwind-tables -fno-rtti -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -L/home/arla/.local/llvm-project/build/lib -lLLVMCore -lLLVMRemarks -lLLVMBitstreamReader -lLLVMBinaryFormat -lLLVMTargetParser -lLLVMSupport -lLLVMDemangle

DEBUGFLAGS = -g -fsanitize=address,undefined

LLVMFILES = -I/home/

FILES = main.cpp compiler.cpp

c: 
	cd src/ && clang++ -o main $(LLVMFLAGS)  main.cpp  && ./main && lli out.ll

ex: 
	cd ./llvmex/ && clang++ -o main $(LLVMFLAGS) $(file) && ./main && lli out.ll
