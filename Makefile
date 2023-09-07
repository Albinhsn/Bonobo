cfd: 
	cd src/ && g++ -O2 -g -fsanitize=address,undefined -o main main.cpp  table.cpp memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input

cd: 
	cd src/ && g++ -O2 -g -fsanitize=address,undefined -o main main.cpp table.cpp memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp
c:
	cd src/ && g++ -O2 -o  main main.cpp  memory.cpp table.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp

cf: 
	cd src/ && g++ -O2 -g -o main main.cpp  table.cpp memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input

p: 
	cd src/ && g++ -O2 -pg -o main main.cpp  table.cpp memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input && gprof ./main > ../profile/log.txt

b:
	cmake -S . -B build && cmake --build build

bt:
	cmake -S . -B build && cmake --build build
	cd build/ && ctest --output-on-failure -V

t:
	cd build/ && ctest  --output-on-failure -V 

i:
	cd src/ && ./main ../input

vg:
	cd src/ &&  g++ -g -o main main.cpp  table.cpp memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && valgrind --tool=callgrind --callgrind-out-file=../profile/callgrind.out.123 ./main ../input

time:
	cd src/ && g++ -O2 -o  main main.cpp  memory.cpp table.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp
	echo "Recursive fib(40):";\
	./src/main ./benchmark/fib; \
	echo "Struct property:";\
	./src/main ./benchmark/struct; \
	echo "equality:";\
	./src/main ./benchmark/equality; \
	echo "struct instantiation:";\
	./src/main ./benchmark/instantiation; \
	echo "string equality:";\
	./src/main ./benchmark/string_equality

cc:
	cd csrc/ && gcc -O2 -o  main main.c  memory.c table.c debug.c value.c vm.c compiler.c scanner.c object.c
