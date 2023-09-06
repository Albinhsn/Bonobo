cfd: 
	cd src/ && g++ -g -fsanitize=address,undefined -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input

c:
	cd src/ && g++ -O2 -o  main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp

cf: 
	cd src/ && g++ -g -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input

p: 
	cd src/ && g++ -O3 -pg -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input && gprof ./main > ../profile/log.txt

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
	cd src/ &&  g++ -g  -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && valgrind --tool=callgrind --callgrind-out-file=../profile/callgrind.out.123 ./main ../input
