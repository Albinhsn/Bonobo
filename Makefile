cd:
	cd src/ && g++ -g -fsanitize=address,undefined -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp
crd:
	cd src/ && g++  -g -fsanitize=address,undefined -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main

cfd: 
	cd src/ && g++ -g -fsanitize=address,undefined -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input

c:
	cd src/ && g++ -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp
cr:
	cd src/ && g++  -g -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main

cf: 
	cd src/ && g++ -g -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input

p: 
	cd src/ && g++ -pg -o main main.cpp  memory.cpp debug.cpp value.cpp vm.cpp compiler.cpp scanner.cpp object.cpp && ./main ../input && gprof ./main > log.txt


b:
	cmake -S . -B build && cmake --build build

bt:
	cmake -S . -B build && cmake --build build
	cd build/ && ctest --output-on-failure -V

t:
	cd build/ && ctest  --output-on-failure -V 

i:
	cd src/ && ./main ../input

br:
	cmake -S . -B build && cmake --build build && cd build/ && ./main ../input 

r:
	cmake --build build && cd build/ && ./main $(file)

