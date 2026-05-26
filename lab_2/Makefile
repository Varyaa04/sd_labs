FC=flang
FFLAGS=-fimplicit-none -fopenmp -march=native
FOPT=-O3

#OMP_ROOT = /usr/lib/llvm-19
#OMP_LIB = -L$(OMP_ROOT)/lib -lomp
OMP_ROOT=/opt/homebrew/opt/libomp
OMP_LIB=-L$(OMP_ROOT)/lib -lomp
OMP_INC = -I$(OMP_ROOT)/include

all: obj bin
	$(FC) $(FFLAGS) $(OMP_INC) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) $(FOPT) $(OMP_INC) -c src/text_io.f90 -J obj/ -o obj/text_io.o
	$(FC) $(FFLAGS) $(FOPT) $(OMP_INC) -c src/text_process.f90 -J obj/ -o obj/text_process.o
	$(FC) $(FFLAGS) $(FOPT) $(OMP_INC) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app obj/environment.o obj/text_io.o obj/text_process.o obj/main.o $(OMP_LIB)

obj:
	mkdir -p obj

bin:
	mkdir -p bin

clean:
	rm -rf obj/* bin/* result.txt

run:
	cd ./bin && ./app
	cat bin/result.txt
    LANG=ru_RU.UTF-8 cat bin/result.txt