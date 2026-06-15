FC = flang
FFLAGS = -fimplicit-none -fopenmp -march=native
FOPT = -O3

OMP_ROOT = /usr/lib/llvm-19
OMP_LIB = -L$(OMP_ROOT)/lib -lomp
#OMP_ROOT = /opt/homebrew/opt/libomp
#OMP_LIB = -L$(OMP_ROOT)/lib -lomp
OMP_INC = -I$(OMP_ROOT)/include

all: obj bin
	$(FC) $(FFLAGS) $(OMP_INC) -c src/environment.f90 -J obj/ -o obj/environment.o
#	$(FC) $(FFLAGS) $(FOPT) $(OMP_INC) -c src/list_io.f90 -J obj/ -o obj/list_io.o
	$(FC) $(FFLAGS) $(FOPT) $(OMP_INC) -c src/circular_list.f90 -J obj/ -o obj/circular_list.o
	$(FC) $(FFLAGS) $(FOPT) $(OMP_INC) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app obj/environment.o obj/circular_list.o obj/main.o $(OMP_LIB)

obj:
	mkdir -p obj

bin:
	mkdir -p bin

clean:
	rm -rf obj/* bin/* output.txt

run:
	ulimit -s unlimited && cd ./bin && ./app
	cat bin/output.txt
