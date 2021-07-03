default: all

MAIN := llvm_program

all:
	cd ../src && dune build $(MAIN).exe && cp -f _build/default/$(MAIN).exe ../build/$(MAIN) && cd ../build

bytecode:
	@for f in $(shell ls *.cpp); do g++ $${f} -c -std=c++2a -emit-llvm; done

clean:
	cd ../src && dune clean && cd ../build && rm -f *.ll && rm -f *.bc && rm -f *.s && rm -f llvm_program