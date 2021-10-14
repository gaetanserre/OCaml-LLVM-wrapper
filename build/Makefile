default: all

MAIN := main

all:
	cd ../src && dune build $(MAIN).exe && cp -f _build/default/$(MAIN).exe ../build/$(MAIN) && cd ../build

bytecode:
	@for f in $(shell ls *.cpp); do g++ $${f} -S -std=c++2a -emit-llvm; done

clean:
	cd ../src && dune clean && cd ../build && rm -f *.ll && rm -f *.bc && rm -f *.s && rm -f main