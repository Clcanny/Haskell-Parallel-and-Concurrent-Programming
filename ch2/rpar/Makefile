object = Rpar1
core = 1

clean :
	- rm $(object).hi
	- rm Helper.hi
	- rm $(object).o
	- rm Helper.o
	- rm $(object)

compile : $(object).hs Helper.hs
	ghc -O2 $(object).hs -threaded

run : compile
	./$(object) +RTS -N$(core)
