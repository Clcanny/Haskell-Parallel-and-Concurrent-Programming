object = Rpar4

clean :
	- rm $(object).hi
	- rm Helper.hi
	- rm $(object).o
	- rm Helper.o
	- rm $(object)

compile : clean $(object).hs Helper.hs
	ghc -O2 $(object).hs Helper.hs -threaded -rtsopts

run : compile
	./$(object) +RTS -N1
	./$(object) +RTS -N2
