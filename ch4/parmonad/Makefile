clean :
	- rm parmonad.hi
	- rm parmonad.o
	- rm parmonad

compile : clean parmonad.hs
	ghc -O2 parmonad.hs -threaded -rtsopts

run : compile
	./parmonad
