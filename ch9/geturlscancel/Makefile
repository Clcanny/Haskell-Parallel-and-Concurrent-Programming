clean :
	- rm *.hi
	- rm *.o
	- rm geturlscancel

compile : clean geturlscancel.hs
	ghc -O2 geturlscancel.hs

run : compile
	./geturlscancel
