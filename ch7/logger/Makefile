clean :
	- rm *.hi
	- rm *.o
	- rm logger

compile : logger.hs clean
	ghc -O2 logger.hs

run : compile
	./logger
