clean :
	- rm *.hi
	- rm *.o
	- rm fork
	- rm remainder
	- rm wrongRemainder

compileFork : fork.hs
	ghc -O2 fork.hs

runFork : compileFork
	./fork

compileRemainder : remainder.hs
	ghc -O2 remainder.hs

runRem : compileRemainder
	./remainder

compileWrongRem : wrongRemainder.hs
	ghc -O2 wrongRemainder.hs

runWrongRem : compileWrongRem
	./wrongRemainder
