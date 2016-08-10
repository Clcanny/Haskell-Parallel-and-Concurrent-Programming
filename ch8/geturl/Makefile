clean : 
	- rm *.hi
	- rm *.o
	- rm geturls1
	- rm geturls2
	- rm geturls3
	- rm geturls4

compile1 : geturls1.hs clean
	ghc -O2 geturls1.hs

run1 : compile1
	./geturls1

compile2 : geturls2.hs clean
	ghc -O2 geturls2.hs

run2 : compile2
	./geturls2

compile3 : geturls3.hs clean
	ghc -O2 geturls3.hs

run3 : compile3
	./geturls3

compile4 : geturls4.hs clean
	ghc -O2 geturls4.hs

run4 : compile4
	./geturls4
