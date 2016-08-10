clean : 
	- rm *.hi
	- rm *.o
	- rm geturls1
	- rm geturls2

compile1 : geturls1.hs clean
	ghc -O2 geturls1.hs

run1 : compile1
	./geturls1

compile2 : geturls2.hs clean
	ghc -O2 geturls2.hs

run2 : compile2
	./geturls2
