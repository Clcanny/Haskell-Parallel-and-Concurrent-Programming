object = server4

clean :
	- rm *.hi
	- rm *.o
	- rm $(object)

make : clean $(object).hs
	ghc -O2 $(object).hs

run : make
	./$(object)
