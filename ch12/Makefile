clean :
	- rm *.hi
	- rm *.o
	- rm server

make : clean server.hs
	ghc -O2 server.hs

run : make
	./server
