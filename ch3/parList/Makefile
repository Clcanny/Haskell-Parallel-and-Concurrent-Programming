object = sudoku5
core = 2

clean :
	- rm $(object).hi
	- rm Sudoku.hi
	- rm ParList.hi
	- rm $(object).o
	- rm Sudoku.o
	- rm ParList.o
	- rm $(object)
	- rm $(object).eventlog

compile : clean $(object).hs Sudoku.hs
	ghc -O2 $(object).hs -rtsopts -threaded -eventlog

run : compile
	./$(object) sudoku17.1000.txt +RTS -s -N$(core) -l

analyse : run
	threadscope $(object).eventlog
