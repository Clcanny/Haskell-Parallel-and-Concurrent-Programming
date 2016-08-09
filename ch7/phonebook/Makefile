clean : 
	- rm *.hi
	- rm *.o
	- rm phonebook

compile : phonebook.hs clean
	ghc -O2 phonebook.hs

run : compile
	./phonebook
