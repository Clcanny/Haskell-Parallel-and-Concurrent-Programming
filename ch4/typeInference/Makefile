prepare : clean
	happy Parse.y
	alex Lex.x

clean :
	- rm Parse.hs
	- rm Lex.hs

run : prepare
	ghci parinfer.hs
