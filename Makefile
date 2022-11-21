compile:
	ghc Main.hs Tableaux.hs -o Main

run:
	ghc Tableaux.hs Main.hs -o Main && ./Main
