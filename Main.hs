import Tableaux

main :: IO()
main = do 
  putStr "Digite a fórmula desejada :\n "
  formulaString <- getLine
  executaTableuax (transformaStringVariavel formulaString 0) 
  putStr ""
  

