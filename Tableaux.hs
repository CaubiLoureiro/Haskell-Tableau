module Tableaux where

data Formula = Var String
              | And Formula Formula 
              | Or Formula Formula 
              | Implication Formula Formula
              | Not Formula


data Noinfo = Noinfo Formula Bool

data Arvore = Null | No Noinfo Arvore Arvore | RamoFechado


-------------------------------------- Funções para aplicar as regras do método-----------------------------------------


criaArvore :: Formula -> Arvore
criaArvore formula = percorreArvore(aplicaRegra (No (Noinfo (Not formula) False) Null Null))



aplicaRegra :: Arvore -> Arvore

aplicaRegra (No (Noinfo (Var var) False) fe fd) = aplicaRegra (No (Noinfo (Var var) True) fe fd)
aplicaRegra (No (Noinfo (Not (Var var)) False) fe fd) = aplicaRegra (No (Noinfo (Not (Var var)) True) fe fd)

aplicaRegra Null = Null

aplicaRegra (No (Noinfo formula True) fe fd) = No (Noinfo formula True) (aplicaRegra fe) (aplicaRegra fd)

-- v: a & b ==> v: a ; v : b
aplicaRegra (No (Noinfo (And formula1 formula2) False) fe fd) =
    aplicaRegra ( insereNo arvore2 (No (Noinfo formula2 False) Null Null) Null ) where
        arvore2 = ( insereNo arvore1 (No (Noinfo formula1 False) Null Null) Null ) where
            arvore1 = No (Noinfo (And formula1 formula2) True) fe fd

-- f: a & b ==> f: a / f : b
aplicaRegra (No (Noinfo (Not (And formula1 formula2)) False) fe fd) =
    aplicaRegra( insereNo arvore (No (Noinfo (Not formula1) False) Null Null) (No (Noinfo (Not formula2) False) Null Null) ) where
        arvore = No (Noinfo (Not (And formula1 formula2)) True) fe fd

-- v: a -> b ==> f: a / v: b
aplicaRegra (No (Noinfo (Implication formula1 formula2) False) fe fd) =
    aplicaRegra( insereNo arvore (No (Noinfo (Not formula1) False) Null Null) (No (Noinfo formula2 False) Null Null) )where
        arvore = No (Noinfo (Implication formula1 formula2) True) fe fd

-- f: a -> b ==> v: a ; f: b
aplicaRegra (No (Noinfo (Not (Implication formula1 formula2)) False) fe fd) =
    aplicaRegra(insereNo arvore2 (No (Noinfo (Not formula2) False) Null Null) Null ) where
        arvore2 = (insereNo arvore1 (No (Noinfo formula1 False) Null Null) Null) where
            arvore1 = No (Noinfo (Not (Implication formula1 formula2)) True) fe fd

-- v: a | b ==> v: a / v: b
aplicaRegra (No (Noinfo (Or formula1 formula2) False) fe fd) =
    aplicaRegra( insereNo arvore (No (Noinfo formula1 False) Null Null) (No (Noinfo formula2 False) Null Null) ) where
        arvore = No (Noinfo (Or formula1 formula2) True) fe fd

-- f: a | b ==> f: a ; f: b
aplicaRegra (No (Noinfo (Not (Or formula1 formula2)) False) fe fd) =
    aplicaRegra ( insereNo arvore2 (No (Noinfo (Not formula2) False) Null Null) Null ) where
        arvore2 = ( insereNo arvore1 (No (Noinfo (Not formula1) False) Null Null) Null ) where
            arvore1 = No (Noinfo (Not (Or formula1 formula2)) True) fe fd

-- f: ~a ==> v: a
aplicaRegra (No (Noinfo (Not (Not formula1)) False) fe fd) =
    aplicaRegra (insereNo arvore (No (Noinfo formula1 False) Null Null) Null ) where
        arvore = No (Noinfo (Not (Not formula1)) True) fe fd

----------------------------------------------------------------------------------------------------------



------------------------------------ Funçoes para inserir um nó na arvore ------------------------------------


insereNo :: Arvore -> Arvore -> Arvore -> Arvore
insereNo (No nodeinfo Null Null) a1 a2 = No nodeinfo a1 a2
insereNo Null a1 a2 = Null
insereNo (No nodeinfo e d) a1 a2 = No nodeinfo (insereNo e a1 a2) (insereNo d a1 a2)

---------------------------------------------------------------------------------------------------------



-------------------------------------------- função para mostrar-------------------------------------

converteFormula :: Formula -> String
converteFormula (Var variavel) = variavel

converteFormula (Not (formula1)) = "( Not " ++(converteFormula ( formula1)) ++" )"

converteFormula (And formula1 formula2) = "( " ++(converteFormula (formula1) ++ " and " ++ converteFormula (formula2)) ++" )"

converteFormula (Or formula1 formula2) = "( " ++(converteFormula (formula1) ++ " or " ++ converteFormula (formula2)) ++" )"

converteFormula (Implication formula1 formula2) = "( " ++(converteFormula (formula1) ++ " -> " ++ converteFormula (formula2)) ++" )"



printNivel :: Int -> IO()
printNivel 0 = putStr ""
printNivel x = do
  putStr "-"
  printNivel (x - 1)


mostraArvore :: Arvore -> Int -> String -> IO()
mostraArvore Null _ _ = putStr ""

mostraArvore (No noinfo fe fd) inteiro string = do
  putStrLn ""
  printNivel inteiro
  putStr string
  printArvore (No noinfo fe fd)
  putStrLn ""
  mostraArvore fe (inteiro+1) "E "
  mostraArvore fd (inteiro+1) "D "

mostraArvore RamoFechado inteiro string = do
  putStrLn ""
  printNivel inteiro
  putStr string
  putStr "RamoFechado"
  putStrLn ""
  



 
printArvore :: Arvore -> IO()
printArvore (No (Noinfo formula verificado) fe fd) = putStr(converteFormula formula)

------------------------------------------------------------------------------------------------------------




---------------------------------------------Verifica contradicao ---------------------------------------------


verificaContradicao :: Formula -> Formula ->  Bool 
verificaContradicao (Var variavel1) (Not (Var variavel2)) = variavel1 == variavel2 
verificaContradicao (Not (Var variavel1)) (Var variavel2) = variavel1 == variavel2 
verificaContradicao _ _ = False


verificaAtomico :: Formula -> Bool
verificaAtomico (Var variavel) = True
verificaAtomico (Not (Var variavel)) = True
verificaAtomico _ = False

percorreArvore :: Arvore -> Arvore
percorreArvore Null = Null
percorreArvore RamoFechado = RamoFechado
percorreArvore (No (Noinfo formula verificado) fe fd) 
    | verificaAtomico formula = No (Noinfo formula verificado) (percorreArvore (busca fe formula)) (percorreArvore(busca fd formula))
    |otherwise = No (Noinfo formula verificado) (percorreArvore fe) (percorreArvore fd)



busca :: Arvore -> Formula -> Arvore
busca Null _ = Null
busca RamoFechado _ = RamoFechado
busca (No (Noinfo formula verificado) fe fd) formulaPai
    | verificaAtomico formula && verificaContradicao formula formulaPai = marcaRamo(No (Noinfo formula verificado) fe fd)
    | otherwise =  No (Noinfo formula verificado) (busca fe formulaPai) (busca fd formulaPai)



marcaRamo :: Arvore -> Arvore
marcaRamo Null = Null
marcaRamo RamoFechado = RamoFechado
marcaRamo (No noinfo Null Null) = (No noinfo RamoFechado RamoFechado)
marcaRamo (No (Noinfo formula verificado) fe fd) = No (Noinfo formula verificado) (marcaRamo fe) (marcaRamo fd)

-------------------------------------------------- Verificar tautologia --------------------------------------------------
verificaTautologia :: Arvore -> Bool
verificaTautologia arvore | verificaQuantidadeNos arvore == verificaQuantidadeFechado arvore = True
                          | otherwise =  False



verificaQuantidadeFechado :: Arvore -> Int
verificaQuantidadeFechado (No noinfo Null Null) = 0
verificaQuantidadeFechado (No noinfo RamoFechado RamoFechado) = 1
verificaQuantidadeFechado (No noinfo fe fd) = 0 + (verificaQuantidadeFechado fe) + (verificaQuantidadeFechado fd)
verificaQuantidadeFechado Null = 0


verificaQuantidadeNos :: Arvore -> Int
verificaQuantidadeNos (No noinfo Null Null) = 1
verificaQuantidadeNos (No noinfo RamoFechado RamoFechado) = 1
verificaQuantidadeNos (No noinfo fe fd) = 0 + (verificaQuantidadeNos fe) + (verificaQuantidadeNos fd)
verificaQuantidadeNos Null = 0

-------------------------------------------------- Contra-exemplo --------------------------------------------------

verificaRamoAberto :: Arvore -> Bool
verificaRamoAberto arvore | verificaQuantidadeNos arvore == verificaQuantidadeFechado arvore = False
                          | otherwise =  True


encontraContraExemplo :: Arvore -> [Formula] -> [Formula]

encontraContraExemplo (No (Noinfo formula verificado) Null Null) listaFormulas 
    | verificaAtomico formula = listaFormulas ++ [formula]
    | otherwise = listaFormulas

encontraContraExemplo (No (Noinfo formula verificado) fe fd) listaFormulas
    | verificaAtomico formula && verificaRamoAberto fe = encontraContraExemplo fe (listaFormulas ++ [formula])
    |verificaAtomico formula && verificaRamoAberto fd = encontraContraExemplo fd (listaFormulas ++ [formula])
    | verificaRamoAberto fe = encontraContraExemplo fe listaFormulas
    | verificaRamoAberto fd = encontraContraExemplo fd listaFormulas


imprimeFormula :: Formula -> IO()
imprimeFormula (Var variavel) = putStrLn (variavel ++ ": V")
imprimeFormula (Not (Var variavel)) = putStrLn (variavel ++ ": F")



mostraContraExemplo :: [Formula]  -> IO()
mostraContraExemplo [] = putStrLn ""
mostraContraExemplo (cabeca:calda) = do
  imprimeFormula cabeca
  mostraContraExemplo calda

imprimeContraExemplo :: Arvore -> IO()
imprimeContraExemplo arvore = do
  mostraContraExemplo (encontraContraExemplo arvore [])


--------------------------------------------------------- Pega entrada ---------------------------------------------------------

procuraVirgulaPrincipal :: String -> Int -> Int -> Int -> Int
procuraVirgulaPrincipal [] _ _ virgula = virgula


procuraVirgulaPrincipal string indice parenteses virgula 
    | (string !! indice) == '(' = procuraVirgulaPrincipal string (indice + 1) (parenteses + 1) virgula

    | (string !! indice) == ')' = if parenteses /= 1 
                                  then procuraVirgulaPrincipal string (indice + 1) (parenteses - 1) virgula 
                                  else virgula

    | (string !! indice) == ',' = if parenteses /= 1 
                                  then procuraVirgulaPrincipal string (indice + 1) parenteses virgula 
                                  else procuraVirgulaPrincipal string (indice + 1) parenteses indice

    | otherwise = procuraVirgulaPrincipal string (indice + 1) parenteses virgula




transformaStringVariavel string indice 
    | (string !! indice) == '&' = And (transformaStringVariavel string (indice + 2)) (transformaStringVariavel string (procuraVirgulaPrincipal string (indice + 1) 0 0))
    | (string !! indice) == 'v' = Or (transformaStringVariavel string (indice + 2)) (transformaStringVariavel string (procuraVirgulaPrincipal string (indice + 1) 0 0))
    | (string !! indice) == '>' = Implication (transformaStringVariavel string (indice + 2)) (transformaStringVariavel string (procuraVirgulaPrincipal string (indice + 1) 0 0))
    | (string !! indice) == '-' = Not (transformaStringVariavel string (indice + 1))
    | (string !! indice) == ',' = transformaStringVariavel string (indice + 1)
    | otherwise = Var [string !! indice]
  





-------------------------------------------------------------------------------------------------------------------------------------------------------



executaTableuax :: Formula -> IO()
executaTableuax formula = do
    putStr "##############################\n"
    putStr "Legenda: E indica ramo à esquerda e D indica ramo à direita\n\n"
    putStrLn "Árvore:"
    mostraArvore a 0 "R "
    putStr "\n"
    putStr "##############################\n\n"
    if (verificaTautologia a)
    then 
        putStr "A fórmula é tautologia.\n"
    else do
        putStr "A fórmula é falsificável.\n"
        putStr "\n"
        putStr "Contra exemplo:\n"
        imprimeContraExemplo a
    putStr "\n"
    where
     a = criaArvore formula
  



