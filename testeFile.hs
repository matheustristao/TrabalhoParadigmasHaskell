module Fila
( Fila
, vazia
, push
, lerChar
, pop
, top
, listaEmFila
)where

import System.IO

data Fila a = Fvazia
		| Fl [a]
     deriving (Eq, Show)


vazia :: String -> IO()
vazia arquivo = writeFile arquivo ""


push :: String -> String -> IO()
push arquivo escrever = appendFile arquivo (escrever++"\n")	


lerChar :: String -> Int -> Int
lerChar texto posicao | texto !! posicao == '\n' = posicao+1
		      | otherwise = lerChar texto (posicao+1)


pop arquivo = do
	texto <- readFile arquivo
	length texto `seq` (writeFile (arquivo) (drop (lerChar(texto) (0)) (texto)))
	


top :: Fila a -> a
top (Fl []) = error "Fila Vazia!!!!"
top (Fl(a:b)) = a


listaEmFila :: [a] -> Fila a
listaEmFila x = Fl x


--Implementa��o
--import Fila
-- push 2 (listaEmFila[0,1]) retorna Fl [2,0,1]
-- pop (listaEmFila[0,1]) retorna Fl [1]
-- top (listaEmFila[0,1]) retorna 0