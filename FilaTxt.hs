module FilaTxt
( Fila
, clear
, enqueue
, contaChar
, dequeue
, firstElement 
)where

import System.IO


data Fila a = Fvazia
		| Fl [a]
     deriving (Eq, Show)


clear :: String -> IO()
clear arquivo = writeFile arquivo ""


enqueue :: String -> String -> IO()
enqueue arquivo escrever = appendFile arquivo (escrever++"\n")	


contaChar :: String -> Int -> Int
contaChar texto posicao | texto !! posicao == '\n' = posicao+1
		      | otherwise = contaChar texto (posicao+1)


dequeue arquivo = do
	texto <- readFile arquivo
	length texto `seq` (writeFile (arquivo) (drop (contaChar(texto) (0)) (texto)))
	

firstElement arquivo = do
	texto <- readFile arquivo	 
	return (take ((contaChar (texto) (0) -1)) (texto))


--Implementação
-- enqueue "nomeDoArquivo.txt" "primeira linha"
-- enqueue "nomeDoArquivo.txt" "segunda linha"
-- firstElement "nomeDoArquivo.txt"
-- dequeue "nomeDoArquivo.txt"
-- clear "nomeDoArquivo.txt"