module Fila
( Fila
, vazia
, push
, pop
, listaEmFila
)where


data Fila a = Fvazia
		| Fl [a]
     deriving (Eq, Show)


vazia :: String -> IO()
vazia arquivo = writeFile arquivo ""


push :: String -> String -> IO()
push arquivo escrever = appendFile arquivo (escrever++"\n")	


pop :: Fila a -> Fila a
pop (Fl []) = error "Fila Vazia!!!!"
pop (Fl(a:b)) = Fl b


top :: Fila a -> a
top (Fl []) = error "Fila Vazia!!!!"
top (Fl(a:b)) = a


listaEmFila :: [a] -> Fila a
listaEmFila x = Fl x


--Implementação
--import Fila
-- push 2 (listaEmFila[0,1]) retorna Fl [2,0,1]
-- pop (listaEmFila[0,1]) retorna Fl [1]
-- top (listaEmFila[0,1]) retorna 0