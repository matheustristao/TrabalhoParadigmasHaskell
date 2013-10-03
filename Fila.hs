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

vazia :: Fila a -> Fila a
vazia(Fl a) = Fvazia

push :: a -> Fila a -> Fila a
push x(Fl y) = Fl(x:y)	

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