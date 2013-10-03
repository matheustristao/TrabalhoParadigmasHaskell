module Pilha
( Pilha
, vazia
, push
, pop
, listaEmPilha
)where

data Pilha a = Pvazia
		| Pl [a]
     deriving (Eq, Show)

vazia :: Pilha a -> Pilha a
vazia(Pl a) = Pvazia

push :: a -> Pilha a -> Pilha a
push x(Pl y) = Pl(x:y)	

pop :: Pilha a -> Pilha a
pop (Pl []) = error "Pilha Vazia!!!!"
pop (Pl(a:b)) = Pl b

top :: Pilha a -> a
top (Pl []) = error "Pilha Vazia!!!!"
top (Pl(a:b)) = a

listaEmPilha :: [a] -> Pilha a
listaEmPilha x = Pl x

--Implementação
--import Pilha
-- push 2 (listaEmPilha[0,1]) retorna Pl [2,0,1]
-- pop (listaEmPilha[0,1]) retorna Pl [1]
-- top (listaEmPiha[0,1]) retorna 0