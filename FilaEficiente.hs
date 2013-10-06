module FilaEficiente (Fila, push, pop, front, vazia, novaFila) where

data Fila t = Fila [t] [t]

novaFila :: Fila t
novaFila = Fila [] []

vazia :: Fila t -> Bool
vazia (Fila [] []) = True
vazia _ = False

front :: Fila t -> t
front (Fila [] []) = error "Fila vazia!"
front (Fila (x:xs) _ ) = x
front (Fila [] ys) = front (Fila (reverse ys) [])

push :: t -> Fila t = Fila t
push y (Fila [] []) = Fila [y] []
push y (Fila _ ys) = Fila _ (y:ys)

pop :: Fila t -> Fila t
pop (Fila (x:xs) ys) = Fila xs ys
pop (Fila [] []) = error "Fila vazia"
pop (Fila [] ys) = Fila tail (reverse ys) []