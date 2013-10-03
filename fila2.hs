module (Queue, enqueue, queueEmpty, front, newQueue) where
enqueue :: t -> Queue t -> Queue t
dequeue :: Queue t-> Queue t
queueEmpty :: Queue t -> Bool
front :: Queue t -> t
newQueue :: Queue t

data Queue t = Fila [t]

enqueue x (Fila q) = Fila (q ++ [x]) -- insere o elemento no final da lista

dequeue (Fila(x:xs)) = Fila xs -- tira o elemento da cabeca da lista
dequeue _ = error "Fila de espera vazia"

queueEmpty (Fila[]) = True
queueEmpty _ = False

front (Fila (x : _) = x -- o front eh o elemento a ser retirado
front _ = error "Fila vazia"

newQueue = (Fila[])