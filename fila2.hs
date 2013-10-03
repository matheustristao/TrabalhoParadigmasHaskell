module (Queue, enqueue) where
enqueue :: t -> Queue t -> Queue t
dequeue :: Queue t-> Queue t

data Queue t = Fila [t]

enqueue x (Fila q) = Fila (q ++ [x]) -- insere o elemento no final da lista

dequeue (Fila(x:xs)) = Fila xs -- tira o elemento da cabeca da lista
dequeue _ = error "Fila de espera vazia"