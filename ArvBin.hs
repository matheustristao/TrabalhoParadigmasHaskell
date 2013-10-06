module ArvBin (ArvBin, preordem, emordem, posordem, alturaArv, balanceada) where

data ArvBin a = Null 
	    | No a (ArvBin a) (ArvBin a)

preordem :: ArvBin a -> [a]
preordem Null = []
preordem (No x esq dir) =  [x] ++  (preordem esq) ++ (preordem dir) 

emordem :: ArvBin a -> [a] 
emordem Null = []
emordem (No x esq dir) = (emordem esq) ++ [x] ++ (emordem dir)  

posordem :: ArvBin a -> [a]
posordem Null = []
posordem (No x esq dir) =   (posordem esq) ++ (posordem dir) ++ [x]

--alturaArv :: ArvBin a -> int
alturaArv Null = 0
alturaArv (No a esq dir) = 1 + max (alturaArv esq) (alturaArv dir)

balanceada :: ArvBin a -> Bool
balanceada Null = True
balanceada (No a esq dir) = (abs ((alturaArv esq)-(alturaArv dir))) <= 1 && (balanceada esq) && (balanceada dir)