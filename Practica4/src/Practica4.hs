{-
-- | Lógica Computacional 2022-01
-- | Práctica 4: Unificación
-- | Profesor: Dr. Favio E. Miranda Perea
-- | Ayudante: Javier Enríquez Mendoza
-- | Ayudante: Ramón Arenas Ayala
-- | Laboratorio: Daniel Lugo Cano
-}

module Practica4 where

import LPO
import LPOSintac

-- |1| Funcion simpSus que dada una sustitución elimina de ella los pares con componentes iguales
simpSus :: Subst -> Subst
simpSus [] = []
simpSus (x:xs) = simpSus_aux x ++ simpSus xs

-- | simpSus_aux: Función auxiliar para simpSus, recibe los elementos sobre los que
--                itera simpSus. Hace la eliminación en caso de ser necesaria.
simpSus_aux :: (Nombre, Term) -> Subst
simpSus_aux (nombre, (V v))
  | nombre /= v = [(nombre, V v)]
  | otherwise   = []
simpSus_aux x = [x]

-- |2| Funcion compSus la cual recibe dos Subst y devuelve su compisición.
compSus :: Subst -> Subst -> Subst
compSus [] x = []
compSus x y = compSus_aux x y ++ r
  where
    r = restante y (names x) 

-- | compSus_aux: Función auxiliar recursiva que nos permite iterar y así
--                poder calcular los restantes en compSus
compSus_aux :: Subst -> Subst -> Subst
compSus_aux [] x = []
compSus_aux ((nombre, termino):xs) y = [(nombre, (sust termino  y))] ++ compSus_aux xs y

-- | sust: El termino recibido checa si puede o no hacer la sustitución recibida,
--         en caso de poder la hace, si no regresa el termino como estaba (caso base).
sust :: Term -> Subst -> Term
sust x [] = x
sust (V v) ((nombre, termino):xs)
  | v == nombre = termino 
  | otherwise   =  sust (V v) xs
sust (F n x) y = F n (sustFunc x y)

-- | sustFunc: función auxiliar de func para el caso de las funciones.
sustFunc :: [Term] -> Subst -> [Term]
sustFunc [] y = []
sustFunc (x:xs) y = [sust x y] ++ sustFunc xs y

-- | restante: regresa las sustituciones que no se encontraban en la original
restante :: Subst -> [Nombre] -> Subst
restante [] _ = []
restante ((nombre, term):xs) y
  | elem nombre y =  restante xs y
  | otherwise     = [(nombre, term)] ++ restante xs y
    
-- |3| Funcion que dados dos términos devuelva una lista de sustituciones que cumplan las condiciones dadas
unifica :: Term -> Term -> [Subst]
unifica (F c1 []) (F c2 [])  -- Los terminos son constantes
  | c1 == c2  = [[(c1, V "x")]] -- Cualquier sustitución es valida
  | otherwise = []
unifica (V n) (F c terms) -- Alguno de los terminos es variable
  | esta n terms = error "La variable se encuentra en una función del otro termino"
  | otherwise    = [[(n, (F c terms))]]
unifica (F c terms) (V n)  -- Alguno de los terminos es variable, segundo caso
  | esta n terms = error "La variable se encuentra en una función del otro termino"
  | otherwise    = [[(n, (F c terms))]]
unifica (F c1 terms1) (F c2 terms) -- Los dos son funcionales
  | c1 == c2 = unificaFunc terms1 terms
  |otherwise = []
unifica (V c1) (V c2) -- ambos son variables
  | c1 == c2  = []
  | otherwise = [[(c1, V c2)]]

-- | esta: Checa si el nombre figura en la lista recibida    
esta :: Nombre -> [Term] -> Bool
esta n [] = False
esta n ((V c):xs)
  | n == c = True
  | otherwise = esta n xs
esta n ((F c terms):xs)
  | n == c    = True 
  | otherwise = (esta n xs) || (esta n terms)

-- | unificaFunc: función auxiliar de unifica, itera sobre las listas
--                elemento a elemento.
-- *NOTA: La función supone son de la misma longitud.
unificaFunc :: [Term] -> [Term] -> [Subst]
unificaFunc [] _ = []
unificaFunc _ [] = []
unificaFunc (x:xs)(y:ys) = (unifica x y )++ (unificaFunc xs ys)

-- |4| Funcion que unifica dos listas de términos de la misma longitud componente a componente
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas [] [] = []
unificaListas (x:xs)(y:ys) = (unifica x y) ++ unificaListas xs ys

-- |5| Funcion unificaConj que implementa el caso general para unificar un conjunto (lista)
unificaConj :: [Term] -> [Subst]
unificaConj x
  | verifica(namesS(conj)) = conj
  | otherwise = error "no unificables"
  where
    conj = elim_dup (unificaConj_aux x)

-- | unificaConj_aux: función auxiliar, itera y checa si hay error    
unificaConj_aux :: [Term] -> [Subst]
unificaConj_aux (x:y:[])
  | sust2 y (unifica x y) ==y && (unifica x y) /= []
    && sust2 y (unifica x y) /=x = error "no unificables"
  | otherwise        = unifica x y
unificaConj_aux (x:y:xs) 
  | y2 == y && miu /= [] = error "no unificables"
  | otherwise                 = miu ++ (unificaConj_aux (y2:nuevalista))
  where
    miu = (unifica x y)
    y2 = (sust2 y miu)
    nuevalista = nl_aux xs miu

-- | nl_aux: Nos genera la nueva lista 
nl_aux :: [Term] -> [Subst] -> [Term]
nl_aux [] _ = []
nl_aux _ [] = []
nl_aux x (y:ys) = (sustFunc x y) ++ (nl_aux x ys)

-- | sust2: auxiliar para sust, [Subst] en vez de Subst.
sust2 :: Term -> [Subst] -> Term
sust2 x [] = x
sust2 x (y:ys) = sust2 (sust x y) ys

-- | elim_dup: elimina duplicados, para checar si es correcta la Subst
elim_dup :: [Subst] -> [Subst]
elim_dup [] = []
elim_dup (x:xs)
  | elem x xs = elim_dup xs
  | otherwise = [x] ++ elim_dup xs

-- | verifica: Si hay dos nombres iguales esta mal, llamar antes a elim_dup
verifica :: [Nombre] -> Bool
verifica [] = True
verifica (x:xs)
  | elem x xs = False
  | otherwise = verifica xs

-- | namesS: da los nombres en SUBST
namesS :: [Subst] -> [Nombre]
namesS [] = []
namesS ([(nombre, term)]:xs) = [nombre] ++ namesS xs
  
-- |6| Funcion que unifica dos literales
unificaLit :: Form -> Form -> [Subst]
unificaLit = error "Implementar, ya toy cansadito. Perdon :c"


phi = [("x", F "f" [V "y"]), ("y", V "z")]
gamma = [("x", F "g" [V "w"]), ("z", V "m"), ("z", V "w")]
theta = [("y", V "m"), ("w", F "f" [V "n"]), ("v", V "w")]


uno = [("x", V "a"), ("o", V "b")]
dos = [("z", V "x"), ("b", V "o")]

t10 = (F "f"[F "g" [V "x"], F "h"[V "x", V "u"]])
t11 = (F "f"[V "z",F "h"[F "f"[V "y", V "y"], V "z"]])
test_unifica = unifica t10 t11

t1 = F "h" [F "f"[V "w"], F "f"[V "w"]]
t2 = F "h" [F "f"[V "w"], F "f"[V "x"]]
t3 = F "h" [V "z", V "z"]
t4 = V "x"


alpha = Pr "P" [F "f"[F "h" [V "z"], F "h"[V "z"]]]
beta = Pr "P" [V "x"]
zeta = Pr "P" [F "f"[F "h"[V "z"], F "h" [V "x"]]]


-- Ejemplo 4, nota 9 pag 4.
ejemplo4_1 = unifica (F "c" [])(F "d" []) == []
ejemplo4_2 = unifica (V "x")(F "f" [V "y"]) == [[("x", F "f" [V "y"])]]
ejemplo4_3 = unifica (F "g" [V "x", V "w"])(F "h" [V "y", F "a" []]) == []
ejemplo4_4 = unifica (F "f" [V "x", F "g" [V "y"], V "w"])
             (F "f" [F "a" [], F "g" [F "b" []], F "h" [V "w"]]) -- lanza error, w figura en
                                                                 -- h w
