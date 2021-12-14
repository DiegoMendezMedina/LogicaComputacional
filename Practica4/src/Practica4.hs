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
simpSus = error "Implementar"

-- |2| Funcion compSus la cual recibe dos Subst y devuelve su compisición.
compSus :: Subst -> Subst -> Subst
compSus = error "Implementar"

-- |3| Funcion que dados dos términos devuelva una lista de sustituciones que cumplan las condiciones dadas
unifica :: Term -> Term -> [Subst]
unifica = error "Implementar"

-- |4| Funcion que unifica dos listas de términos de la misma longitud componente a componente
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas = error "Implementar"

-- |5| Funcion unificaConj que implementa el caso general para unificar un conjunto (lista)
unificaConj :: [Term] -> [Subst]
unificaConj = error "Implementar"

-- |6| Funcion que inifica dos literales
unificaLit :: Form -> Form -> [Subst]
unificaLit = error "Implementar"
