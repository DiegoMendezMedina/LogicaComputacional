{-
-- | Lógica Computacional 2022-01
-- | Práctica 2: Forma normal negativa y conjuntiva
-- | Profesor: Dr. Favio E. Miranda Perea
-- | Ayudante: Javier Enríquez Mendoza
-- | Ayudante: Ramón Arenas Ayala
-- | Laboratorio: Daniel Lugo Cano
-}

module Practica2 where

import LProp


-- |1| Funcion fnn que recibe una formula φ y devuelve su forma normal negativa
fnn :: Prop -> Prop
fnn = error "Implementar"

-- |2| Funcion distr la cual aplica adecuadamente las leyes distributivas a una formula proposicional
distr :: Prop -> Prop
distr = error "Implementar"

-- |3| Funcion fnc que recibe una formula φ y devuelve su forma normal conjuntiva, es decir:
--     Permite expresar cualquier formula proposicional como una conjunción de disyunciones.
-- IMPORTANTE: Se puede suponer que la formula de entrada ya se encuentra en forma normal negativa
fnc :: Prop -> Prop
fnc = error "Implementar"

--Definimos un tipo de dato Literal como sinónimo de fórmula proposicional
type Literal = Prop

--Definimos un tipo de dato Clausula como una lista de literales
type Clausula = [Literal]

-- |4| Función ctolist recibe una proposicion y devuelve la forma clausular de la proposicion
ctolist :: Prop -> Clausula
ctolist = error "Implementar"

-- |5| Función fncC recibe una fórmula en forma normal conjuntiva y debe devolver su conversión en una lista de cláusulas.
fncC :: Prop -> [Clausula]
fncC = error "Implementar"

-- |6| Funcion fncConj recibe una proposicion y devuelve el conjunto de clausulas equivalentes a esa proposicion
fncConj :: Prop -> [Clausula]
fncConj = error "Implementar"


{--
PUNTOS EXTRA
--}
-- |1| Función argcorrecto que verifica si un argumento con premisas y una conclusión es lógicamente correcto
argcorrecto :: [Prop] -> Prop -> Bool
argcorrecto = error "Implementar"
