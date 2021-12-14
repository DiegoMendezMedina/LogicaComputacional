module LPO where

import Data.List

--Tipo para representar el nombre
type Nombre = String

--Tipo para representar los terminos
data Term = V Nombre | F Nombre [Term] deriving (Show, Eq)

--Tipo para representar las formulas
data Form = TrueF 
          | FalseF
          | Pr Nombre [Term]
          | Eq Term Term 
          | Neg Form 
          | Conj Form Form 
          | Disy Form Form 
          | Imp Form Form 
          | Equi Form Form 
          | All Nombre Form 
          | Ex Nombre Form deriving (Show, Eq)

--Función que devuelve la lista de subterminos dado un termino
{-
--Ejemplos
*LPO> subt (F "h" [F "b" [], F "f" [F "a" []], V "z"])
[F "h" [F "b" [],F "f" [F "a" []],V "z"],F "b" [],F "f" [F "a" []],F "a" [],V "z"]
*LPO> subt (F "h" [F "f" [V "x"]])
[F "h" [F "f" [V "x"]],F "f" [V "x"],V "x"]
-}
subt :: Term -> [Term]
subt (V x) = [V x]
subt (F n []) = [F n []]
subt (F n xs) = union [F n xs] (subtL xs) where
subtL [] = []
subtL (x:xs) = union (subt x) (subtL xs)

--Funcion que devuelve el numero de conectivos y cuantificadores sobre una formula
{-
--Ejemplos
*LPO> peso (Imp (Pr "R" [V "x"]) (Eq (V "x") (F "b" [])))
2
*LPO> peso (All "x" (Pr "Q" [V "x",F "g" [V "x"]]))
1
-}
peso :: Form -> Int
peso TrueF = 0
peso FalseF = 0
peso (Pr s t) = 0
peso (Eq f1 f2) = 1
peso (Neg f) = (peso f) + 1
peso (Conj f1 f2) = (peso f1) + (peso f2) + 1
peso (Disy f1 f2) = (peso f1) + (peso f2) + 1
peso (Imp f1 f2) = (peso f1) + (peso f2) + 1
peso (Equi f1 f2) = (peso f1) + (peso f2) + 1
peso (All x p) = 1 + (peso p)
peso (Ex x p) = 1 + (peso p) 
