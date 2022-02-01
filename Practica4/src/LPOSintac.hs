{-
LÓGICA DE PRIMER ORDEN: CONCEPTOS SINTÁCTICOS IMPROTANTES
-}

module LPOSintac where

import LPO

import Data.List

-- φ = ∀x∃z( Q(y, z) ∨ R(z, x, y) ∧ P(z, x))

--Funcion que devuelve la lista con todos 
--los nombres de las variables que figuran 
--en t
varT :: Term -> [Nombre]
varT (V x) = [x]
varT (F s []) = []
varT (F s xs) = varTL xs where
    varTL [] = []
    varTL (x:xs) = union (varT x) (varTL xs)


fv :: Form -> [Nombre]
fv TrueF = []
fv FalseF = []
fv (Pr n lt) = concat (map varT lt)
fv (Eq t1 t2) = union (varT t1) (varT t2)
fv (Neg f) = fv f
fv (Conj t1 t2) =  union (fv t1) (fv t2)
fv (Disy t1 t2) =  union (fv t1) (fv t2)
fv (Imp t1 t2) =  union (fv t1) (fv t2)
fv (Equi t1 t2) =  union (fv t1) (fv t2)
fv (All n f) = [y | y <- fv f, n /= y]
fv (Ex n f) = [y | y <- fv f, n /= y]


bv :: Form -> [Nombre]
bv TrueF = []
bv FalseF = []
bv (Pr n lt) = []
bv (Eq t1 t2) = []
bv (Neg f) = bv f
bv (Conj t1 t2) =  union (bv t1) (bv t2)
bv (Disy t1 t2) =  union (bv t1) (bv t2)
bv (Imp t1 t2) =  union (bv t1) (bv t2)
bv (Equi t1 t2) =  union (bv t1) (bv t2)
bv (All n f) = union [n] (bv f)
bv (Ex n f) = union [n] (bv f)


-- subFCuan (All "x" (Pr "Q" [V "x",F "g" [V "x"]]))
-- [All "x" (Pr "Q" [V "x",F "g" [V "x"]])]
-- subFCuan (Pr "Q" [V "x",F "g" [V "x"]])
-- []
subFCuan :: Form -> [Form]
subFCuan (Neg f) = subFCuan f
subFCuan (Conj f1 f2) = (subFCuan f1) ++ (subFCuan f2)
subFCuan (Disy f1 f2) = (subFCuan f1) ++ (subFCuan f2)
subFCuan (Imp f1 f2) = (subFCuan f1) ++ (subFCuan f2)
subFCuan (Equi f1 f2) = (subFCuan f1) ++ (subFCuan f2)
subFCuan (All n f) = [All n f] ++ (subFCuan f)
subFCuan (Ex n f) = [Ex n f] ++ (subFCuan f)
subFCuan f = []


duplaCuan :: Form -> (Nombre, Form)
duplaCuan (All n f) = (n, f)
duplaCuan (Ex n f) = (n, f)

--al tomar como entrada P esta función
--devuelve una lista de pares de la forma 
--(A, B) donde A es una cuantificación 
--que es subfórmula de P y B
--es el alcance de A.
--EJEMPLO:
--alcF (All "x" (Pr "Q" [V "x",F "g" [V "x"]]))
-- [("x",Pr "Q" [V "x",F "g" [V "x"]])]
-- alcF (All "x" (Ex "y" (Pr "P" [V "z",V "w"])))
-- [("x",Ex "y" (Pr "P" [V "z",V "w"])),("y",Pr "P" [V "z",V "w"])]
alcF :: Form -> [(Nombre, Form)]
alcF f = map (duplaCuan) (subFCuan f)

-- Sinónimo para representar una sustitución
type Subst = [(Nombre, Term)]

--verifVector [("x", V "x")]
-- False
-- verifVector [("x", V "y"), ("y", V "x")]
-- True
--verificar si los nombres de variables en esta lìsta de 
--sustituciónes no aparecen cómo términos que sustituirán a variables.
verifVector :: Subst -> Bool
verifVector [] = True 
verifVector (x:xs) = (verifV (fst x) (snd x)) && (verifVector xs)

verifV :: Nombre -> Term -> Bool
verifV x (V y) = x /= y
verifV x (F _ _) = True

-- Devuelve una lista con los nombres que figuran en la lista de sustituciones
names :: Subst -> [Nombre]
names [] = []
names (x:xs) = [fst x] ++ (names xs)

namesU :: Subst -> [Nombre]
namesU [] = []
namesU (x:xs) = union [fst x] (namesU xs)

--Funcion que verifica si la lista dada es una sustitución,
-- es decir, que cumplan con la definición
verifSus :: Subst -> Bool
verifSus x = (verifVar x) && (verifVector x) where
    verifVar x = (names x) == (namesU x)


apSubTaux :: Term -> Subst -> Term
apSubTaux t [] = t
apSubTaux (V z) (x:xs) = if (z == (fst x)) then snd x else (apSubT (V z) xs)
apSubTaux (F s []) _ = F s []
apSubTaux (F s r) ls = F s (apSubTlist r ls) where

apSubTlist [] _ = []
apSubTlist (x:xs) l = [apSubT x l] ++ (apSubTlist xs l)

apSubT :: Term -> Subst -> Term
apSubT t l = if (verifSus l) then (apSubTaux t l) else error "Sustituciónn inválida."
