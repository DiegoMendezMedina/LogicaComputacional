module LProp where

import Data.Char

{--Semántica formal de los conectivos lógicos--}

--Tipo de datos que representan las formulas de la lógica proposicional
data Prop = Top | Bot | P VarP | Neg Prop | Or Prop Prop | And Prop Prop | Impl Prop Prop | Syss Prop Prop deriving (Eq, Ord)

--Tipo para definir variables en la lógica proposicional
type VarP = Char

--Instancia para mostrar las formulas
-- (And(Or (P 'p') (P 'q')) (Or(Neg (P 'q')) (P 'r'))) = (('p' v 'q') ^ (~ 'q' v 'r'))
instance Show Prop where
  show Top = "T"
  show Bot = "F"
  show (P x) = show x
  show (Neg p) = "¬ "++show p
  show (Or p1 p2) = "("++(show p1)++" v "++(show p2)++")"
  show (And p1 p2) = "("++(show p1)++" ∧ "++(show p2)++")"
  show (Impl p1 p2) = "("++(show p1)++" --> "++(show p2)++")"
  show (Syss p1 p2) = "("++(show p1)++" <--> "++(show p2)++")"

--Conjunto donde guardaremos el valor de verdad de las variables
--Si la variable se encuentra en el conjunto, su valor de verdad es True
--Si no, es False
type Estado = [VarP]

--interp i φ = I*(φ) donde el estado i representa al estado I de la teoría.
-- interp ['q'] (And(Or (P 'p') (P 'q')) (Or(Neg (P 'q')) (P 'r'))) = False
interp :: Estado -> Prop -> Bool
interp estado prop = case prop of
  Top -> True
  Bot -> False
  P var -> inter estado (P var)
  Neg p -> not(interp estado p)
  Or p1 p2 -> interp estado p1 || interp estado p2
  And p1 p2 -> interp estado p1 && interp estado p2
  Impl p1 p2 -> not(interp estado p1) || (interp estado p2)
  Syss p1 p2 -> (not(interp estado p1) || (interp estado p2)) && (not(interp estado p2) || (interp estado p1))

--Funcion auxiliar para comprobar la interpretación de una variable (P Var)
inter :: Estado -> Prop -> Bool
inter [] (P var) = False
inter (x:xs) (P var) = if x == var
                            then True
                            else inter xs (P var)  


--Estados posibles: dada una proposicion φ con n variables proposicionales
--la funcion estados devuelve la lista con los 2^n estados distintos para φ
-- estados (And(Or (P 'p') (P 'q')) (Or(Neg (P 'q')) (P 'r'))) = ["","r","q","qr","p","pr","pq","pqr"]
estados :: Prop -> [Estado]
estados Top = []
estados Bot = []
estados (P x) = subconj (vars (P x))
estados (Neg p) = subconj (vars (Neg p))
estados (Or p1 p2) = subconj (vars (Or p1 p2))
estados (And p1 p2) = subconj (vars (And p1 p2))
estados (Impl p1 p2) = subconj (vars (Impl p1 p2))
estados (Syss p1 p2) = subconj (vars (Syss p1 p2))

--Funcion que devuelve la lista de variables proposicionales que figuran en φ (SIN REPETIR)
--vars (And(Or (P 'p') (P 'q')) (Or(Neg (P 'q')) (P 'r'))) = "pqr"
vars :: Prop -> [VarP]
vars Top = []
vars Bot = []
vars (P x) = [x]
vars (Neg p) = filtro(vars p)
vars (Or p1 p2) = filtro(vars p1 ++ vars p2)
vars (And p1 p2) = filtro(vars p1 ++ vars p2)
vars (Impl p1 p2) = filtro(vars p1 ++ vars p2)
vars (Syss p1 p2) = filtro(vars p1 ++ vars p2)


filtro :: Eq a => [a] -> [a]
filtro [] = []
filtro (x:xs) = x : (filtro (filter(/= x) xs))

--Dada una lista de elementos, devuelve la lista con las sublistas de la lista dada
-- subconj [1,2] = [[],[2],[1],[1,2]]
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = subconj xs ++ map (x:) (subconj xs)

{--CONCEPTOS SEMANTICOS BASICOS--}

--Dada una formula φ devuelve la lista de todos sus modelos.
--modelos (And(Or (P 'p') (P 'q')) (Or(Neg (P 'q')) (P 'r'))) = ["qr","p","pr","pqr"]
modelos :: Prop -> [Estado]
modelos phi = [i | i <- (estados phi), interp i phi]
--modelos(φ) = [i | i ∈ estados(φ), interp i φ = True]

--Funcion que verifica si una proposicion es tautologia
--tautologia (And(Or (P 'p') (P 'q')) (Or(Neg (P 'q')) (P 'r'))) = False
--tautologia (Impl (And (P 'p') (P 'q')) (P 'p'))
{-
p  |  q  | p ^ q  |  (p ^ q) -> p
---+-----+--------+-------------
V  |  V  |   V    |      V
V  |  F  |   F    |      V
F  |  V  |   F    |      V
F  |  F  |   F    |      V

-}
tautologia :: Prop -> Bool
tautologia prop = estados prop == modelos prop

--Verifica si una prop es satisfacible dada una interpretacion
satisfen :: Estado -> Prop -> Bool
satisfen i prop = interp i prop == True

--Verifica si la prop es satisfacible
satisf :: Prop -> Bool
satisf prop = modelos prop /= []

--Verifica si una prop es insatisfacible dada una interpretación
insatisfen :: Estado -> Prop -> Bool
insatisfen i prop = interp i prop == False

--Verifica si una prop es contradicción o insatisfacible
--contrad (And (P 'p') (Neg(P 'p')))
contrad :: Prop -> Bool
contrad prop = modelos prop == []

{--EQUIVALENCIA DE FÓRMULAS--}

--Decide si dos formulas son equivalentes
-- equiv (Impl (P 'p') (P 'q')) (Or (Neg(P 'p')) (P 'q')) 
equiv :: Prop -> Prop -> Bool
equiv prop1 prop2 = tautologia(Syss prop1 prop2)

--Dada una prop elimina las equivalencias (syss)
-- elimEquiv (And (Syss (P 'p') (P 'q')) (Syss (P 'q') (P 'r')))
elimEquiv :: Prop -> Prop
elimEquiv (P prop) = (P prop)
elimEquiv (Neg prop) = Neg (elimEquiv prop)
elimEquiv (Or prop1 prop2) = Or (elimEquiv prop1) (elimEquiv prop2)
elimEquiv (And prop1 prop2) = And (elimEquiv prop1) (elimEquiv prop2)
elimEquiv (Impl prop1 prop2) = Impl (elimEquiv prop1) (elimEquiv prop2)
elimEquiv (Syss prop1 prop2) = And (Impl (elimEquiv prop1) (elimEquiv prop2)) (Impl (elimEquiv prop2) (elimEquiv prop1))


--Dada una prop elñimina las implicaciones
--SUPONER QUE prop ya no cuenta con syss
-- elimImp(elimEquiv(Syss (P 'p') (P 'q'))) 
elimImp :: Prop -> Prop
elimImp (P prop) = (P prop)
elimImp (Neg prop) = Neg (elimImp prop)
elimImp (Or prop1 prop2) = Or (elimImp prop1) (elimImp prop2)
elimImp (And prop1 prop2) = And (elimImp prop1) (elimImp prop2)
elimImp (Impl prop1 prop2) = Or (Neg(elimImp prop1)) (elimImp prop2)
