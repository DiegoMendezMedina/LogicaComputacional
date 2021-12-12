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
fnn phi = fnn'(elimImp(elimEquiv(phi)))

-- | fnn': función auxiliar, ya no hay equivalencias ni implicaciones.
fnn' :: Prop -> Prop
fnn' (P phi) = (P phi)
fnn' (Neg(P phi)) = Neg(P phi)
fnn' (Neg(Neg phi)) = fnn' phi
fnn' (And phi psi) = And (fnn' phi)(fnn' psi)
fnn' (Or phi psi) = Or (fnn' phi)(fnn' psi)
fnn' (Neg (And psi phi)) =  Or (fnn'(Neg psi)) (fnn'(Neg phi))
fnn' (Neg (Or psi phi)) = And (fnn'(Neg psi)) (fnn'(Neg phi))


-- |2| Funcion distr la cual aplica adecuadamente las leyes distributivas a una formula proposicional
distr :: Prop -> Prop
distr (Or (P psi)(P phi)) = (Or (P psi)(P phi)) -- a a
distr (Or (Neg(P psi))((P phi))) = (Or (Neg(P psi))((P phi))) -- -a a
distr (Or ((P psi))(Neg(P phi))) = (Or ((P psi))(Neg(P phi))) -- a -a
distr (Or (Neg(P psi))(Neg(P phi))) = (Or (Neg(P psi))(Neg(P phi))) -- -a -a
distr (Or psi (And alpha beta)) = And ((Or psi alpha)) ((Or psi beta))
distr (Or (And alpha beta) psi) = And ((Or alpha psi)) ((Or beta psi))

-- | distr: leyes distributivas que recibe dos elementos de Prop. Unicamente se manda
--          llamar cuando en fnc se presenta un or. Checa la distributividad cuando
--          alguna de las recibidas tiene un And.
distr_dos :: Prop -> Prop -> Prop
--literales
distr_dos (P psi)(P phi) = Or (P psi)(P phi)
distr_dos (Neg (P psi))(Neg (P phi)) = Or (Neg (P psi))(Neg(P phi)) -- -a -a
distr_dos (Neg (P psi))(P phi) = Or (Neg (P psi))(P phi) -- -a a
distr_dos (P psi)(Neg (P phi)) = Or (P psi)(Neg (P phi))-- a -a
--
distr_dos (And alpha beta) phi = And (distr_dos alpha phi)(distr_dos beta phi)
distr_dos psi (And alpha beta) = And (distr_dos psi alpha)(distr_dos psi beta)
distr_dos psi (Or alpha beta) = Or psi (distr_dos alpha beta)
distr_dos (Or alpha beta) psi = Or (distr_dos alpha beta) psi

-- |3| Funcion fnc que recibe una formula φ y devuelve su forma normal conjuntiva, es decir:
--     Permite expresar cualquier formula proposicional como una conjunción de disyunciones.
-- IMPORTANTE: Se puede suponer que la formula de entrada ya se encuentra en forma normal negativa
fnc :: Prop -> Prop
fnc (P phi) = (P phi)
fnc (Neg (P phi)) = Neg (P phi)
fnc (And phi psi) = And (fnc phi)(fnc psi)
--fnc (Or phi psi) = distr (Or (fnc phi) (fnc psi))
fnc (Or phi psi) = distr_dos(fnc phi) (fnc psi)

--Definimos un tipo de dato Literal como sinónimo de fórmula proposicional
type Literal = Prop

--Definimos un tipo de dato Clausula como una lista de literales
type Clausula = [Literal]

-- |4| Función ctolist recibe una proposicion y devuelve la forma clausular de la proposicion
ctolist :: Prop -> Clausula
ctolist phi = ctolist'(fnc(fnn phi))

-- | ctolist': Función auxiliar, las proposiciones recibidas estan en forma normal conjuntiva.  
ctolist' :: Prop -> Clausula
ctolist' (P psi) = [(P psi)]
ctolist' (Or psi phi) = [Or psi phi]
ctolist' (And psi phi) = (ctolist' psi) ++ (ctolist' phi)

-- |5| Función fncC recibe una fórmula en forma normal conjuntiva y debe devolver su conversión en una lista de cláusulas.
fncC :: Prop -> [Clausula]
fncC psi = fncC_aux(ctolist psi)

-- | fncC_aux: función auxiliar para fncC, recibe las Claulas de una poposición.
fncC_aux :: Clausula -> [Clausula]
fncC_aux [] = []
fncC_aux [P psi] = [[P psi]]
fncC_aux (x:xs) = [(getAtoms x )]++ (fncC_aux xs)

-- | getAtoms: función auxiliara para fncC_aux, convierte prop en una lista  de atomicos.
getAtoms :: Prop -> Clausula
getAtoms (P phi) = [P phi]
getAtoms (Neg(P phi)) = [Neg(P phi)]
getAtoms (Or phi psi) = getAtoms phi ++ getAtoms psi

-- |6| Funcion fncConj recibe una proposicion y devuelve el conjunto de clausulas equivalentes a esa proposicion
fncConj :: Prop -> [Clausula]
fncConj phi = elimTautologias(elimDup(fncC(fnc(fnn phi))))

-- | elimDup: función auxiliar que elimina los elementos repetidos para cada lista de clausulas.
elimDup :: [Clausula] -> [Clausula]
elimDup [] = []
elimDup (x:xs) = [rmDup x] ++ elimDup xs

-- |  rmDup: función auxiliar, quita los repetidos de una clausula.
rmDup :: Clausula -> Clausula
rmDup [] = []
rmDup (x:xs)
  | elem x xs = rmDup xs
  | otherwise = [x]++rmDup xs

-- | elimTautologias: Elimina presencías de (psi, Neg psi) en cada una de las listas de las listas
--                    recibida.
elimTautologias :: [Clausula] -> [Clausula]
elimTautologias [] = []
elimTautologias (x:xs) = [rmTautologias x]++elimTautologias xs

-- | rmTautologias: Elimina presencias de (psi, Neg psi) en la lista recibida.
rmTautologias :: Clausula -> Clausula
rmTautologias [] = []
rmTautologias ((Neg x):xs)
  | elem x xs = rmTautologias((quitaElem x xs))
  | otherwise = [(Neg x)] ++ rmTautologias xs
rmTautologias (x:xs)
  | elem (Neg x) xs = rmTautologias((quitaElem (Neg x) xs))
  | otherwise = [x] ++ rmTautologias xs

-- | quitaElem: Borra presencias del elemento recibido en la lista(El mismo tambíen).
quitaElem :: Prop -> Clausula -> Clausula
quitaElem _ [] = []
quitaElem y (x:xs)
  | y == x = quitaElem y xs
  | otherwise = [x] ++ (quitaElem y xs)
  
{--
PUNTOS EXTRA
--}
-- |1| Función argcorrecto que verifica si un argumento con premisas y una conclusión es lógicamente correcto
--argcorrecto :: [Prop] -> Prop -> Bool
--argcorrecto lista conclusion = correcto(getArgFNC (lista ++ [fnc(fnn conclusion)]))


-- | gerArgFNC: Convierte un argumento(lista de proposiciones) a una lista de clausulas.
getArgFNC :: [Prop] -> [Clausula]
getArgFNC [] = []
getArgFNC (x:xs) = fncC x ++ getArgFNC xs

--correcto' :: [Clausula] -> Bool
--correcto' (x:xs) = contrario x xs

--contrario :: Clausula -> [Clausula] -> Bool
--contratio (x:xs) (y:ys) =



-- Probando  
a = P 'a'
b = P 'b'
c = P 'c'

equivalencia = Syss (And a c) (Or a c)
equiv' = fnn equivalencia
imp = Impl(Neg a) b
b'' = elimEquiv equivalencia
b''' = elimImp b''

test_fnn = equiv' == (And (Or( Or(Neg a)(Neg c))(Or a c))(Or(And(Neg a)(Neg c))(And a c)))

ordis = (Or (And a b) (And a c))
anddis = (And (Or a b) (Or a c))

distr_test = distr_dos (And a b) c
distr_test2 = distr_dos (Or a b)(Or a c)

{--
Semanal 3
--}

m = P 'M'
p = P 'P'
f = P 'F'
s = P 'S'

primera = Impl m (Or (Neg p)(Neg f))
segunda = Impl f s
tercera = Impl (And s p) m
cuarta = p
conclusion = Neg f

fnn_primera = fnn primera --bn
fnn_seg = fnn segunda --bn 
fnn_ter = fnn tercera --bn 
fnn_cuarta = fnn cuarta --bn

fnc_prim = fnc fnn_primera --bn
fnc_seg = fnc fnn_seg -- bn
fnc_ter = fnc fnn_ter -- bn
fnc_cuarta = fnc fnn_cuarta -- bn

premisas = [primera, segunda, tercera, cuarta]
argumentofnc = getArgFNC (premisas++ [fnc(fnn (Neg conclusion))])

{--
Ejemplo 2.1
--}
q = P 'Q'
r = P 'R'

dos_uno = Impl (Impl (Neg p) q) (Impl (Neg r) s)
test_ejemplo2 = fnc(fnn dos_uno) == And(Or(Neg p)(Or r s))(Or(Neg q)(Or r s))
fnc_dos_uno = fnc(fnn dos_uno)
c_dos_uno = ctolist fnc_dos_uno
fncC_dos_uno = fncC fnc_dos_uno

{--
Ejemplo 3.1
--}

prim = Impl p (Impl q r)
seg = Neg(Impl q r)

fncprim = fnc(fnn prim) -- Funciona
fncseg = fnc(fnn seg) -- Tmb funciona yupi

ejem = (Syss (And a b)(Or (And b a)(c)))
ejemfnc = fnc (fnn ejem) -- bien
ejemcC = fncC ejem
ejemcCsin = fncConj ejem
