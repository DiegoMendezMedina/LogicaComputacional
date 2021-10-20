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
fnn (P phi) = (P phi)
fnn phi = elim_dobleneg(demorgan(elimImp(elimEquiv(phi))))

-- | demorgan: función auxiliar para fnn, decidimos implementar la implicación y la
--             equivalencía, aunque en principio para fines de esta práctica, no deberia recibirlo
demorgan :: Prop -> Prop
demorgan (P psi) = (P psi)
demorgan (Neg (Or psi phi)) = And(Neg (demorgan(psi)))(Neg (demorgan(phi)))
demorgan (Neg (And psi phi)) = Or(Neg (demorgan(psi)))(Neg (demorgan(phi)))
demorgan (Neg psi) = Neg (demorgan psi)
demorgan (Or psi phi) = Or (demorgan psi) (demorgan phi)
demorgan (And psi phi) = And (demorgan psi) (demorgan phi)
demorgan (Impl psi phi) = Impl (demorgan psi)(demorgan phi)
demorgan (Syss psi phi) = Syss (demorgan psi)(demorgan phi)

-- | elim_dobleneg: función auxiliar para fnn, elimina doble negaciones.
--                  De nuevo decidimos extenderla a implicaciones y equivalencias.
elim_dobleneg :: Prop -> Prop
elim_dobleneg (P psi) = (P psi)
elim_dobleneg (Neg (Neg psi)) = elim_dobleneg psi
elim_dobleneg (Neg psi) = Neg (elim_dobleneg psi)
elim_dobleneg (And psi phi) = And (elim_dobleneg psi)(elim_dobleneg phi)
elim_dobleneg (Or psi phi) = Or (elim_dobleneg psi)(elim_dobleneg phi)
elim_dobleneg (Impl psi phi) = Impl (elim_dobleneg psi)(elim_dobleneg phi)
elim_dobleneg (Syss psi phi) = Syss (elim_dobleneg psi)(elim_dobleneg phi)

-- |2| Funcion distr la cual aplica adecuadamente las leyes distributivas a una formula proposicional
distr :: Prop -> Prop
distr (P psi) = (P psi)
distr (Or (And psi phi) (And alpha chi)) 
  | psi == alpha = And (distr psi)(Or (distr phi) (distr chi))
  | chi == phi = And (distr phi)(Or (distr psi) (distr alpha))
distr (And(Or psi phi) (Or alpha chi))
  | psi == alpha = Or( distr psi)(And (distr phi) (distr chi))
  | phi == chi = Or( distr phi)(And (distr psi) (distr alpha))
distr (Neg psi) = Neg (distr psi)


-- | distr: leyes distributivas que recibe dos elementos de Prop. Unicamente se manda
--          llamar cuando en fnc se presenta un or. Checa la distributividad cuando
--          alguna de las recibidas tiene un And.
distr_dos :: Prop -> Prop -> Prop
distr_dos (P psi)(P phi) = Or (P psi)(P phi)
distr_dos (Neg (P psi))(Neg (P phi)) = Or (Neg (P psi))(Neg(P phi))
distr_dos (Neg (P psi))(P phi) = Or (Neg (P psi))(P phi)
distr_dos (P psi)(Neg (P phi)) = Or (P psi)(Neg (P phi))
distr_dos (P psi) phi = Or (P psi) (fnc phi)
distr_dos psi (P phi) = Or (fnc psi) (P phi)
distr_dos (Neg(P psi)) phi = Or (Neg(P psi)) (fnc phi)
distr_dos psi (Neg(P phi)) = Or (fnc psi) (Neg(P phi))
distr_dos (And alpha beta) phi = And (distr_dos alpha phi)(distr_dos beta phi)
distr_dos psi (And alpha beta) = And (distr_dos psi alpha)(distr_dos psi beta)
      
-- |3| Funcion fnc que recibe una formula φ y devuelve su forma normal conjuntiva, es decir:
--     Permite expresar cualquier formula proposicional como una conjunción de disyunciones.
-- IMPORTANTE: Se puede suponer que la formula de entrada ya se encuentra en forma normal negativa
fnc :: Prop -> Prop
fnc (P phi) = (P phi)
fnc (Neg (P phi)) = Neg (P phi)
fnc (And phi psi) = And (fnc phi)(fnc psi)
fnc (Or phi psi) = distr_dos (fnc phi) (fnc psi)

--Definimos un tipo de dato Literal como sinónimo de fórmula proposicional
type Literal = Prop

--Definimos un tipo de dato Clausula como una lista de literales
type Clausula = [Literal]

-- |4| Función ctolist recibe una proposicion y devuelve la forma clausular de la proposicion
ctolist :: Prop -> Clausula
ctolist (P psi) = [(P psi)]
ctolist (Or psi phi) = [Or psi phi]
ctolist (And psi phi) = (ctolist psi) ++ (ctolist phi)

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


a = P 'a'
b = P 'b'
c = P 'c'
equivalencia = Syss (And a c) (Or a c)
equiv' = fnn equivalencia
imp = Impl(Neg a) b
b'' = elimEquiv equivalencia
b''' = elimImp b''
dneg = Neg(Neg a)
dnegand = And (Neg(Neg a))(Neg b)

test_fnn = equiv' == (And (Or( Or(Neg a)(Neg c))(Or a c))(Or(And(Neg a)(Neg c))(And a c)))
test_dobleNeg = (And a b) == elim_dobleneg dnegand

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

fnn_primera = fnn primera --bn
fnn_seg = fnn segunda --bn 
fnn_ter = fnn tercera --bn 
fnn_cuarta = fnn cuarta --bn

fnc_prim = fnc fnn_primera --bn
fnc_seg = fnc fnn_seg -- bn
fnc_ter = fnc fnn_ter -- bn
fnc_cuarta = fnc fnn_cuarta -- bn

{--
Ejemplo 2.1
--}
q = P 'q'
r = P 'r'

dos_uno = Impl (Impl (Neg p) q) (Impl (Neg r) s)
test_ejemplo2 = fnc(fnn dos_uno) == And(Or(Neg p)(Or r s))(Or(Neg q)(Or r s))
{--
Ejemplo 3.1
--}

prim = Impl p (Impl q r)
seg = Neg(Impl q r)

fncprim = fnc(fnn prim) -- Funciona
fncseg = fnc(fnn seg) -- Tmb funciona yupi
