module DPLL where

-- Importen los módulos necesarios de las prácticas anteriores para tener sus definiciones
-- de lógica proposicional

import LProp

type Literal       = Prop
type Clausula      = [Literal]
type Modelo        = [Literal]
type Formula       = [Clausula]
type Configuracion = (Modelo,Formula)

--------------------------------------  BÚSQUEDA DE MÓDELOS HACIA ATRAS ------------------------------

unit :: Configuracion -> Configuracion
unit conf = unit_aux conf []

-- | unit_aux: Función auxiliar para hacer llamadas recursivas 
unit_aux :: Configuracion -> Formula -> Configuracion
unit_aux ([], []) f = ([], f)
unit_aux (m, []) f = (m, f)
unit_aux (m, x:xs) f
  | length x == 1 && not (elem (head x) m) =
    (m++x, f++xs)
  | otherwise =  unit_aux (m, xs) (f++[x])


elim :: Configuracion -> Configuracion
elim conf = elim_aux conf []

-- | elim_aux: Función auxiliar para hacer recursión 
elim_aux :: Configuracion -> Formula -> Configuracion
elim_aux ([], []) f = ([], f)
elim_aux (m, []) f = (m, f)
elim_aux (m, x:xs) f
  | elem2listas m x =  elim_aux (m, xs) f
  | otherwise = elim_aux (m, xs) (f++[x])

-- | elem2listas: elem en dos listas
elem2listas :: Modelo -> Clausula -> Bool
elem2listas _ [] = False
elem2listas y (x:xs)
  | elem x y =  True
  | otherwise = elem2listas y xs


red :: Configuracion -> Configuracion
red conf = red_aux conf []

-- | red_aux: Función auxiliar para hacer recursión 
red_aux :: Configuracion -> Modelo -> Configuracion
red_aux ([], f)  m = (m, f)
red_aux ((x:xs), f) m =  red_aux(xs, (busca_eq x f)) ([x]++m)

-- | busca_eq: busca literales opuestas en las clausulas 
busca_eq :: Literal -> Formula -> Formula
busca_eq _ [] = []
busca_eq l (x:xs) = [(rem_eqAtoms l x [])]++(busca_eq l xs )

-- | rem_eqAtoms: borra las literales iguales y sus opuestos
rem_eqAtoms :: Literal -> Clausula -> Clausula -> Clausula
rem_eqAtoms literal [] salida = salida
rem_eqAtoms literal (x:xs) salida
  | (Neg  literal) == x || literal == (Neg x) =  rem_eqAtoms literal xs salida
  | otherwise = rem_eqAtoms literal xs salida++[x]


split :: Configuracion -> [Configuracion]
split (m,[]) = [(m, [])]
split (m, f) = [(m++a, f), (m++b, f)]
  where
    a = get_lit f
    b = opuesto(get_lit f)

-- | get_lit: Función auxiliar, obtiene la primer literal 
get_lit :: Formula -> Modelo
get_lit (x:xs)
  | length x == 1 = [head x]
  | otherwise = [head x]

-- | opuesto: regresa la literal opuesta
opuesto :: [Literal] -> Modelo
opuesto [(Neg x)] = [x]
opuesto [x] = [(Neg x)]


conflict :: Configuracion -> Bool
conflict (modelo, formula) = formula == [[]]

success :: Configuracion -> Bool
success (modelo, formula) = length(formula) == 0

----------------------------------------------- ARBOLES DPLL -------------------------------------

rcu :: Formula -> Formula
rcu [] = []
rcu (x:xs) = quita_vacias (rcu_aux xs x)

-- | rcu_aux: Función auxiliar para hacer recursión
rcu_aux :: Formula -> Clausula -> Formula
rcu_aux [] [] = []
rcu_aux f [] = f
rcu_aux f (l:c) = [get_clasula l c] ++ rcu_auxLit f l

-- | rcu_auxLit: Ayuda a rcu_aux con las literales
rcu_auxLit :: Formula -> Literal -> Formula
rcu_auxLit [] l = []
rcu_auxLit (x:xs) l = [get_clasula l x] ++ rcu_auxLit xs l

-- | get_Clausula: obtiene las clausulas que tengan las literales o
--                las contrarias
get_clasula :: Literal -> Clausula -> Clausula
get_clasula l [] = []
get_clasula l (x:xs)
  | l == x || (Neg l) == x = get_clasula l xs
  | otherwise              = [x] ++ get_clasula l xs

-- | quita_vacias: quita clausulas vacias, llamar solo en rcu.
quita_vacias :: Formula -> Formula
quita_vacias (x:[]) = [x]
quita_vacias (x:xs)
  | x /= []   = [x]++quita_vacias xs
  | otherwise = quita_vacias xs

rlp :: Formula -> Formula
rlp f = 
  case literalPura f of 
    Nothing -> f
    Just p  -> rlp_aux p f
    
rlp_aux :: Literal -> Formula -> Formula
rlp_aux p f = filter (p `notElem`) f

literalPura :: Formula -> Maybe Literal
literalPura formula = literalPura' (concat formula) formula

literalPura' :: Clausula -> Formula -> Maybe Literal
literalPura' [] f           = Nothing
literalPura' (x:xs) formula
 | esPura x formula         = Just x
 | otherwise                = literalPura' xs formula
 
interseccion :: Literal -> Clausula -> Bool
interseccion l [] = False
interseccion l (x:xs)
  | l == x || Neg l == x = True
  | otherwise            = interseccion l xs

esPura :: Literal -> Formula -> Bool 
esPura p formula =  p `notElem` (concat formula)

opuestoLit :: Literal -> Literal 
opuestoLit (Neg p) = p
opuestoLit  p      = Neg p

--
rd :: Formula -> (Formula,Formula)
rd [] = ([], [])
rd f = ((quita_vacias s1)++r, (quita_vacias s2)++r)
  where
    l = (get_ClausF f)
    a = getA  l f
    b = getB  l f
    r = filter (\f -> f `notElem` b)b ++ filter (\f -> f `notElem` a) a
    s1 = quita l a
    s2 =  quita (opuesto_Lit l) b
  
opuesto_Lit :: Literal -> Literal
opuesto_Lit (Neg x) = x
opuesto_Lit x = (Neg x)

quita :: Literal -> Formula -> Formula
quita x [] = [[]]
quita l (x:xs) = (quita' l [x]) ++ (quita l xs)

quita' :: Literal -> [Clausula] -> Formula
quita' l [] = []
quita' l (x:xs) = (quita2 l x) ++ (quita' l xs)

quita2 :: Literal -> Clausula -> Formula
quita2 l [] = []
quita2 l (x:xs) = (quita3 l x) ++ (quita2 l xs)

quita3 :: Literal -> Literal -> Formula
quita3 l x
  | x == l = [[]]
  | otherwise = [[x]]

quita_opuesto :: Literal -> Formula -> Formula
quita_opuesto x [] = [[x]]
quita_opuesto (Neg l) f = quita l f
quita_opuesto l f = quita (Neg l) f

get_ClausF :: Formula -> Literal
get_ClausF (x:xs) = head x

getA :: Literal -> Formula -> Formula
getA l [] = [[]]
getA l (x:xs)
  | contiene_literal x l = [x]++(getA l xs)
  | otherwise = getA l xs
  
getB :: Literal -> Formula -> Formula
getB l [] = [[]]
getB l (x:xs)
  | contiene_literal_neg x l = [x]++(getB l xs)
  | otherwise = getB l xs



contiene_literal :: Clausula -> Literal -> Bool
contiene_literal [] l = False
contiene_literal (x:xs) l
  |  l==  x   = True
  | otherwise = contiene_literal xs l

contiene_literal_neg :: Clausula -> Literal -> Bool
contiene_literal_neg [] l = False
contiene_literal_neg (x:xs) l
  |  (Neg l) == x || (Neg x) == l=  True
  | otherwise     = contiene_literal_neg xs l



data ArbolDPLL = 
                 Void
               | Uni Formula ArbolDPLL
               | Bi Formula ArbolDPLL ArbolDPLL
               deriving (Eq, Show)

dplltree :: Formula -> ArbolDPLL
dplltree [] = Void


p = P 'p'
q = P 'q'
r = P 'r'
s = P 's'

ejemplo_unit = unit ([], [[p, q],[p], [Neg r], [r, q]])
ejemplo_elim = elim ([p], [[p,q], [r], [r, Neg p], [r,s,p]])
ejemplo_red = red ([p], [[r], [q, r, Neg p], [r, s, q]])
ejemplo_split = split ([], [[r], [q, r, Neg p], [r, s, q]])
ejemplo_conflict = conflict ([p, r, Neg q], [[]])
ejemplo_succes = success ([p, r, Neg q], [])


ejemplo_rcu = rcu [[p, q, Neg r] , [p, Neg q] , [Neg p] , [r]]
ejemplo_rlp = rlp [[p,q] , [q, r] , [Neg q, p] , [r, Neg q]]
ejemplo_rd = rd [[Neg q, r] , [Neg r, q] , [Neg q, Neg r]]
