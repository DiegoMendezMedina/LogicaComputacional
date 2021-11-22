module DPLL where

-- Importen los módulos necesarios de las prácticas anteriores para tener sus definiciones
-- de lógica proposicional

type Literal = Prop
type Clausula = [Literal]
type Formula = [Clausula]
type Modelo = [Literal]
type Configuracion = (Modelo,Formula)

--------------------------------------  BÚSQUEDA DE MÓDELOS HACIA ATRAS ------------------------------

unit :: Configuracion -> Configuracion
unit _ = error "Implementar :)"

elim :: Configuracion -> Configuracion
elim _ = error "Implementar :)"

red :: Configuracion -> Configuracion
red _ = error "Implementar :)"

split :: Configuracion -> [Configuracion]
split _ = error "Implementar :)"

conflict :: Configuracion -> Bool
conflict _ = error "Implementar :)"

success :: Configuracion -> Bool
success _ = error "Implementar :)"

----------------------------------------------- ARBOLES DPLL -------------------------------------

rcu :: Formula -> Formula
rcu _ = error "Implementar :)"

rlp :: Formula -> Formula
rlp _ = error "Implementar :)"

rd :: Formula -> (Formula,Formula)
rd _ = error "Implementar :)"

data ArbolDPLL = 
                 Void
               | Uni Formula ArbolDPLL
               | Bi Formula ArbolDPLL ArbolDPLL
               deriving (Eq, Show)

dplltree :: Formula -> ArbolDPLL
dplltree _ = error "Implementar :)"
