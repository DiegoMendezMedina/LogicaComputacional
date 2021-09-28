{-
-- | Lógica Computacional 2022-01
-- | Práctica 1: Introducción a Haskell 
-- | Profesor: Dr. Favio E. Miranda Perea
-- | Ayudante: Javier Enríquez Mendoza
-- | Ayudante: Ramón Arenas Ayala
-- | Laboratorio: Daniel Lugo Cano
-}

module Practica1 where 

-- | Tipo de dato Binario, es la representacion de todos los numero binarios que empiezan con uno
data Binario = U | Cero Binario | Uno Binario 

-- |1| Definicion de la clase Show para el tipo de dato Binario
instance Show Binario where
  show U = "1"
  show (Uno bin) = show bin ++ "1"
  show (Cero bin) =  show bin ++ "0"
  
-- |2| sucesor. Regresa el sucesor de un Binario
-- -> Ejemplo sucesor de U (uno)  = Cero U , sucesor de 1 es 10
sucesor  :: Binario -> Binario
sucesor U = Cero U
sucesor (Cero U) = Uno U
sucesor (Uno U) = Cero(Cero U)
sucesor (Cero bin) = Uno bin
sucesor (Uno bin) = Cero (sucesor bin)

-- |3| suma. Regresa la suma de 2 numeros de un Binarios
-- -> ejemplo suma de U U = Cero U , suma de 1 y 1 es 10
suma :: Binario -> Binario -> Binario
suma = error "Implementar"

-- |4| producto. Regresa el producto de 2 numeros Binarios
-- -> Ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)) , producto de 10 con 10 es 100
producto :: Binario -> Binario -> Binario
producto = error "Implementar"

-- |5| natBinLista. Regresa el la representacion en Binario de un numero Decimal en forma de Lista
-- -> ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista = error "Implementar"
                

-- |6| sumaBinLista. Regresa la suma de 2 listas que representan 2 numeros binarios.
-- -> ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista = error "Implementar"

{-- PUNTOS EXTRA --}

-- |1| natABin: Recibe un natural (mayor que 1) y devuelve un binario
natABin :: Int -> Binario
natABin = error "Solo numeros mayores que cero"

-- |2| binANat: Recibe un binario y devuelve su reprentacion en numero natural (mayor que 1).
binANat :: Binario -> Int
binANat = error "Implementar"

-- |3| predecesor: Recibe un binario y devuelve su binario anterior
predecesor :: Binario -> Binario
predecesor = error "Implementar"


-- | Variables pa probar:
ocho = Cero (Cero(Cero U))
dos = Cero U
once = Uno (Uno (Cero U))
nueve = sucesor ocho
doce = sucesor once
