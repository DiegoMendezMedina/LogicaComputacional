-- 
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
    show (Cero x)= show x ++ "0"
    show (Uno x)= show x ++ "1"


-- |2| sucesor. Regresa el sucesor de un Binario
-- -> Ejemplo sucesor de U (uno)  = Cero U , sucesor de 1 es 10
sucesor  :: Binario -> Binario
sucesor U = Cero U
sucesor (Cero x) = Uno x
sucesor (Uno x) = Cero (sucesor x) 


-- |3| suma. Regresa la suma de 2 numeros de un Binarios
-- -> ejemplo suma de U U = Cero U , suma de 1 y 1 es 10
suma :: Binario -> Binario -> Binario
suma U U = Cero U
suma = error "implementar"


-- |4| producto. Regresa el producto de 2 numeros Binarios
-- -> Ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)) , producto de 10 con 10 es 100
producto :: Binario -> Binario -> Binario
--producto U U = U
producto = error "Implementar"

-- |5| natBinLista. Regresa el la representacion en Binario de un numero Decimal en forma de Lista
-- -> ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista 1 = [] 
natBinLista n = []
                

-- |6| sumaBinLista. Regresa la suma de 2 listas que representan 2 numeros binarios.
-- -> ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista = error "Implementar"

{-- PUNTOS EXTRA --}

-- |1| natABin: Recibe un natural (mayor que 1) y devuelve un binario
natABin :: Int -> Binario
natABin = error "implementar"
-- |2| binANat: Recibe un binario y devuelve su reprentacion en numero natural (mayor que 1).
binANat :: Binario -> Int
binANat = error "implementar"

-- |3| predecesor: Recibe un binario y devuelve su binario anterior
-- esta funcion solo calcula los predecesores de los numeros que tienen el binario Uno x
predecesor :: Binario -> Binario
predecesor (Cero U) = U
predecesor (Uno x) = Cero x
predecesor (Cero x) = predecesor (Uno x)