--Hasta este punto el código fue tomado del repositorio en la rama diego
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
  show (Uno bin)  = show bin ++ "1"
  show (Cero bin) = show bin ++ "0"
  
-- | Implementacion de Eq solo para ver si nuestras funciones estan bien
instance Eq Binario where
  (==) bin bim = aux_eq bin bim

-- | aux_eq: Funcion auxiliar para determinar si los dos Binarios recibidos
--           son iguales o no.
aux_eq :: Binario -> Binario -> Bool
aux_eq U U = True
aux_eq (Cero bin) (Cero bim) = aux_eq bin bim
aux_eq (Uno bin)  (Uno bim)  = aux_eq bin bim
aux_eq _ _ = False

-- |2| sucesor. Regresa el sucesor de un Binario
-- -> Ejemplo sucesor de U (uno)  = Cero U , sucesor de 1 es 10
sucesor  :: Binario -> Binario
sucesor U = Cero U
sucesor (Cero bin) = Uno bin
sucesor (Uno bin)  = Cero (sucesor bin)

-- |3| suma. Regresa la suma de 2 numeros de un Binarios
-- -> ejemplo suma de U U = Cero U , suma de 1 y 1 es 10
suma :: Binario -> Binario -> Binario
suma U U   = sucesor U
suma x bin = aux_suma (binANat x) bin

-- | aux_suma: Recibe un Natural y un Binario, regresa la suma
--             haciendo n sucesores al Binario recibido.
--             donde n es el primer número recibido
--           Funcion auxiliar para suma.
aux_suma :: Int -> Binario -> Binario
aux_suma 1 bin = sucesor bin
aux_suma x bin = sucesor(aux_suma (x-1) bin)

-- | peano_sum: Recibe dos Binarios y los suma siguiendo la suma definida
--              por Peano y utilizada en la llamada aritmética de Peano.
--             Funcion extra solo para ver si tambien queda.
--             Termino siendo auxiliar de producto.
suma_peano :: Binario -> Binario -> Binario
suma_peano x U = sucesor x
suma_peano x y = sucesor (suma_peano x (predecesor y))

-- |4| producto. Regresa el producto de 2 numeros Binarios
-- -> Ejemplo producto de (Cero U) (Cero U) = (Cero (Cero U)) , producto de 10 con 10 es 100
-- Producto de aritmetica de peano de una.
producto :: Binario -> Binario -> Binario
producto bin U = bin
producto x y   = suma_peano (producto (x) (predecesor y)) x

-- |5| natBinLista. Regresa el la representacion en Binario de un numero Decimal en forma de Lista
-- -> ejemplo natBinLista 8 = [1,0,0,0]
natBinLista :: Int -> [Int]
natBinLista 1 = [1]
natBinLista 0 = [0]
natBinLista x
  | mod x 2 == 0   =  (natBinLista (div x 2)) ++ [0]
  | otherwise      =  (natBinLista (div x 2)) ++ [1]

-- |6| sumaBinLista. Regresa la suma de 2 listas que representan 2 numeros binarios.
-- -> ejemplo sumaBinLista de [1] [1,0] = (Uno U)
sumaBinLista :: [Int] -> [Int] -> Binario
sumaBinLista x y = suma_peano (natListaABin(reversa x))
                                (natListaABin(reversa y))

-- | natListaABin: Recibe una lista de unos y ceros y regresa su representación
--                 en Binario. Lista volteada, como el constructor de Binario.
--                Funcion auxiliar para sumaBinLista.
--  -> ejemplo natListaABin [0, 0, 0, 1] = Cero(Cero(Cero U))
natListaABin :: [Int] -> Binario
natListaABin [0] = error "El contructor es U"
natListaABin [1] = U
natListaABin (x:xs)
  | x == 0 = Cero (natListaABin xs)
  | x == 1 = Uno  (natListaABin xs)
  | otherwise = error "Solo recibe unos y ceros"

-- | reversa: Recibe una lista y regresa está volteada.
--          Función auxiliar para natListaABin.
reversa :: [Int] -> [Int]
reversa [] = []
reversa (x:xs) = (reversa xs) ++ [x]

{-- PUNTOS EXTRA --}

-- |1| natABin: Recibe un natural (mayor que 1) y devuelve un binario
natABin :: Int -> Binario
natABin 1 = U
natABin x
  | x <= 0    =  error "Solo numeros mayores que cero"
  | otherwise =  a b
  where
    a
      | mod x 2 == 0 = Cero
      | otherwise    = Uno
    b = natABin (div x 2)

-- |2| binANat: Recibe un binario y devuelve su reprentacion en numero natural (mayor que 1).
binANat :: Binario -> Int
binANat bin = auxbinANat 0 bin

-- | auxbinANat: Recibe un Natural y un Binario, calcula su valor
--               sabiendo que un número binario es la representación
--               de la suma de potencias de dos.
--              Funcion auxiliar para natABin.
--              Solo utilizar como funcion auxiliar.
auxbinANat :: Int -> Binario -> Int
auxbinANat x U =  potencia_dos x
auxbinANat x (Uno bin)  = (potencia_dos x) + auxbinANat (x+1) bin
auxbinANat x (Cero bin) = auxbinANat (x+1) bin

-- | potencia_dos: Recibe el numero de la potencia de dos que se desea calcular,
--                 devuelve esta. Funcion auxiliar para auxbinANat.
potencia_dos :: Int -> Int
potencia_dos 0 = 1
potencia_dos x = 2 * potencia_dos (x-1)

-- |3| predecesor: Recibe un binario y devuelve su binario anterior
predecesor :: Binario -> Binario
predecesor U   = U
predecesor bin = natABin((binANat bin)-1)


--{ Variables pa probar -}
dos = Cero U
dos' = sucesor(U)
nueve = Uno(Cero(Cero U))
nueve' = sucesor(sucesor(sucesor(sucesor(sucesor(sucesor(sucesor(sucesor(
                                                                    U))))))))
once = Uno(Uno (Cero U))
once' = sucesor(sucesor nueve')

ocho = Cero(Cero(Cero U))
tres = sucesor(sucesor(U))
ocho_lista = [1, 0, 0, 0]
dieciseis_lista = [1, 0, 0, 0 , 0]
dieciseis' = (producto (natListaABin (reversa ocho_lista)) dos)
dieciseis'' = sumaBinLista ocho_lista ocho_lista
dieciseis = Cero(Cero(Cero(Cero U)))
--variables pa probar -Mau
dieciseis'''= natBinLista 16
seis = Cero(Uno U)

{- Pruebas -}
prueba_sucesor = dos == dos' && once == once'    -- sucesor funciona 
prueba_predecesor = U == predecesor(dos)         -- predecesor funciona
                    && predecesor(nueve) == ocho  
prueba_binANat = binANat(once) == binANat(once') -- natABin funciona 
prueba_natABin = tres == (natABin 3) &&
                 ocho == (natABin 8)               -- natABin funciona
prueba_suma = (suma dos dos) == (Cero(Cero U))
prueba_suma_peano :: Bool
prueba_suma_peano =                              -- suma_peano funciona
  (suma_peano (suma_peano dos  U) (sucesor tres))
  == (predecesor ocho)
prueba_sumaBinLista :: Bool
prueba_sumaBinLista = dieciseis == dieciseis' && -- sumaBinLista funciona
                      dieciseis == dieciseis''  
--Hasta este punto el código fue tomado del repositorio en la rama diego
prueba_producto :: Bool
prueba_producto = (producto tres dos) == seis
prueba_natBinLista ::Bool 
prueba_natBinLista = dieciseis_lista == dieciseis'''