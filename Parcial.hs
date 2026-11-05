-- Segundo parcial 2026 - Paradigma Funcional (Haskell)
-- Consignas: README.md
-- Cargar con: ghci Parcial.hs   (y :r para recargar después de editar)

data Clase = Guerrero | Mago | Arquero
  deriving (Show, Eq)

data Personaje = Personaje
  { nombre :: String
  , clase  :: Clase
  , vida   :: Int      -- 0 = fuera de combate, máximo 100
  , nivel  :: Int
  } deriving (Show)

-- Personajes de prueba
ana, beto, caro :: Personaje
ana  = Personaje "Ana"  Guerrero 80 12
beto = Personaje "Beto" Mago      0  5
caro = Personaje "Caro" Arquero  25  9


-- Punto 1: Tipos de datos y pattern matching -------------------------------

data Item = Arma String Int        -- nombre, daño
          | Armadura String Int    -- nombre, defensa
          | Pocion String Int      -- nombre, vida que cura
  deriving (Show)

-- a)
poder :: Item -> Int
poder (Arma _ danio)       = danio
poder (Armadura _ defensa) = defensa
poder (Pocion _ _)         = 0

-- b)
usarPocion :: Item -> Personaje -> Personaje
usarPocion (Pocion _ cura) personaje
  | vida personaje + cura > 100 = personaje { vida = 100 }
  | otherwise                   = personaje { vida = vida personaje + cura }
usarPocion _ personaje = personaje

-- c) 
estado :: Personaje -> String
estado personaje
  | vida personaje <= 0 = "KO"
  | vida personaje < 30 = "herido"
  | otherwise           = "sano"


-- Punto 2: Recursión con pattern matching ----------------------------------

data Mazmorra = Cofre Int | Bifurcacion Mazmorra Mazmorra
  deriving (Show)

-- a)
vidaTotal :: [Personaje] -> Int
vidaTotal [] = 0
vidaTotal (p : resto) = vida p + vidaTotal resto

-- b) 
sobrevivientes :: [Personaje] -> [String]
sobrevivientes [] = []
sobrevivientes (p : resto)
  | vida p > 0 = nombre p : sobrevivientes resto
  | otherwise  = sobrevivientes resto

-- c) 
tesoroTotal :: Mazmorra -> Int
tesoroTotal (Cofre oro) = oro
tesoroTotal (Bifurcacion izq der) = tesoroTotal izq + tesoroTotal der

-- d) 
profundidad :: Mazmorra -> Int
profundidad (Cofre _) = 0
profundidad (Bifurcacion izq der) = 1 + mayor (profundidad izq) (profundidad der)

mayor :: Int -> Int -> Int
mayor a b
  | a >= b    = a
  | otherwise = b


-- Punto 3: Orden superior, composición, aplicación parcial y lambdas -------

-- a) 
cuantosDeClase :: Clase -> [Personaje] -> Int
cuantosDeClase c = length . filter ((== c) . clase)

-- b)
nombresDeVeteranos :: [Personaje] -> [String]
nombresDeVeteranos = map nombre . filter ((>= 10) . nivel)

-- c)
entrenar :: [Personaje -> Personaje] -> Personaje -> Personaje
entrenar = foldr (.) id

-- d) 
cumpleTodos :: [Personaje -> Bool] -> Personaje -> Bool
cumpleTodos requisitos personaje = all ($ personaje) requisitos

-- e) lambda para filter
--    filter (\p -> vida p < 50 && nivel p > 8)