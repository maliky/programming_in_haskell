import Data.Char
{-
Regroupe des fonctions de mathématiques
-}

-- check if a char is a digit in base 10
-- isChiffre2 :: Char -> Bool

-- see isDigit from Data.Char


-- définie les chiffres classique des bases 2, 6, 8, 16
-- Il doit y avoir moyen de factoriser tout cela
data Chiffre2 = Zero | One
  deriving (Eq, Ord, Read)

instance Show Chiffre2 where
  show Zero = "0"
  show One = "1"  

type Base2 = [Chiffre2]

data Chiffre6 = Chiffre6 | Two | Three | Four | Five
  deriving (Eq, Ord, Read)

instance Show Chiffre6 where
  show Two = "2"
  show Three = "3"  
  show Four = "4"
  show Five = "5"  

type Base6 = [Chiffre6]


-- data Chiffre8 = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7
-- data Base8 = [Chiffre8]
--   deriving (Eq, Ord, Show, Read)

-- data Chiffre10 = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-- data Base10 = [Chiffre10]
--   deriving (Eq, Ord, Show, Read)

-- data Chiffre16 = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | A | B | C | D | E
-- data Base16 = [Chiffre16]
--   deriving (Eq, Ord, Show, Read)

-- isChiffre2 :: Char -> Bool
-- isChiffre2 x = -- how to convert a char to an Int ?

-- check if a String is a number in base 10
-- check if a String is a number in base b

-- dataType base Définie un type base qui est un ensemble de symbol

-- check if a char is a digit in base b


-- En fait je ne veux pas tout redéfinir, mais réutiliser des fonctions existantes.
-- pour passer d'un type à un autre
