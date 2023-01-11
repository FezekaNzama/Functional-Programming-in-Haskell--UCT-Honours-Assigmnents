--type Heart = String
--type Clubs = String
--type Spades = String
--type Diamonds = String

data Suit = Hearts | Clubs | Spades | Diamonds
                deriving Eq

data Rank = Numeric Int | Jack | Queen | King
                deriving Eq

data Card = NormalCard Rank Suit | Joker
                deriving Eq




showSuit :: Card -> Bool
showSuit (NormalCard _ Spades) = True
showSuit Joker = True
showSuit (NormalCard _ _) = False

countAces :: [Card] -> Int
countAces [] = 0
countAces (n:ns) | showSuit n == True = 1 + countAces ns
                 | otherwise = countAces ns

--countAces xs = length(filter)