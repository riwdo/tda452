module BlackJack where
import Cards
import RunGame

import Test.QuickCheck



-- Implement functions for "empty", "value", "gameOver", and "winner"

empty :: Hand
empty = Empty

valueRank :: Rank -> Integer


valueCard :: Card -> Integer

value :: Hand -> Integer
value Hand
