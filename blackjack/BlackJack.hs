module BlackJack where
import Cards
import RunGame

import Test.QuickCheck



-- Implement functions for "empty", "value", "gameOver", and "winner"

empty :: Hand
empty = Empty

valueRank :: Rank -> Integer
valueRank Jack = 10
valueRank Queen = 10
valueRank King = 10
valueRank Ace = 11
valueRank Numeric i = i

valueCard :: Card -> Integer


value :: Hand -> Integer
