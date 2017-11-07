module BlackJack where
import Cards
import RunGame

import Test.QuickCheck



-- Implement functions for "empty", "value", "gameOver", and "winner"
hand1 = Add (Card Jack Hearts) empty
hand2 = Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 9) Hearts) empty)
hand3 = Add (Card Jack Hearts) empty
hand4 = Add (Card Jack Hearts) empty


empty :: Hand
empty = Empty

valueRank :: Rank -> Integer
valueRank Jack = 10
valueRank Queen = 10
valueRank King = 10
valueRank Ace = 11
valueRank (Numeric i) = i

valueCard :: Hand -> Integer
valueCard (Add card hand) = valueRank (rank card) + valueCard hand
valueCard _ = 0
--valueCard a = valueRank (rank a)

value :: Hand -> Integer
value Empty = 0
value hand = valueCard hand
