module BlackJack where
import Cards
import RunGame

import Test.QuickCheck


-- Implement functions for "empty", "value", "gameOver", and "winner"
hand1 = Add (Card Ace Hearts) empty
hand2 = Add (Card Ace Hearts) (Add (Card Ace Hearts) empty)
hand5 = Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 9) Hearts) (Add (Card (Numeric 10) Hearts) empty))
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


valueCard :: Card -> Integer
valueCard card = valueRank (rank card)
--valueCard _ = 0
--valueCard a = valueRank (rank a)

addAces :: Hand -> Integer
addAces Empty = 0
addAces (Add card hand) = if((rank card) == Ace) then 1 + addAces hand else 0 + addAces hand

value :: Hand -> Integer
value Empty = 0
value (Add card hand) = valueCard card + value hand

getValue :: Hand -> Integer
getValue (Add card hand) | (value (Add card hand) > 21 && addAces (Add card hand) > 0) = ((value (Add card hand)) - (addAces (Add card hand) * 10))
                         | otherwise = value (Add card hand)

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner guest bank = if((value guest) > (value bank)) then Guest else Bank
