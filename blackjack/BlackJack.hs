-----------------Lab 2, Part A-----------------------
------------------Task 3.1 Document------------------
--We have read the document.


------------------Task 3.2 Size----------------------
--size hand2
-- = size (Add (Card (Numeric 2) Hearts)
--              (Add (Card Jack Spades) Empty))
-- = ...
-- = 2

-- first the function recognizes that (Add (card (numeric 2) Hearts) is a hand. And that (card (numeric 2) Hearts) is a card. Therefor it ads +1 to the hand size.

-- Then it continutes with (Add (Card Jack Spades) and recognises that it is another card and therefor it adds +1 again to the hand size for a 1+1 so far.

-- Finally it comes to Empty) and Empty is valued 0. So it finishes up and adds +0 to the hand size. Giving us 1+1+0=2 for hand size and returns it.


--------------------Part 3.4-------------------------------
module BlackJack where
import Cards
import RunGame

import Test.QuickCheck



-- Implement functions for "empty", "value", "gameOver", and "winner"
hand1 = Add (Card Ace Hearts) empty
hand2 = Add (Card Ace Hearts) (Add (Card Ace Hearts) empty)
hand3 = Add (Card Jack Hearts) empty
hand4 = Add (Card Jack Hearts) empty
hand5 = Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 9) Hearts) (Add (Card (Numeric 10) Hearts) empty))
hand6 = Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 9) Hearts) (Add (Card (Numeric 10) Hearts) (Add (Card Ace Hearts) empty)))

-- function for emptying hand

empty :: Hand
empty = Empty

-- valueRank assigning values to the rank of the card. No implementation for aces so far
-- Face cards have a value of 10. Aces have 11 so far, and numeric cards have their number as value
valueRank :: Rank -> Integer
valueRank (Numeric i) = i
valueRank Ace = 11
valueRank _ = 10




-- valueCard relies on valueRank to give values to each card

valueCard :: Card -> Integer
valueCard card = valueRank (rank card)


addAces :: Hand -> Integer
addAces Empty = 0
addAces (Add card hand) = if((rank card) == Ace) then 1 + addAces hand else 0 + addAces hand


-- if the hand is empty the value is 0 otherwise it itterates through all cards in the hand and adds their value to the value of the hand
--

getValue :: Hand -> Integer
getValue Empty = 0
getValue (Add card hand) = valueCard card + getValue hand


value :: Hand -> Integer
value (Add card hand) | (getValue (Add card hand) > 21 && addAces (Add card hand) > 0) = ((getValue (Add card hand)) - (addAces (Add card hand) * 10))
                         | otherwise = getValue (Add card hand)


-- if the hand is over 21 the game is over. Relies on value function.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- winner decided with help from value if the bank or the guest has won.
winner :: Hand -> Hand -> Player
winner guest bank = if((value guest) > (value bank)) then Guest else Bank



-----------------------Lab2B------------------------------
