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




module BlackJack where
import Cards
import RunGame

import Test.QuickCheck

--------------------Part 3.4, lab 2A-------------------------------




-- arbitrary hands with cards
hand1 = Add (Card Jack Hearts) empty
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
hand3 = Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 9) Hearts) empty)
hand4 = Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 9) Hearts) (Add (Card (Numeric 10) Hearts) empty))

-- function for emptying hand
empty :: Hand
empty = Empty

-- valueRank assigning values to the rank of the card. No implementation for aces so far
-- Face cards have a value of 10. Aces have 11 so far, and numeric cards have their number as value
valueRank :: Rank -> Integer
valueRank Jack = 10
valueRank Queen = 10
valueRank King = 10
valueRank Ace = 11
valueRank (Numeric i) = i

-- valueCard relies on valueRank to give values to each card
valueCard :: Card -> Integer
valueCard card = valueRank (rank card)

-- if the hand is empty the value is 0 otherwise it itterates through all cards in the hand and adds their value to the value of the hand
--
value :: Hand -> Integer
value Empty = 0
value (Add card hand) = valueCard card + value hand

-- if the hand is over 21 the game is over. Relies on value function.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- winner decided with help from value if the bank or the guest has won.
winner :: Hand -> Hand -> Player
winner guest bank = if((value guest) > (value bank)) then Guest else Bank

-----------------------Lab2B------------------------------
