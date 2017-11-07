module BlackJack where
import Cards
import RunGame

import Test.QuickCheck


-- arbitrary hands with cards
hand1 = Add (Card Jack Hearts) empty
hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
hand3 = Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 9) Hearts) empty)
hand4 = Add (Card (Numeric 5) Hearts) (Add (Card (Numeric 9) Hearts) (Add (Card (Numeric 10) Hearts) empty))

-- empty function
empty :: Hand
empty = Empty

-- valueRank assigning values to the rank of the card. No implementation for aces so far
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
value :: Hand -> Integer
value Empty = 0
value (Add card hand) = valueCard card + value hand

-- if the hand is over 21 the game is over. Relies on value function.
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- winner decided with help from value if the bank or the guest has won.
winner :: Hand -> Hand -> Player
winner guest bank = if((value guest) > (value bank)) then Guest else Bank
