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
import System.Random
import Test.QuickCheck hiding (shuffle)



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
value Empty = 0
value (Add card hand) | (getValue (Add card hand) > 21 && addAces (Add card hand) > 0) = ((getValue (Add card hand)) - (addAces (Add card hand) * 10))
                         | otherwise = getValue (Add card hand)


-- if the hand is over 21 the game is over. Relies on value function.
gameOver :: Hand -> Bool
gameOver hand = getValue hand > 21

-- winner decided with help from value if the bank or the guest has won.
winner :: Hand -> Hand -> Player
winner guest bank | (gameOver guest) = Bank
                  | (gameOver bank && (gameOver guest) == False) = Guest
                  | (getValue guest) > (getValue bank) = Guest
                  | otherwise = Bank



-----------------------Lab2B-----------------------------
---First if both hands are empty the new hand becomes empty.
---If one of the hands are empty it adds cards from the non empty hand
--- otherwise it puts the first on top of the second
(<+) :: Hand -> Hand -> Hand
(<+) Empty Empty = empty
(<+) Empty (Add card2 hand2) = Add (Card (rank card2) (suit card2)) ((<+) Empty hand2)
(<+) (Add card1 hand1) Empty = empty
(<+) (Add card1 hand1) hand2 = Add (Card (rank card1) (suit card1)) ((<+) hand1 hand2)

--- all the ranks
handOfSuits :: Suit -> Hand
handOfSuits s = Add (Card (Numeric 2) s) (Add (Card (Numeric 3) s) (Add (Card (Numeric 4) s)(Add (Card (Numeric 5) s)
                (Add (Card (Numeric 6) s)(Add (Card (Numeric 7) s)(Add (Card (Numeric 8) s)(Add (Card (Numeric 9) s)
                (Add (Card (Numeric 10) s) (Add (Card Jack s) (Add (Card Queen s) (Add (Card King s) (Add (Card Ace s) empty))))))))))))

---utilises handofsuits and <+ to build a deck with all the different cards
fullDeck :: Hand
fullDeck = handOfSuits Hearts <+ handOfSuits Spades <+ handOfSuits Clubs <+ handOfSuits Diamonds

---associative test
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

---size test
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = (size (p1<+p2)) == ((size p1) + (size p2))

---if the deck is empty it cannot draw a card
---otherwise it draws the top card of the deck adds it to the hand and returns them
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = ((deck),(Add (Card (rank card) (suit card)) hand))

---relies on the playBank' function to play as the Bank
playBank :: Hand -> Hand
playBank deck = playBank' deck empty

--- draws cards for the Bank from the deck if the bank has not yet reached 16 or more of score
playBank' :: Hand -> Hand -> Hand
playBank' deck Empty = playBank' deck' bankHand'
                      where (deck',bankHand') = draw deck Empty
playBank' deck bankHand | (value bankHand > 16) = bankHand
                        | otherwise = playBank' deck' bankHand'
                        where (deck',bankHand') = draw deck bankHand

---shuffles the deck by removing a random card from the deck and adding it to a new deck. Repeating this for each card.
--- Probably not the best solution
shuffle :: StdGen -> Hand -> Hand
shuffle stdgen hand = shuffle' stdgen Empty hand

shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' stdgen newDeck oldDeck | (size newDeck == 52) = newDeck
                                | otherwise = shuffle' stdGen' ((Add (getCard oldDeck 0 cardNumber)) newDeck) (removeCardDeck Empty oldDeck 0 cardNumber)
                                where (cardNumber, stdGen') = randomNumber stdgen ((size oldDeck)-1)

removeCardDeck :: Hand -> Hand -> Integer -> Integer -> Hand
removeCardDeck newHand Empty s r = newHand
removeCardDeck newHand (Add card hand) s r |   (s == r) = removeCardDeck newHand hand (s+1) r
                                           | otherwise = removeCardDeck ((Add (Card (rank card) (suit card))) newHand) (hand) (s+1) r

getCard :: Hand -> Integer -> Integer -> (Card)
getCard (Add card hand) 0 0 = card
getCard (Add card hand) s r |  (s == r) = card
                            | otherwise = getCard hand (s+1) r

randomNumber :: StdGen -> Integer -> (Integer, StdGen)
randomNumber g index = (n1, g1)
  where (n1, g1) = randomR (0, index) g

---test for same cards in the original deck and the shuffled deck
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
              c `belongsTo` h == c `belongsTo` shuffle g h

---help function for prop_shuffle_sameCards that checks if a card belongs in a deck or not
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

implementation = Interface
    { iEmpty    = empty
    , iFullDeck = fullDeck
    , iValue    = value
    , iGameOver = gameOver
    , iWinner   = winner
    , iDraw     = draw
    , iPlayBank = playBank
    , iShuffle  = shuffle
    }

main :: IO ()
main = runGame implementation
