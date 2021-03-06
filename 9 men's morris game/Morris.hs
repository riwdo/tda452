module Morris where
import Man
import RunGame
import Data.Maybe
import Data.Map.Strict
import Test.QuickCheck

-- Create a map that gives adjacentElements given a key coordinate
adjacentElements = fromList [((0,0),[(0,3),(3,0)]),((0,3),[(0,0),(0,6),(1,3)]),((0,6),[(0,3),(3,6)]),((1,1),[(1,3),(3,1)])
                            ,((1,3),[(0,3),(2,3),(1,1),(1,5)]),((1,5),[(1,3),(3,5)]),((2,2),[(2,3),(3,2)]),((2,3),[(2,2),(1,3),(2,4)])
                            ,((2,4),[(2,3),(3,4)]),((3,0),[(0,0),(3,1),(6,0)]),((3,1),[(3,0),(3,2),(1,1),(5,1)]),((3,2),[(2,2),(4,2),(3,1)])
                            ,((3,4),[(2,4),(3,5),(4,4)]),((3,5),[(3,4),(3,6),(1,5),(5,5)]),((3,6),[(0,6),(3,5),(6,6)]),((4,2),[(3,2),(4,3)])
                            ,((4,3),[(4,2),(4,4),(5,3)]),((4,4),[(4,3),(3,4)]),((5,1),[(3,1),(5,3)]),((5,3),[(5,1),(4,3),(5,5),(6,3)])
                            ,((5,5),[(5,3),(3,5)]),((6,0),[(3,0),(6,3)]),((6,3),[(6,0),(5,3),(6,6)]),((6,6),[(6,3),(3,6)])]
-- translate coorinate to number
coordinateToNumber = fromList [((0,0),1),((0,3),2),((0,6),3),((1,1),4),((1,3),5),((1,5),6),((2,2),7),((2,3),8),((2,4),9),((3,0),10)
                        ,((3,1),11),((3,2),12),((3,4),13),((3,5),14),((3,6),15),((4,2),16),((4,3),17),((4,4),18),((5,1),19),((5,3),20)
                        ,((5,5),21),((6,0),22),((6,3),23),((6,6),24)]
-- translate number to coordinate
numberToCoordinate = fromList [(1,(0,0)),(2,(0,3)),(3,(0,6)),(4,(1,1)),(5,(1,3)),(6,(1,5)),(7,(2,2)),(8,(2,3)),(9,(2,4)),(10,(3,0))
                        ,(11,(3,1)),(12,(3,2)),(13,(3,4)),(14,(3,5)),(15,(3,6)),(16,(4,2)),(17,(4,3)),(18,(4,4)),(19,(5,1)),(20,(5,3))
                        ,(21,(5,5)),(22,(6,0)),(23,(6,3)),(24,(6,6))]

-- diplays the men you can move on the board
menToMove :: Morris -> Man -> [Pos]
menToMove board player = [match coordinate | coordinate <- (mans board (Just player)), length (possibleMoves board coordinate) > 0]
                where match coordinate = head [coordinate | adjCoord <- (adjacentElements ! coordinate), isNothing (checkPos board (adjCoord))]

-- displays possible moves given for a coorinate
possibleMoves :: Morris -> (Int,Int) -> [Int]
possibleMoves board coordinate = [coordinateToNumber ! (adjacentCoord) | adjacentCoord <- (adjacentElements ! coordinate),  isNothing (checkPos board (adjacentCoord))]

-- parses coorinate to nunbers
parseCoordinate :: [(Int,Int)] -> [Int]
parseCoordinate coordinates = [coordinateToNumber ! coordinate | coordinate <- coordinates]

-- parse a number to a coordinate
parseNumber :: Int -> (Int,Int)
parseNumber number = numberToCoordinate ! number

-- Empty board
startingMorris :: Morris
startingMorris = Morris [[n,      Just bl,Just bl,  n, Just bl,Just bl,          n]
                        ,[Just bl,   n,   Just bl,  n, Just bl,   n,       Just bl]
                        ,[Just bl,Just bl,    n,    n,    n,   Just bl,    Just bl]
                        ,[n,         n,       n, Just bl, n,      n,             n]
                        ,[Just bl,Just bl,    n,    n,    n,   Just bl,    Just bl]
                        ,[Just bl,   n,    Just bl, n, Just bl,   n,       Just bl]
                        ,[n,      Just bl, Just bl, n, Just bl,Just bl,         n]]
                        where n = Nothing
                              bl= Blank
                              w = White
                              b = Black

printBoard :: Morris -> String
printBoard board = (formatBoard (listPair board) [1,4,7,10,16,19,22])

formatBoard :: [[Maybe Man]] -> [Int] -> String
formatBoard [] position = ""
formatBoard (x:xs) (h:position) = getElement x h ++  "\n" ++ formatBoard xs position

getElement :: [Maybe Man] -> Int -> String
getElement [] position = ""
getElement (x:xs) position | x == n = (show position) ++ ". _\t"  ++ getElement xs (position+1)
                           | x == (Just Blank) = "\t" ++ getElement xs position
                           | otherwise = (show position) ++ ". " ++ (Prelude.take 1 (show (fromJust x))) ++ "\t"++ getElement xs ((position)+1)
                            where n = Nothing
                                  j = Just
                                  b = Blank


--Fills the players hand with men
fullHand :: Man -> HandMan
fullHand player = fullHand' player 8 (Add player Empty)

--helperfunction for fullHand
fullHand' :: Man -> Int -> HandMan -> HandMan
fullHand' player 0 hand = hand
fullHand' player i hand = fullHand' player (i-1) ((Add player) hand)

menInHand :: HandMan -> Int
menInHand hand = menInHand' hand 0

menInHand' :: HandMan -> Int -> Int
menInHand' Empty nbrOfMen = nbrOfMen
menInHand' (Add player hand) nbrOfMen = menInHand' hand (nbrOfMen+1)

-- Get all blank positions
blanks :: Morris -> [(Int,Int)]
blanks (Morris board) = [(y,col) | (y,row) <- zip [0..8] board
                          , col <- blanks' row]
                          where blanks' row  = [col | (col,value) <- zip [0..8] row, isNothing value]

-- Check if each player has formed a mill and returns a bool if that's the case.
mill :: Morris -> (Int,Int) ->  Man -> [Bool]
mill (board) (y,x) player = [checkMill board False (y,x) (y,x) player] ++ [checkMill board True (y,x) (y,x) player]

--After a piece has been placed on the board this function checks if there exists any three in a row. The function returns how many men the player is allowed to mill
checkMill :: Morris -> Bool -> (Int, Int) -> (Int, Int) -> Man -> Bool
checkMill board vertical (y,x) prevState player | length [value | value <- neighbours, case vertical of True -> x == snd value
                                                                                                        False -> y == (fst value) ] == 1 =
                                        if checkPos board (y,x) == Just player then
                                          checkMill board vertical (head [value | value <- neighbours, case vertical of True -> x == snd value
                                                                                                                        False -> y == fst value]) (y,x) player
                                        else
                                          False
                              | otherwise = if (length [True | element <- list, checkPos board element /= Just player] /= 0) then False else True
                              where (neighbours, list) = ((adjacentElements ! (y,x)), ([(y,x)] ++ [value | value <- neighbours, case vertical of True -> x == snd value
                                                                                                                                                 False -> y == fst value, value /= prevState]))
--Checks the position on the board
checkPos :: Morris -> (Int,Int) -> Maybe Man
checkPos board (yIn,xIn) = head ([checkRow list y yIn xIn | (y,list) <- zip [0..] (listPair board)] !! yIn)
    where checkRow list y yIn xIn = [man | (x,man) <- zip [0..] list,yIn == y,xIn ==x]



-- Given a board and a Man returns the positions of that player's mans
mans :: Morris -> Maybe Man -> [(Int,Int)]
mans (Morris board) man = [(y,col) | (y,row) <- zip [0..8] board
                         , col <- blanks' row]
                         where blanks' row  = [col | (col,value) <- zip [0..8] row, isNothing value == False, value == (man)]

-- takes a list and a index,value pair and uses list comprehension to create a new list with the new value at that position
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) list (index, newValue) =  [if i == index then newValue else a | (i, a) <- zip [0..] list]

  -- updates a given Morris board with a new man at given position
updateBoard :: Morris -> Maybe Man -> Pos -> Morris
updateBoard (Morris board) newValue (yIN,xIN) = Morris [if y == yIN then (row !!= (xIN, newValue)) else row | (y,row) <- zip [0..] board]

--Function to remove a man from the board
removeMan :: Morris -> Pos -> Morris
removeMan board (y,x) = updateBoard board Nothing (y,x)

--Function to move a man on the board
moveMan :: Morris -> Pos -> Pos -> Morris
moveMan board currentPos newPos = removeMan (updateBoard board (checkPos board currentPos) newPos) currentPos

--If a player has fewer than 3 pieces on the board the player is elimanted from the game
gameOver :: Morris -> (Bool,Maybe Man)
gameOver board | length (mans board w) < 3 = (True, w)
               | length (mans board b) < 3 = (True, b)
               | otherwise = (False, Nothing)
               where (w,b) = (Just White, Just Black)

implementation = Interface
    { iEmptyBoard = startingMorris
    , iFullHand   = fullHand
    , iPrintBoard = printBoard
    , iBlanks     = blanks
    , iParseNumber = parseNumber
    , iparseCoordinate = parseCoordinate
    , iPossibleMoves = possibleMoves
    , iMenInHand  = menInHand
    , iMill       = mill
    , iCheckPos   = checkPos
    , iMenToMove  = menToMove
    , iMans       = mans
    , iUpdateBoard= updateBoard
    , iRemoveMan  = removeMan
    , iMoveMan    = moveMan
    , iGameOver   = gameOver
    }

main :: IO ()
main = runGame implementation

----------------------------------Properties-----------------------------------------------
--Generates arbitrary cells for the morris board with different board pieces, not properly weighted
cell :: Gen (Maybe Man)
cell = frequency [(1,return (Just Black)), (1, return (Just White))]

-- Genereates an arbitrary morris board and fills it with board pieces using cell
instance Arbitrary Morris where
  arbitrary =
    do rows <- vectorOf 7 (vectorOf 7 cell)
       return (Morris rows)

--checks if a man has moved around. If quickCheck checks with a negative value
--or a value bigger than the board it will return true as we dont know how to handle expected exceptions
prop_moveMan :: Morris -> (Int,Int) -> Maybe Man -> Bool
prop_moveMan m (y,x) man | y > 6  || x > 6 || y < 0 || x < 0 = True
                         | listPair m == [] = True
                         | checkPos (updateBoard m man (y,x)) (y,x) == man = True
                         | otherwise = False


--checks if a man has been removed around. If quickCheck checks with a negative value
--or a value bigger than the board it will return true as we dont know how to handle expected exceptions
prop_removeMan :: Morris -> Pos -> Bool
prop_removeMan m (y,x) | y > 6  || x > 6 || y < 0 || x < 0 = True
                         | listPair m == [] = True
                         | checkPos (removeMan m (y,x)) (y,x) == Nothing = True
                         | otherwise = False
