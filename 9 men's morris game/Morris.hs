import Data.Maybe
import Data.Map.Strict
-- Define Morris board

data HandMan = Empty | Add Man HandMan deriving (Show, Eq)

data Man = Black | White deriving (Show, Eq)


newtype Morris = Morris {listPair :: [[Maybe Man]]}
  deriving (Show, Eq)

adjacentElements = fromList [((0,0),[(0,1),(6,0)]),((0,1),[(0,0),(0,2),(1,1)]),((0,2),[(0,1),(3,5)]),((1,0),[(1,1),(3,1)])
                            ,((1,1),[(0,1),(2,1)]),((1,2),[(1,1),(3,4)]),((2,0),[(2,1),(3,2)]),((2,1),[(2,0),(1,1),(2,2)])
                            ,((2,2),[(2,1),(3,3)]),((3,0),[(0,0),(3,1),(6,0)]),((3,1),[(3,0),(3,2)]),((3,2),[(2,0),(4,0)])
                            ,((3,3),[(2,2),(3,4),(4,2)]),((3,4),[(3,3),(3,5)]),((3,5),[(0,2),(3,4),(6,2)]),((4,0),[(3,2),(4,1)])
                            ,((4,1),[(4,0),(4,2)]),((4,2),[(4,1),(3,3)]),((5,0),[(3,1),(5,1)]),((5,1),[(5,0),(4,1),(5,2)])
                            ,((5,2),[(5,1),(3,4)]),((6,0),[(3,0),(6,1)]),((6,1),[(6,0),(5,1),(6,2)]),((6,2),[(6,1),(3,5)])]

--startingMorris = Morris [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2),()]


--startingMorris = Morris [([1,2,9,21],n),([0,3,4,7],n),([0,1,14,23],n),([4,5,10,18],n),([3,5,1,7],n),([3,4,13,20],n)
--                        ,([7,8,11,15],n),([6,8,4,1],n),([6,7,13,20],n),([0,21,10,11],n),([9,11,3,18],n),([9,10,6,15],n)
--                        ,([13,14,8,17],n),([12,14,5,20],n),([12,13,2,23],n),([])]

startingMorris = Morris [[Just w,n,n]
                        ,[n,n,n]
                        ,[n,n,n]
                        ,[Just w,n,Just w,n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]]
                        where n = Nothing
                              w = White
                              b = Black

-- Get all blank positions
blanks :: Morris -> [(Int,Int)]
blanks (Morris board) = [(y,col) | (y,row) <- zip [0..8] board
                          , col <- blanks' row]
                          where blanks' row  = [col | (col,value) <- zip [0..8] row, isNothing value]

-- Check if each player has formed a mill and returns a bool if that's the case.
--mill :: Morris -> Man -> Bool
--mill (Morris board) player | player == Black =
--                           | otherwise =



type Pos = (Int,Int)

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

getRow :: Morris -> Int -> [Maybe Man]
getRow (Morris board) rowNbr = head [row | (i, row) <- zip [0..] board, i == rowNbr]

possibleMovePhaseTwo :: Morris -> Pos -> [Pos]
possibleMovePhaseTwo (Morris board) (y,x) | y < 3 = getRowMoves
                                  | y > 3 = getRowMoves
                                  | otherwise = getRowMoves
                                    where getRowMoves = [(y,i) | (i, value) <- zip [0..] (getRow (Morris board) y), i == (x-1) || i == (x+1), isNothing value]

-- If a player only has three men left they are allowed to "fly" e.g move to any other point on the board.
-- possibleMovePhaseThree

-- after each player's turn, show the current baord
--showBoard

--showBoard :: Morris -> IO ()
--showBoard (Morris board) = putStrLn (formatMorris board)
