import Data.Maybe
-- Define Morris board

data HandMan = Empty | Add Man HandMan deriving (Show, Eq)

data Man = Black | White deriving (Show, Eq)


newtype Morris = Morris {rows :: [[Maybe Man]]}
  deriving (Show, Eq)

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

possibleMove :: Morris -> Pos -> [Pos]
possibleMove (Morris board) (y,x) | y < 3 = getRowMoves
                                  | y > 3 = getRowMoves
                                  | otherwise = getRowMoves
                                    where getRowMoves = [(y,i) | (i, value) <- zip [0..] (getRow (Morris board) y), i == (x-1) || i == (x+1)]
