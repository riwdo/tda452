import Data.Maybe
-- Define Morris board

data HandMan = Empty | Add Man HandMan deriving (Show, Eq)

data Man = Black | White deriving (Show, Eq)

type Pos = (Int, Int)

newtype Morris = Morris {rows :: [[Maybe Man]]}
  deriving (Show, Eq)

startingMorris = Morris [[n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]
                        ,[Just w,n,Just w,n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]]
                        where n = Nothing
                              w = White
                              b = Black

blanks :: Morris -> [(Int,Int)]
blanks (Morris board) = [(x,col) | (x,row) <- zip [0..8] board
                          , col <- blanks' row]
                          where blanks' row  = [col | (col,value) <- zip [0..8] row, isNothing value]

--mill :: Morris -> Man -> Bool
--mill (Morris board) player | player == Black =
--                           | otherwise =

mans :: Morris -> Maybe Man -> [(Int,Int)]
mans (Morris board) man = [(x,col) | (x,row) <- zip [0..8] board
                         , col <- blanks' row]
                         where blanks' row  = [col | (col,value) <- zip [0..8] row, isNothing value == False, value == (man)]
