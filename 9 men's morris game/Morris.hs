import Data.Maybe
-- Define Morris board

data HandMan = Empty | Add Man HandMan deriving (Show, Eq)

data Man = Man deriving (Show, Eq)

type Pos = (Int, Int)

newtype Morris = Morris {rows :: [[Maybe Man]]}
  deriving (Show, Eq)

startingMorris = Morris [[n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]
                        ,[Just m,n,Just m,n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]
                        ,[n,n,n]]
                        where n = Nothing
                              m = Man

blanks :: Morris -> [(Int,Int)]
blanks (Morris board) = [(x,col) | (x,row) <- zip [0..8] board
                          , col <- blanks' row]
                          where blanks' row  = [col | (col,value) <- zip [0..8] row, isNothing value]
