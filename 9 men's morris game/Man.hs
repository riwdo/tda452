module Man where

data HandMan = Empty | Add Man HandMan deriving (Show, Eq)

data Man = Black | White deriving (Show, Eq)

type Pos = (Int,Int)

newtype Morris = Morris {listPair :: [[Maybe Man]]}
  deriving (Show, Eq)
