module Man where
import Test.QuickCheck

data HandMan = Empty | Add Man HandMan deriving (Show, Eq)

data Man = Black | White | Blank deriving (Show, Eq)

instance Arbitrary Man where
  arbitrary = oneof [ return Black, return White]

type Pos = (Int,Int)

newtype Morris = Morris {listPair :: [[Maybe Man]]}
  deriving (Show, Eq)
