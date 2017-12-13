module Man where
import Test.QuickCheck

--Shows how many men a player has on the hand
data HandMan = Empty | Add Man HandMan deriving (Show, Eq)

--A piece is either Black, White or Nothing(Blank)
data Man = Black | White | Blank deriving (Show, Eq)

instance Arbitrary Man where
  arbitrary = oneof [ return Black, return White]

--A position has a Y and X cordinate
type Pos = (Int,Int)

--A morris board consists of a list of lists with men in it
newtype Morris = Morris {listPair :: [[Maybe Man]]}
  deriving (Show, Eq)
