module RunGame where
import Man

data Interface = Interface
  { iEmptyBoard :: Morris
  , iBlanks     :: Morris -> [(Int,Int)]
  , iMill       :: Morris -> (Int,Int) ->  Man -> [Bool]
  , iCheckPos   :: (Int,Int) -> Maybe Man
  , iMans       :: Morris -> Maybe Man -> [(Int,Int)]
  , iUpdateBoard:: Morris -> Maybe Man -> Pos -> Morris
  , iPlayBank :: Hand -> Hand
  , iShuffle  :: StdGen -> Hand -> Hand
  }
