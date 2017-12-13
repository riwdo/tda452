module RunGame where
import Man

data Interface = Interface
  { iEmptyBoard :: Morris
  , iBlanks     :: Morris -> [(Int,Int)]
  , iMill       :: Morris -> (Int,Int) ->  Man -> [Bool]
  , iCheckPos   :: (Int,Int) -> Maybe Man
  , iMans       :: Morris -> Maybe Man -> [(Int,Int)]
  , iUpdateBoard:: Morris -> Maybe Man -> Pos -> Morris
  , iRemoveMan  :: Morris -> Pos -> Morris
  , iMoveMan    :: Morris -> Pos -> Pos -> Morris
  }

runGame :: Interface -> IO ()
runGame i =
    do putStrLn "Welcome to the game."
