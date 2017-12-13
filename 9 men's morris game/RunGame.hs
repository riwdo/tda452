module RunGame where
import Man
import Text.Show

data Interface = Interface
  { iEmptyBoard :: Morris
  , iFullHand   :: Man -> HandMan
  , iPrintBoard :: Morris -> IO ()
  , iBlanks     :: Morris -> [(Int,Int)]
  , iMill       :: Morris -> (Int,Int) ->  Man -> [Bool]
  , iCheckPos   :: Morris -> (Int,Int) -> Maybe Man
  , iMans       :: Morris -> Maybe Man -> [(Int,Int)]
  , iUpdateBoard:: Morris -> Maybe Man -> Pos -> Morris
  , iRemoveMan  :: Morris -> Pos -> Morris
  , iMoveMan    :: Morris -> Pos -> Pos -> Morris
  }

runGame :: Interface -> IO ()
runGame i = do
    putStrLn "Welcome to the game."
    let startingBoard = iEmptyBoard i
    let blackPlayer = iFullHand i Black
    let whitePlayer = iFullHand i White
    phaseOne i startingBoard blackPlayer whitePlayer

phaseOne :: Interface -> Morris -> HandMan -> HandMan -> IO ()
phaseOne i board Empty Empty = phaseTwo i board
phaseOne i board (Add player1 hand) (Add player2 hand2) = do
    putStrLn ("Player1's turn to place Man")
    putStrLn ("Possible coordinates: " ++  show (iBlanks i board))
    input <- getLine
    let coordinate = (read input :: (Int,Int))
  --  let man = iGetNextMan i player1
    let board2 = iUpdateBoard i board (Just player1) coordinate
    iPrintBoard i board2
    --if or (iMill i board coordinate player1) then remove
    putStrLn (show (or (iMill i board2 coordinate player1)))
    putStrLn ("Player2's turn to place Man")
    putStrLn ("Possible coordinates: " ++  show (iBlanks i board2))
    input <- getLine
    let coordinate = (read input :: (Int,Int))

    let board3 = iUpdateBoard i board2 (Just player2) coordinate
    iPrintBoard i board3
    phaseOne i board3 hand hand2


phaseTwo :: Interface -> Morris -> IO ()
phaseTwo i board = do
  putStrLn ("Welcome to phase two")

--removeMan ::
