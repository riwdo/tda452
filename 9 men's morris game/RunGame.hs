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
    phaseOne i (iEmptyBoard i) (iFullHand i Black) (iFullHand i White)

phaseOne :: Interface -> Morris -> HandMan -> HandMan -> IO ()
phaseOne i board Empty Empty = phaseTwo i board
phaseOne i board (Add player1 hand) (Add player2 hand2) = do
    putStrLn ("Black: " ++ show (9-length (iMans i board (Just Black))) ++ "\nWhite: " ++ show (9-length (iMans i board (Just White))))
    iPrintBoard i board
    putStrLn ("Black's turn")
    putStrLn ("Possible coordinates: " ++  show (iBlanks i board))
    input <- getLine
    let coordinate = (read input :: (Int,Int))
    let board2 = iUpdateBoard i board (Just player1) coordinate
    iPrintBoard i board2
    if or (iMill i board2 coordinate player1) then showAndRemoveMan i board2 (Just player2) (iMill i board2 coordinate player1) else putStrLn ("")
    putStrLn ("White's turn")
    putStrLn ("Possible coordinates: " ++  show (iBlanks i board2))
    input <- getLine
    let coordinate = (read input :: (Int,Int))
    let board3 = iUpdateBoard i board2 (Just player2) coordinate
    iPrintBoard i board3
    if or (iMill i board3 coordinate player2) then showAndRemoveMan i board3 (Just player1) (iMill i board3 coordinate player2) else putStrLn ("")
    phaseOne i board3 hand hand2



phaseTwo :: Interface -> Morris -> IO ()
phaseTwo i board = do
  putStrLn ("Welcome to phase two")

showAndRemoveMan :: Interface -> Morris -> Maybe Man -> [Bool] -> IO ()
showAndRemoveMan i board playerToRemove listOfMills = do
  putStrLn ("Board:")
  iPrintBoard i board
  putStrLn ("Choose man to remove:")
  putStrLn (show (iMans i board playerToRemove))
  input <- getLine
  let coordinate = (read input :: (Int,Int))
  let board2 = iRemoveMan i board coordinate
  iPrintBoard i board2
  if and listOfMills then (showAndRemoveMan i board2 (playerToRemove)) [False] else putStrLn ("Next Player")
