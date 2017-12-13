module RunGame where
import Man
import Text.Show

data Interface = Interface
  { iEmptyBoard :: Morris
  , iFullHand   :: Man -> HandMan
  , iPrintBoard :: Morris -> IO ()
  , iBlanks     :: Morris -> [(Int,Int)]
  , iGetAdjacentElements :: Morris -> (Int,Int) -> [(Int,Int)]
  , iCheckPos   :: Morris -> (Int,Int) -> Maybe Man
  , iMill       :: Morris -> (Int,Int) ->  Man -> [Bool]
  , iMans       :: Morris -> Maybe Man -> [(Int,Int)]
  , iUpdateBoard:: Morris -> Maybe Man -> Pos -> Morris
  , iRemoveMan  :: Morris -> Pos -> Morris
  , iMoveMan    :: Morris -> Pos -> Pos -> Morris
  , iGameOver   :: Morris -> (Bool,Maybe Man)
  }

runGame :: Interface -> IO ()
runGame i = do
    putStrLn "Welcome to the Nine men's morris!"
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
    let tempBoard = board2
    board2 <- if or (iMill i tempBoard coordinate (Black))
                then
                showAndRemoveMan i tempBoard (Just White) (iMill i tempBoard coordinate (White))
                else
                do return tempBoard
    putStrLn ("White's turn")
    putStrLn ("Possible coordinates: " ++  show (iBlanks i board2))
    input <- getLine
    let coordinate = (read input :: (Int,Int))
    let board3 = iUpdateBoard i board2 (Just player2) coordinate
    iPrintBoard i board3
    let tempBoard = board3
    board3 <- if or (iMill i board2 coordinate (White))
                then
                showAndRemoveMan i tempBoard (Just Black) (iMill i tempBoard coordinate (Black))
                else
                do return tempBoard
    phaseOne i board3 hand hand2


phaseTwo :: Interface -> Morris -> IO ()
phaseTwo i board = do
  putStrLn ("Phase two: Move men and form mills")
  iPrintBoard i board
  putStrLn ("Black's turn \n choose man to move:" ++ show (iMans i board (Just Black)))
  input <- getLine
  let curPos = (read input :: (Int,Int))
  putStrLn ("Possible Moves: " ++ show (iGetAdjacentElements i board curPos))
  input <- getLine
  let destPos = (read input :: (Int,Int))
  let board2 = iMoveMan i board curPos destPos
  iPrintBoard i board2
  let tempBoard = board2
  board2 <- if or (iMill i board2 destPos (Black))
              then
              showAndRemoveMan i tempBoard (Just White) (iMill i tempBoard destPos (White))
              else
              do return tempBoard
  putStrLn ("White's turn \n choose man to move:" ++ show (iMans i board2 (Just White)))
  input <- getLine
  let curPosWhite = (read input :: (Int,Int))
  putStrLn ("Possible Moves: " ++ show (iGetAdjacentElements i board2 curPos))
  input <- getLine
  let destPosWhite = (read input :: (Int,Int))
  let board3 = iMoveMan i board2 curPosWhite destPosWhite
  iPrintBoard i board3
  let tempBoard = board3
  board3 <- if or (iMill i board2 destPosWhite (White))
              then
              showAndRemoveMan i tempBoard (Just Black) (iMill i tempBoard destPosWhite (Black))
              else
              do return tempBoard
  if fst (iGameOver i board3)
    then
      if snd (iGameOver i board3) == (Just Black)
        then
          putStrLn ("Winner: White")
        else
          putStrLn ("Winner: Black")
      else
        phaseTwo i board3



showAndRemoveMan :: Interface -> Morris -> Maybe Man -> [Bool] -> IO Morris
showAndRemoveMan i board playerToRemove listOfMills = do
  putStrLn ("Board:")
  iPrintBoard i board
  putStrLn ("Choose man to remove:")
  putStrLn (show (iMans i board playerToRemove))
  input <- getLine
  let coordinate = (read input :: (Int,Int))
  let board2 = iRemoveMan i board coordinate
  iPrintBoard i board2
  if and listOfMills then (showAndRemoveMan i board2 (playerToRemove)) [False] else return board
