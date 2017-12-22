module RunGame where
import Man
import Text.Show

--Inteface for the implementation
data Interface = Interface
  { iEmptyBoard :: Morris
  , iFullHand   :: Man -> HandMan
  , iPrintBoard :: Morris -> String
  , iBlanks     :: Morris -> [(Int,Int)]
  , iParseNumber :: Int -> (Int,Int)
  , iparseCoordinate :: [(Int,Int)] -> [Int]
  , iPossibleMoves :: Morris -> (Int,Int) -> [Int]
  , iMenInHand  :: HandMan -> Int
  , iCheckPos   :: Morris -> (Int,Int) -> Maybe Man
  , iMill       :: Morris -> (Int,Int) ->  Man -> [Bool]
  , iMans       :: Morris -> Maybe Man -> [(Int,Int)]
  , iMenToMove  :: Morris -> Man -> [Pos]
  , iUpdateBoard:: Morris -> Maybe Man -> Pos -> Morris
  , iRemoveMan  :: Morris -> Pos -> Morris
  , iMoveMan    :: Morris -> Pos -> Pos -> Morris
  , iGameOver   :: Morris -> (Bool,Maybe Man)
  }

--sets up the game
runGame :: Interface -> IO ()
runGame i = do
    putStrLn "Welcome to the Nine men's morris!"
    phaseOne i (iEmptyBoard i) (iFullHand i Black) (iFullHand i White)


phaseOne :: Interface -> Morris -> HandMan -> HandMan -> IO ()
phaseOne i board Empty Empty = phaseTwo i board
phaseOne i board (Add player1 hand) (Add player2 hand2) = do
    putStrLn ("Black: " ++ show (iMenInHand i (Add player1 hand)) ++ "\nWhite: " ++ show (iMenInHand i (Add player2 hand2)))
    putStrLn (iPrintBoard i board)
    putStrLn ("Black's turn")
    putStrLn ("Possible places: " ++  show (iparseCoordinate i (iBlanks i board)))
    input <- getLine
    let number = (read input :: Int)
    let board2 = iUpdateBoard i board (Just player1) (iParseNumber i number)
    putStrLn (iPrintBoard i board2)
    let tempBoard = board2
    board2  <- if or (iMill i tempBoard (iParseNumber i number) (Black))
                then
                showAndRemoveMan i tempBoard (Just White) (iMill i tempBoard (iParseNumber i number) (White))
                else
                do return tempBoard
    putStrLn ("White's turn")
    putStrLn ("Possible places: " ++  show (iparseCoordinate i (iBlanks i board2)))
    input <- getLine
    let number = (read input :: Int)
    let board3 = iUpdateBoard i board2 (Just player2) (iParseNumber i number)
    putStrLn(iPrintBoard i board3)
    let tempBoard = board3
    board3 <- if or (iMill i tempBoard (iParseNumber i number) (White))
                then
                showAndRemoveMan i tempBoard (Just Black) (iMill i tempBoard (iParseNumber i number) (Black))
                else
                do return tempBoard
    phaseOne i board3 hand hand2


phaseTwo :: Interface -> Morris -> IO ()
phaseTwo i board = do
  putStrLn ("Phase two: Move men and form mills")
  putStrLn (iPrintBoard i board)
  putStrLn ("Black's turn \n choose man to move:" ++ show (iparseCoordinate i (iMenToMove i board Black)))
  input <- getLine
  let curPos = (read input :: Int)
  putStrLn ("Possible Moves: " ++ show  (iPossibleMoves i board (iParseNumber i curPos)))
  input <- getLine
  let destPos = (read input :: Int)
  let board2 = iMoveMan i board (iParseNumber i curPos) (iParseNumber i destPos)
  putStrLn (iPrintBoard i board2)
  let tempBoard = board2
  board2 <- if or (iMill i tempBoard (iParseNumber i destPos) (Black))
              then
              showAndRemoveMan i tempBoard (Just White) (iMill i tempBoard (iParseNumber i destPos) (White))
              else
              do return tempBoard
  putStrLn ("White's turn \n choose man to move:" ++ show (iparseCoordinate i (iMenToMove i board2 White)))
  input <- getLine
  let curPosWhite = (read input :: Int)
  putStrLn ("Possible Moves: " ++ show  (iPossibleMoves i board2 (iParseNumber i curPosWhite)))
  input <- getLine
  let destPosWhite = (read input :: Int)
  let board3 = iMoveMan i board2 (iParseNumber i curPosWhite) (iParseNumber i destPosWhite)
  putStrLn (iPrintBoard i board3)
  let tempBoard = board3
  board3 <- if or (iMill i tempBoard (iParseNumber i destPosWhite) (White))
              then
              showAndRemoveMan i tempBoard (Just Black) (iMill i tempBoard (iParseNumber i destPosWhite) (Black))
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
  putStrLn (iPrintBoard i board)
  putStrLn ("Choose man to remove:")
  putStrLn (show (iparseCoordinate i (iMans i board playerToRemove)))
  input <- getLine
  let number = (read input :: Int)
  let board2 = iRemoveMan i board (iParseNumber i number)
  putStrLn (iPrintBoard i board2)
  if and listOfMills then (showAndRemoveMan i board2 (playerToRemove)) [False] else return board2
