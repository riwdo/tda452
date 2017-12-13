module Morris where
import Man
import RunGame
import Data.Maybe
import Data.Map.Strict
-- Define Morris board



-- Create a map that gives adjacentElements given a key coordinate
adjacentElements = fromList [((0,0),[(0,3),(3,0)]),((0,3),[(0,0),(0,6),(1,3)]),((0,6),[(0,3),(3,6)]),((1,1),[(1,3),(3,1)])
                            ,((1,3),[(0,3),(2,3),(1,1),(1,5)]),((1,5),[(1,3),(3,5)]),((2,2),[(2,3),(3,2)]),((2,3),[(2,2),(1,3),(2,4)])
                            ,((2,4),[(2,3),(3,4)]),((3,0),[(0,0),(3,1),(6,0)]),((3,1),[(3,0),(3,2),(1,1),(5,1)]),((3,2),[(2,2),(4,2),(3,1)])
                            ,((3,4),[(2,4),(3,5),(4,4)]),((3,5),[(3,4),(3,6),(1,5),(5,5)]),((3,6),[(0,6),(3,5),(6,6)]),((4,2),[(3,2),(4,3)])
                            ,((4,3),[(4,2),(4,4),(5,3)]),((4,4),[(4,3),(3,4)]),((5,1),[(3,1),(5,3)]),((5,3),[(5,1),(4,3),(5,5),(6,3)])
                            ,((5,5),[(5,3),(3,5)]),((6,0),[(3,0),(6,3)]),((6,3),[(6,0),(5,3),(6,6)]),((6,6),[(6,3),(3,6)])]

-- Empty board
startingMorris :: Morris
startingMorris = Morris [[n,      Just bl,Just bl,  n, Just bl,Just bl,          n]
                        ,[Just bl,   n,   Just bl,  n, Just bl,   n,       Just bl]
                        ,[Just bl,Just bl,    n,    n,    n,   Just bl,    Just bl]
                        ,[n,         n,       n, Just bl, n,      n,             n]
                        ,[Just bl,Just bl,    n,    n,    n,   Just bl,    Just bl]
                        ,[Just bl,   n,    Just bl, n, Just bl,   n,       Just bl]
                        ,[n,      Just bl, Just bl, n, Just bl,Just bl,         n]]
                        where n = Nothing
                              bl = Blank
                              w = White
                              b = Black

printBoard :: Morris -> IO ()
printBoard board = putStrLn (formatSudoku (listPair board))

formatSudoku :: [[Maybe Man]] -> String
formatSudoku [] = ""
formatSudoku (x:xs) = getElement x ++  "\n" ++ formatSudoku xs

getElement :: [Maybe Man] -> String
getElement [] = ""
getElement (x:xs) | x == n = "_\t" ++ getElement xs
                  | x == (Just Blank) = "\t" ++ getElement xs
                  | otherwise = (Prelude.take 1 (show (fromJust x))) ++ "\t"++ getElement xs
                  where n = Nothing
                        j = Just
                        b = Blank

fullHand :: Man -> HandMan
fullHand player = Add (player) (Add (player) (Add (player) (Add (player) (Add (player) (Add (player) (Add (player) (Add (player) (Add (player) Empty))))))))

-- Get all blank positions
blanks :: Morris -> [(Int,Int)]
blanks (Morris board) = [(y,col) | (y,row) <- zip [0..8] board
                          , col <- blanks' row]
                          where blanks' row  = [col | (col,value) <- zip [0..8] row, isNothing value]

-- Check if each player has formed a mill and returns a bool if that's the case.
mill :: Morris -> (Int,Int) ->  Man -> [Bool]
mill (board) (y,x) player = [checkMill board False (y,x) (y,x) player] ++ [checkMill board True (y,x) (y,x) player]

checkMill :: Morris -> Bool -> (Int, Int) -> (Int, Int) -> Man -> Bool
checkMill board vertical (y,x) prevState player | length [value | value <- neighbours, case vertical of True -> x == snd value
                                                                                                        False -> y == (fst value) ] == 1 =
                                        if checkPos board (y,x) == Just player then
                                          checkMill board vertical (head [value | value <- neighbours, case vertical of True -> x == snd value
                                                                                                                        False -> y == fst value]) (y,x) player
                                        else
                                          False
                              | otherwise = if (length [True | element <- list, checkPos board element /= Just player] /= 0) then False else True
                              where (neighbours, list) = ((adjacentElements ! (y,x)), ([(y,x)] ++ [value | value <- neighbours, case vertical of True -> x == snd value
                                                                                                                                                 False -> y == fst value, value /= prevState]))

checkPos :: Morris -> (Int,Int) -> Maybe Man
checkPos board (yIn,xIn) = head ([checkRow list y yIn xIn | (y,list) <- zip [0..] (listPair board)] !! yIn)
    where checkRow list y yIn xIn = [man | (x,man) <- zip [0..] list,yIn == y,xIn ==x]



-- Given a board and a Man returns the positions of that player's mans
mans :: Morris -> Maybe Man -> [(Int,Int)]
mans (Morris board) man = [(y,col) | (y,row) <- zip [0..8] board
                         , col <- blanks' row]
                         where blanks' row  = [col | (col,value) <- zip [0..8] row, isNothing value == False, value == (man)]

-- takes a list and a index,value pair and uses list comprehension to create a new list with the new value at that position
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) list (index, newValue) =  [if i == index then newValue else a | (i, a) <- zip [0..] list]

  -- updates a given Morris board with a new man at given position
updateBoard :: Morris -> Maybe Man -> Pos -> Morris
updateBoard (Morris board) newValue (yIN,xIN) = Morris [if y == yIN then (row !!= (xIN, newValue)) else row | (y,row) <- zip [0..] board]

removeMan :: Morris -> Pos -> Morris
removeMan board (y,x) = updateBoard board Nothing (y,x)

moveMan :: Morris -> Pos -> Pos -> Morris
moveMan board currentPos newPos = removeMan (updateBoard board (checkPos board currentPos) newPos) currentPos

gameOver :: Morris -> (Bool,Maybe Man)
gameOver board | length (mans board w) < 3 = (True, w)
               | length (mans board b) < 3 = (True, b)
               | otherwise = (False, Nothing)
               where (w,b) = (Just White, Just Black)

implementation = Interface
    { iEmptyBoard = startingMorris
    , iFullHand   = fullHand
    , iPrintBoard = printBoard
    , iBlanks     = blanks
    , iMill       = mill
    , iCheckPos   = checkPos
    , iMans       = mans
    , iUpdateBoard= updateBoard
    , iRemoveMan  = removeMan
    , iMoveMan    = moveMan
    }

main :: IO ()
main = runGame implementation


----------------------------------Properties-----------------------------------------------
