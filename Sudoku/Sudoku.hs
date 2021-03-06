import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
newtype Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
  Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: Sudoku
example2 =
        Sudoku
          [ [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          , [j 9,j 6,j 1,j 1,j 7,j 1,j 2,j 1,j 1]
          ]
      where
        j = Just
------------Lab 3A---------------------------------------------------
-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku sudoku) = length sudoku == 9 && row' sudoku


row' :: [[Maybe Int]] -> Bool
row' [] = True
row' (x:xs) = element' x && length x == 9 && row' xs

element' :: [Maybe Int] -> Bool
element' [] = True
element' (x:xs) | x > Just 0 && x < Just 10 = True && element' xs
                | isNothing x = True && element' xs
                | otherwise = False


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku sudoku) = length sudoku == 9 && checkRows sudoku

checkRows :: [[Maybe Int]] -> Bool
checkRows [] = True
checkRows (x:xs) = checkElement x && length x == 9 && checkRows xs

checkElement :: [Maybe Int] -> Bool
checkElement [] = True
checkElement (x:xs) | x > Just 0 && x < Just 10 = True && checkElement xs
                    | otherwise = False

-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku sudoku) = putStrLn(formatSudoku sudoku)

formatSudoku :: [[Maybe Int]] -> String
formatSudoku [] = ""
formatSudoku (x:xs) = getElement x ++  "\n" ++ formatSudoku xs

getElement :: [Maybe Int] -> String
getElement [] = ""
getElement (x:xs) | x > j 0 && x < j 10 = case x of Just x -> show x ++ getElement xs
                  | x == n = "." ++ getElement xs
                  where n = Nothing
                        j = Just
-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
      content <- readFile path
      checkContent (Sudoku (map parseRows (lines content)))

--Checks so that content that has been parsed is actually a sudoku
checkContent :: Sudoku -> IO Sudoku
checkContent sudoku
          | isSudoku sudoku = return sudoku
          | otherwise = error "no sudoku here"

--maps the rows and parses each element
parseRows :: String -> [Maybe Int]
parseRows = map parseElements

--Checks if an element from the file is valid
parseElements :: Char -> Maybe Int
parseElements '.' = Nothing
parseElements x
              | isDigit x = Just (digitToInt x)
              | otherwise = error "This does not belong here"

-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing), (1, elements [Just n | n <- [1..9]])]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3
--prop_sudoku used for quickcheck
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku (Sudoku sudoku) = isSudoku (Sudoku sudoku)

-------------------------------------------------------------------------

-- * D1

type Block = [Maybe Int]

--Checks that a 3x3 block does not contain the same digit twice
-- if its nothing it continues with checking
-- otherwise it checks each element against every other elementin the list
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (Nothing:xs) = isOkayBlock xs
isOkayBlock (x:xs) = notElem x xs && isOkayBlock xs

-- * D2
--blocks acquires rows, cols from the help methods which recursively add the blocks.
blocks :: Sudoku -> [Block]
blocks (Sudoku sudoku) = getRow sudoku ++ getCol sudoku ++ rows' sudoku

getRow :: [[a]] -> [[a]]
getRow [] = []
getRow x = take 9 x ++ getRow (drop 9 x)

getCol :: [[a]] -> [[a]]
getCol [] = []
getCol x = transpose (take 9 x) ++ getCol (drop 9 x)

rows' :: [[a]] -> [[a]]
rows' [] = []
rows' x = cols' (transpose (take 3 x)) ++ rows' (drop 3 x)

cols' :: [[a]] -> [[a]]
cols' [] = []
cols' x = (concat (take 3 x)) : cols' (drop 3 x)

--prop_blocks checks length of block list and that all the blocks are correct
prop_blocks :: Sudoku -> Bool
prop_blocks (Sudoku sudoku) = length (blocks (Sudoku sudoku)) == (3*9) && and [isOkayBlock block | block <- blocks (Sudoku sudoku)]

-- * D3

-- checks that all the blocks are correct.
isOkay :: Sudoku -> Bool
isOkay (Sudoku sudoku) = and [isOkayBlock block | block <- blocks (Sudoku sudoku)]

------------------------------------------------------------------------

-- E1*
type Pos = (Int,Int)

-- Takes a sudoku and returns all blank positions (e.g where the value is equal to Nothing)
blanks :: Sudoku -> [(Int,Int)]
blanks (Sudoku sudoku) = [(i,col) | (i,row) <- zip [0..8] sudoku,
                                     (col) <- (blanks' (row))]
                                     where blanks' cols  = [col | (col,value) <- zip [0..8] cols, isNothing value]

-- prop for checking that the position actually is blank
prop_blanks :: Sudoku -> Bool
prop_blanks s = and [isNothing $ (rows s)!!x!!y | (x,y) <- blanks s]

-- E2*
-- takes a list and a index,value pair and uses list comprehension to create a new list with the new value at that position
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) list (index, newValue) =  [if i == index then newValue else a | (i, a) <- zip [0..] list]

-- E3*
-- updates a given sudoku with a new value at given position
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku sudoku) (yIN,xIN) newValue = Sudoku [if y == yIN then (row !!= (xIN, newValue)) else row | (y,row) <- zip [0..] sudoku]

--prop for checking that the position gets updated with the new value
prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update (Sudoku sudoku) (yIN,xIN) newValue = and [if (row !! xIN)==  newValue then True else False | (y,row) <- zip [0..] sudoku, (x,col) <- zip [0..8] row, y == yIN, x == xIN]

-- E4*
-- Given a sudoku and a blank position the function returns all the possible values for that position
candidates :: Sudoku -> Pos -> [Int]
candidates (Sudoku sudoku) (y,x) = [value | value <- [1..9], isOkay (update (Sudoku sudoku) (y,x) (Just value)) && isSudoku (update (Sudoku sudoku) (y,x) (Just value))]

-- prop for checking if candidates are valid
prop_candidates :: Sudoku -> Pos -> Bool
prop_candidates (Sudoku sudoku) (y,x) = and [True | value <- [1..9], isOkay (update (Sudoku sudoku) (y,x) (Just value)) && isSudoku (update (Sudoku sudoku) (y,x) (Just value))]

-------------------------------------------------------------------------

-- F1
-- Given a sudoku solve uses backtracking and returns a solved sudoku or Nothing if there was no solution found recursively
solve :: Sudoku -> Maybe Sudoku
solve (Sudoku sudoku) | (isSudoku (Sudoku sudoku) && isOkay (Sudoku sudoku)) = solve' (Sudoku sudoku) (blanks (Sudoku sudoku))
                      | otherwise = Nothing
                      where solve' (Sudoku sudoku) [] = Just (Sudoku sudoku)
                            solve' (Sudoku sudoku) (x:blanks) | candid == [] = Nothing
                                                              | otherwise = (listToMaybe ( catMaybes ([solve' (update (Sudoku sudoku) x (Just c)) blanks | c <- candidates (Sudoku sudoku) x ])))
                                                            where (candid) = (candidates (Sudoku sudoku) (x))

-- F2
-- Reads from a file, solves the sudoku and prints it to the solution to the console
readAndSolve :: FilePath -> IO ()
readAndSolve path = do
      content <- readFile path
      case (solve (Sudoku (map parseRows (lines content)))) of Nothing -> putStrLn ("No solution")
                                                               otherwise -> printSudoku (fromJust (solve (Sudoku (map parseRows (lines content)))))
-- F3
-- checks wether a given sudoku is a solution to another soduku and returns true if this is the case
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf (Sudoku s1) (Sudoku s2) = and [if (isSolutionOf' sRow s2Row) then True else False | (i,sRow) <- zip [0..8] s1, (index,s2Row) <- zip [0..8] solved2, i == index]
  where isSolutionOf' sRow s2Row = and [if element1 == Nothing then False else (if(fromJust element1 == fromJust element2) then True else False) | (i,element1) <- zip [0..8] sRow, (index,element2) <- zip [0..8] s2Row, i == index]
        Just (Sudoku solved2) = solve (Sudoku s2)

-- F4
--
prop_solveSound :: Sudoku -> Bool
prop_solveSound sudoku
    | solved == Nothing = True
    | otherwise           = isSolutionOf (fromJust solved) sudoku where
                              solved = solve sudoku

fewerChecks prop = quickCheckWith stdArgs{ maxSuccess = 30 } prop
