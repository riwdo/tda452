import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
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
isSudoku (Sudoku sudoku) = length (sudoku) == 9 && row' sudoku


row' :: [[Maybe Int]] -> Bool
row' [] = True
row' (x:xs) = element' x && length x == 9 && row' xs

element' :: [Maybe Int] -> Bool
element' [] = True
element' (x:xs) | x > (Just 0) && x < (Just 10) = True && element' xs
                | x == Nothing = True && element' xs
                | otherwise = False


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku sudoku) = length (sudoku) == 9 && checkRows sudoku

checkRows :: [[Maybe Int]] -> Bool
checkRows [] = True
checkRows (x:xs) = checkElement x && (length x == 9) && checkRows xs

checkElement :: [Maybe Int] -> Bool
checkElement [] = True
checkElement (x:xs) | x > (Just 0) && x < (Just 10) = True && checkElement xs
                    | otherwise = False

-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku sudoku) = do putStrLn(formatSudoku sudoku)

formatSudoku :: [[Maybe Int]] -> String
formatSudoku [] = ""
formatSudoku (x:xs) = getElement x ++  "\n" ++ formatSudoku xs

getElement :: [Maybe Int] -> String
getElement [] = ""
getElement (x:xs) | x > (j 0) && x < (j 10) = case x of Just x -> show x ++ getElement xs
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
cell = frequency [(1,return (Just 1)), (1,return (Just 2)), (1,return (Just 3)), (1,return (Just 4)), (1,return (Just 5)), (1,return (Just 6)), (1,return (Just 7)), (1,return (Just 8)), (1,return (Just 9)), (9, return (Nothing))]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)

-- * C3
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
isOkayBlock (x:xs) = (notElem x xs) && isOkayBlock xs

-- * D2

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

prop_blocks :: Sudoku -> Bool
prop_blocks (Sudoku sudoku) = length (blocks (Sudoku sudoku)) == (3*9) && and [isOkayBlock block | block <- blocks (Sudoku sudoku)]

-- * D3

isOkay :: Sudoku -> Bool
isOkay (Sudoku sudoku) = checkBlocks (blocks (Sudoku sudoku))

checkBlocks :: [Block] -> Bool
checkBlocks [] = True
checkBlocks (x:xs) = isOkayBlock x && checkBlocks xs

------------------------------------------------------------------------

-- E1*

type Pos = (Int,Int)

blanks :: Sudoku -> [(Int,Int)]
blanks (Sudoku sudoku) = [(x,y)
                          | (x,col) <- zip [0..8] (head sudoku)
                          , (y,row) <- zip [0..8] sudoku
                          , col == Nothing
                          ]

-- E2*

(!!=) :: [a] -> (Int, a) -> [a]
(!!=) list (index, newValue) =  [if i == index then newValue else a | (i, a) <- zip [0..] list]

-- E3*

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku sudoku) (yIN,xIN) newValue = Sudoku [if y == yIN then ((head sudoku) !!= (xIN, newValue)) else row | (y,row) <- zip [0..8] sudoku]

-- E4*

candidates :: Sudoku -> Pos -> [Int]
candidates (Sudoku sudoku) (y,x) = [value | value <- [1..9], isOkay (update (Sudoku sudoku) (y,x) (Just value)) && isSudoku (update (Sudoku sudoku) (y,x) (Just value))]

--prop_candidates :: Sudoku -> Pos -> Bool
--prop_candidates (Sudoku sudoku) (y,x) = and [True | value <- [1..9], isOkay (update (Sudoku sudoku) (y,x) (Just value)) && isSudoku (update (Sudoku sudoku) (y,x) (Just value))]

-- F1*

solve :: Sudoku -> Maybe Sudoku
solve (Sudoku sudoku) = if (isSudoku (Sudoku sudoku) && isOkay (Sudoku sudoku)) then solve' (Sudoku sudoku) (blanks (Sudoku sudoku)) else Nothing

solve' :: Sudoku -> [Pos] -> Maybe Sudoku
solve' (Sudoku sudoku) | blanks (Sudoku sudoku) == [] = Just (Sudoku sudoku)
solve' (Sudoku sudoku) = [solve' (update (Sudoku sudoku) (y,x) (Just (cand)))  | (y,x) <- blanks (Sudoku sudoku)
                                                                          , cand  <- candidates (Sudoku sudoku) (y,x)]
--solve' (Sudoku sudoku) [] | isFilled (Sudoku sudoku) == True = Just (Sudoku sudoku)
--solve' (Sudoku sudoku) (x:blanks) = solve' (update (Sudoku sudoku) x (Just(head (candidates (Sudoku sudoku) x)))) blanks
