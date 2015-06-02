import System.IO
import Control.Exception
import Data.List
import Text.Read

data Cell = Null | Empty | Filled deriving (Eq)
instance Show Cell where
    show Null = "_"
    show Empty = "o"
    show Filled = "x"
    
type Row = [Cell]
type Column = [Cell]
type Board = [Row]

type RowSize = Int
type ColSize = Int

type BlocksInRow = [Int]
type BlocksInCol = [Int]

type BlocksInRows = [BlocksInRow]
type BlocksInCols = [BlocksInCol]

{- HELPER FUNCTIONS -}

-- spliting string by delimeter
splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]] 
  where f c l@(x:xs)  | c == delimiter = []:l
                      | otherwise = (c:x):xs

-- zipping Maybe
zipMaybe :: Maybe a -> Maybe b -> Maybe (a,b)
zipMaybe Nothing _ = Nothing
zipMaybe _ Nothing = Nothing
zipMaybe (Just a) (Just b) = Just (a,b)


{- MAIN FUNCTION -}
main = do
  fileName <- readFileName
  resReadFile <- readFileWithData fileName
  case resReadFile of
      Left  e       -> showFileError e
      Right content -> 
            displaySolution $ parseContent content >>= returnCorrectBoard


{- IO FUNCTIONS -}

-- reads filename
readFileName :: IO (String)
readFileName = do
  putStrLn "Podaje nazwe pliku: "
  getLine

-- reads data
readFileWithData :: String -> IO (Either IOException String)
readFileWithData fName = try (readFile fName)

-- shows error connected with file
showFileError :: IOException -> IO ()
showFileError e = do
  putStrLn $ show e
  putStrLn "Problem z plikiem wejsciowym"
  
-- display correct solution if exists
displaySolution :: Maybe Board -> IO ()
displaySolution maybeBoard =
  case maybeBoard of
    Just correctBoard -> mapM_ print correctBoard
    Nothing -> putStrLn $ id "No solutions found!"


{- PARSER -}

-- parses input
parseContent :: String -> Maybe (BlocksInRows, BlocksInCols)
parseContent content =
  zipMaybe blocksInRows blocksInCols
  where [blocksInRows, blocksInCols] = map (\x -> readMaybe x :: Maybe [[Int]]) $ take 2 $ splitBy '\n' content


{- LOGIC FUNCTIONS -}
  
-- returns a correct board if exists
returnCorrectBoard :: (BlocksInRows, BlocksInCols) -> Maybe Board
returnCorrectBoard (rowsBlocks, colsBlocks) = do
  -- find all possible boards based on blocks in rows 
  -- with blocks in column for a given board
  let colSize = length rowsBlocks
  let rowSize = length colsBlocks
  let init = replicate colSize (replicate rowSize Null)
  let initRow    = guessRowsB init rowSize rowsBlocks
  let initCol    = transpose (guessColsB (transpose initRow) colSize colsBlocks)
  let initRow2    = guessRowsB initCol rowSize rowsBlocks
  --let possibleBoards = genBoardsOnRowWithColBlocks (concatBoard initRow initCol) (length colsBlocks) rowsBlocks
  -- finds proper board based on blocks in columns
  --correctPair <- find (\pair -> colsBlocks == (snd pair)) possibleBoards
  Just $ initRow --concatBoard initRow initCol
  
concatBoard :: Board -> Board -> Board
concatBoard fromRow fromCol = 
  [concatRow (fst z) (snd z) | z <- zip fromRow fromCol]
  
concatRow :: Row -> Row -> Row
concatRow fromRow fromCol =
  zipWith (\x y -> if x == Filled || y == Filled then Filled else (if x == Empty || y == Empty then Empty else Null)) fromRow fromCol
  
guessRowsB :: Board -> Int -> BlocksInRows -> Board
guessRowsB initBoard rowSize blocks =
  [if length (snd z) == 1 then guessRowWithOne (fst z) rowSize (head (snd z)) else concatRow (fst z) (guessRow rowSize (snd z)) | z <- zip initBoard blocks]
  
guessColsB :: Board -> Int -> BlocksInCols -> Board
guessColsB initBoard colSize blocks =
  [if length (snd z) == 1 then guessRowWithOne (fst z) colSize (head (snd z)) else concatRow (fst z) (guessCol colSize (snd z)) | z <- zip initBoard blocks]
  
guessRowWithOne :: Row -> Int -> Int -> Row
guessRowWithOne rowBoard rowSize block = do
  --let remaining = rowSize - (curBlock + (sum restBlocks) + (length restBlocks))
  let rowFilled     = findFilled rowBoard rowSize
  --let leftFil       = if length rowFilled == 0 then block - 1 else (head rowFilled)
  let startLeft     = max 0 (leftFil - block + 1))
  let remainingLeft = rowSize - block - startLeft
  let leftRow       = (replicate startLeft (-2)) ++ (replicate block 0) ++ (replicate remainingLeft (-1))
  let rightFil      = if length rowFilled == 0 then rowSize - block else (last rowFilled)
  let startRight    = min (rowSize - block) rightFil
  let remainingRight = startRight
  let rightRow      = (replicate remainingRight (-1)) ++ (replicate block 0) ++ (replicate (rowSize - remainingRight - block) (-2))
  [if fst c == -2 || snd c == -2 then Empty else (if fst c /= -1 && fst c == snd c then Filled else Null) | c <- zip leftRow rightRow]

moveStart :: Row -> Int -> Int -> Int
moveStart rowBoard start stop  =
  if start == stop then stop else 
  (if rowBoard !! start == Empty then start else 
    moveStart rowBoard (start - 1) stop)

findFilled :: Row -> Int -> [Int]
findFilled row rowSize = map (\z -> snd z) (filter (\z -> fst z == Filled) (zip row [0 .. (rowSize - 1)]))

guessRow :: Int -> BlocksInRow -> Row
guessRow rowSize (curBlock : restBlocks) = do
  let numBlocks = length curBlock
  let remaining = rowSize - (curBlock + (sum restBlocks) + (length restBlocks))
  let leftRow   = (replicate curBlock 1) ++ (guessLeftH restBlocks 2) ++ (replicate remaining (- numBlocks - 1))
  let rightRow  = (replicate remaining (-1)) ++ (replicate curBlock 1) ++ (guessRightH restBlocks 2)
  [if fst c /= -1 && fst c == snd c then Filled else Null | c <- zip leftRow rightRow]

guessCol :: Int -> BlocksInCol -> Column
guessCol colSize (curBlock : restBlocks) = do
  let numBlocks = length curBlock
  let remaining = colSize - (curBlock + (sum restBlocks) + (length restBlocks))
  let leftRow = (replicate curBlock 1) ++ (guessLeftH restBlocks 2) ++ (replicate remaining (- numBlocks - 1))
  let rightRow = (replicate remaining (-1)) ++ (replicate curBlock 1) ++ (guessRightH restBlocks 2)
  [if fst c /= -1 && fst c == snd c then Filled else Null | c <- zip leftRow rightRow]


guessLeftH :: BlocksInRow -> Int -> [Int]
guessLeftH [] _  = []
guessLeftH (curBlock : restBlocks) index =
  (replicate 1 (-index)) ++ (replicate curBlock index)  ++ (guessLeftH restBlocks (index + 1))
  
guessRightH :: BlocksInRow -> Int -> [Int]
guessRightH [] _  = []
guessRightH (curBlock : restBlocks) index =
  (replicate 1 (-index)) ++ (replicate curBlock index)  ++ (guessLeftH restBlocks (index + 1))
  

-- generates pairs: (possible board based on blocks in rows, 
-- for a given board return blocks in columns)
genBoardsOnRowWithColBlocks :: Board -> RowSize -> BlocksInRows -> [(Board, BlocksInCols)]
genBoardsOnRowWithColBlocks initBoard rowSize rowsBlocks =
  [(possibleBoard, getColumnBlocks  possibleBoard) 
    | possibleBoard <- generatePossibleBoards initBoard rowSize rowsBlocks]
  
-- gets blocks in columns for a given board viewed from row perspective
getColumnBlocks :: Board -> BlocksInCols
getColumnBlocks rowViewBoard =
  let colViewBoard = transpose rowViewBoard in
  [map length $ filter (\(cell:_) -> cell == Filled) $ group column | column <- colViewBoard]
 
 
{- POSSIBLE BOARDS GENERATION BASED ON BLOCKS IN ROWS -}
  
-- generates all possible boards combination for given blocks in rows    
generatePossibleBoards :: Board -> RowSize -> BlocksInRows -> [Board]
generatePossibleBoards initBoard rowSize rowsBlocks =
  sequence $ generatePossibleRows initBoard rowSize rowsBlocks
 
-- generates all possible rows combination for given blocks in rows   
generatePossibleRows :: Board -> RowSize -> BlocksInRows -> [[Row]]
generatePossibleRows initBoard rowSize rowsBlocks = 
  map (rowsGenerator rowSize) (zip initBoard rowsBlocks)

-- generates all possible row combination for given blocks in row 
rowsGenerator :: RowSize -> (Row, BlocksInRow) -> [Row]
rowsGenerator rowSize (row, rowBlocks) = do
  -- reservedCells: minimum number of cell that must be filled 
  -- that's all filled cells + one cell intervals between them
  let reservedCells = sum rowBlocks + length rowBlocks - 1
  let rows = rowsGen rowSize 0 reservedCells rowBlocks
  filter (rowFilter row) rows
  
rowFilter :: Row -> Row -> Bool
rowFilter pat cur =
  all (\z -> (fst z /= Filled) || (snd z == Filled)) (zip pat cur)
  
-- helper function for rows generation
-- offsetEmptyCell is 0 for first Filled block, otherwise 1
rowsGen :: RowSize -> Int -> Int -> BlocksInRow -> [Row]
rowsGen n _ _ [] = [replicate n Empty]
rowsGen rowSize offsetEmptyCell reservedCells (curBlock : restBlocks) = 
  -- blockFilled: current filled block
  let blockFilled = replicate curBlock Filled in
  -- all possible Empty before current block ++
  -- current filled block ++
  -- all further possible row generation
  [(replicate i Empty) ++ blockFilled ++ restRow 
    | i <- [offsetEmptyCell .. rowSize - reservedCells], 
      let remainingRowSize = rowSize - i - curBlock,
      let reservedEmpty = 1,
      let remainingReservedCells = reservedCells - curBlock - reservedEmpty,
      restRow <- rowsGen remainingRowSize reservedEmpty remainingReservedCells restBlocks]
      

