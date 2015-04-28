import System.IO
import Control.Exception
import Data.List
import Text.Read

data Cell = Empty | Filled deriving (Eq)
instance Show Cell where
    show Empty = "_"
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
  let possibleBoards = genBoardsOnRowWithColBlocks (length colsBlocks) rowsBlocks
  -- finds proper board based on blocks in columns
  correctPair <- find (\pair -> colsBlocks == (snd pair)) possibleBoards
  Just $ fst correctPair
  
-- generates pairs: (possible board based on blocks in rows, 
-- for a given board return blocks in columns)
genBoardsOnRowWithColBlocks :: RowSize -> BlocksInRows -> [(Board, BlocksInCols)]
genBoardsOnRowWithColBlocks rowSize rowsBlocks =
  [(possibleBoard, getColumnBlocks  possibleBoard) 
    | possibleBoard <- generatePossibleBoards rowSize rowsBlocks]
  
-- gets blocks in columns for a given board viewed from row perspective
getColumnBlocks :: Board -> BlocksInCols
getColumnBlocks rowViewBoard =
  let colViewBoard = transpose rowViewBoard in
  [map length $ filter (\(cell:_) -> cell == Filled) $ group column | column <- colViewBoard]
 
 
{- POSSIBLE BOARDS GENERATION BASED ON BLOCKS IN ROWS -}
  
-- generates all possible boards combination for given blocks in rows    
generatePossibleBoards :: RowSize -> BlocksInRows -> [Board]
generatePossibleBoards rowSize rowsBlocks =
  sequence $ generatePossibleRows rowSize rowsBlocks
 
-- generates all possible rows combination for given blocks in rows   
generatePossibleRows :: RowSize -> BlocksInRows -> [[Row]]
generatePossibleRows rowSize rowsBlocks = 
  rowsGenerator rowSize <$> rowsBlocks

-- generates all possible row combination for given blocks in row 
rowsGenerator :: RowSize -> BlocksInRow -> [Row]
rowsGenerator rowSize rowBlocks = 
  -- reservedCells: minimum number of cell that must be filled 
  -- that's all filled cells + one cell intervals between them
  let reservedCells = sum rowBlocks + length rowBlocks - 1 in 
  rowsGen rowSize 0 reservedCells rowBlocks
  
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
