import System.IO
import Data.List
import Data.Maybe

data Cell = Empty | Filled deriving (Eq)

instance Show Cell where
    show Empty = "_"
    show Filled = "x"

cellToChar :: Cell -> Char
cellToChar Empty = 'o'
cellToChar Filled = 'x'

main = do
  {-putStrLn "Podaje nazwe pliku: "-}  
  let valRow = [[3], [2, 2], [2, 2], [3], [1], [1], [1], [1], [2], [3]]
  let valCol = [[2, 1], [4, 2], [1, 7], [4], [2]]
  displaySolution valRow valCol

{- DISPLAY FUNCTIONS -}
displaySolution :: [[Int]] -> [[Int]] -> IO ()
displaySolution rowsBlocks colsBlocks =
  let correctBoard = returnCorrectBoard rowsBlocks colsBlocks in 
  mapM_ print (displayPossibleRow correctBoard)

displayPossibleRow :: [[Cell]] -> [[Char]]
displayPossibleRow rows = [map cellToChar row | row <- rows ]

-- return correct board 
returnCorrectBoard :: [[Int]] -> [[Int]] -> [[Cell]]
returnCorrectBoard rowsBlocks colsBlocks =
  -- finds all possible boards based on blocks in rows 
  -- with blocks in column for a given board
  let possibleBoardsOnRow = genBoardsOnRowWithColBlocks (length colsBlocks) rowsBlocks in
  -- finds proper board based on blocks in columns
  fst (fromJust (find (\pair -> (colsBlocks == (snd pair))) possibleBoardsOnRow) )
  
-- generate pairs: (possible board based on blocks in rows, 
-- for a given board return blocks in columns)
genBoardsOnRowWithColBlocks :: Int -> [[Int]] -> [([[Cell]], [[Int]])]
genBoardsOnRowWithColBlocks rowSize rowsBlocks =
  [(possibleBoard, getColumnBlocks  possibleBoard) 
    | possibleBoard <- generatePossibleBoards rowSize rowsBlocks]
  
-- get blocks in columns from board
getColumnBlocks :: [[Cell]] -> [[Int]]
getColumnBlocks rowViewBoard =
  let colViewBoard = transpose rowViewBoard in
  [map length (filter (\(cell:_) -> cell == Filled) (group column)) | column <- colViewBoard]
 
 
{- POSSIBLE BOARDS GENERATION BASED ON BLOCKS IN ROWS -}
  
-- generate all possible boards combination for a given blocks in rows    
generatePossibleBoards :: Int -> [[Int]] -> [[[Cell]]]
generatePossibleBoards rowSize rowsBlocks =
  sequence $ generatePossibleRows rowSize rowsBlocks
 
-- generate all possible rows combination for a given blocks in rows   
generatePossibleRows :: Int -> [[Int]] -> [[[Cell]]]
generatePossibleRows rowSize rowsBlocks = 
  map (rowsGenerator rowSize) rowsBlocks

-- generate all possible row combination for a given blocks in row 
rowsGenerator :: Int -> [Int] -> [[Cell]]
rowsGenerator rowSize rowBlocks = 
  -- reservedCells: minimum number of cell that must be filled 
  -- that's all filled cells + one cell intervals between them
  let reservedCells = (sum rowBlocks) + (length rowBlocks) - 1 in 
  rowsGen rowSize 0 reservedCells rowBlocks
  
-- helper function for rows generation
-- offsetEmptyCell is 0 for first Filled block, otherwise 1
rowsGen :: Int -> Int -> Int -> [Int] -> [[Cell]]
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
      let remainingReservedCells = reservedCells - curBlock - 1,
      restRow <- rowsGen remainingRowSize 1 remainingReservedCells restBlocks]
