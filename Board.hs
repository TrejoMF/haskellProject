-- Battleship Haskell Project
-- Module       :  Board
-- Authors      :  Guillermo Ramirez, Eulalio Garcia, Alan Motta

module Board(
  mkBoard,
  row,
  hitBoard,
  isHit,
  hitOrMiss,
  isGameOver,
  isShipPlaceable,
  placeShip,
  boardToStr) where

  {-|
       Return an empty nxn board, where n is a positive number. A 1-based 
       pair of indices (x,y) will be used to access a specific place of
       the board, where x and y are column and row indices, respectively.
  -}
  mkBoard :: Int -> [Int]
  mkBoard n   |   n <= 0      =   []
              |   otherwise   =   take(n*n) (repeat 0)
  
  {-|
      Return the size of a board bd, n for an nxn board.
  -}
  size :: [Int] -> Int
  size bd = round (sqrt(fromIntegral (length bd)))
  
  {-|
      Return a row y of a board bd, where y is a 1-based index. It returns
      a list of size n, where n is the size of bd.
  -}
  row :: Int -> [Int] -> [Int]
  row y bd   |   y < 1           =   []
             |   y > (size bd)   =   []
             |   otherwise       =   getSubList start (start + (size bd) - 1) bd
             where start = (y - 1) * (size bd)

  getSubList :: Int -> Int -> [a] -> [a]
  getSubList 0     0    (h:l) = [h]
  getSubList 0     end  (h:l) = (h:(getSubList 0 (end - 1) l))
  getSubList start end  (h:l) = getSubList (start - 1) (end - 1) l

  {-|
      Hit a place (x,y) in a board bd, where x and y 
      are 1-based column and row indices. The specified place is assumed
  -}
  hitBoard :: Int -> Int -> [Int] -> [Int]
  hitBoard x y bd = firstHalf ++ hitOrMiss n ++ secondHalf
                  where (firstHalf, (n:secondHalf)) = splitAt (((y - 1) * (size bd) + (x-1))) bd

  hitOrMiss :: Int -> [Int]
  hitOrMiss x | x == 0 = [-1]
              | x > 0 = [x * (-1)]
              | otherwise = [x]

  {-|
      Has a place (x,y) been already hit? 
      The x and y are 1-based column and row indices.
  -}
  isHit :: Int -> Int -> [Int] -> Bool
  isHit x y bd = (bd !! (((y-1) * size bd) + (x-1))) < 0

  isOccupied :: Int -> Int -> [Int] -> Bool
  isOccupied x y bd = (bd !! (((y-1) * size bd) + (x-1))) /= 0

  {-|
      Is the game on a board bd over?
  -}
  isGameOver :: [Int] -> Bool
  isGameOver bd = (length [x | x <- bd, x > 0]) == 0
  
  {-|
      Can a ship of size n be places at (x,y) on board bd
  -}
  isShipPlaceable :: Int -> Int -> Int -> Bool -> [Int] -> Bool
  isShipPlaceable n x y dir bd  | n == 0 = True
                                | ((dir == True) && (x + n) < (size bd)) = False
                                | ((dir == False) && (y + n) < (size bd)) = False
                                | (x > (size bd)) || (y > (size bd)) = False
                                | (isOccupied x y bd) = False
                                | otherwise = if(dir == True) then (isShipPlaceable (n - 1) (x + 1) (y) (dir) (bd)) else (isShipPlaceable (n - 1) (x) (y + 1) (dir) (bd))  

  {-|
      Place a ship of size n at (x,y) on board bd
  -}
  --placeShip :: Int -> Int -> Int -> Bool -> [Int] -> [Int]
  
  placeShip bd =    [0,0,0,0,2,2,0,0,0,0]
                  ++[0,0,0,0,0,0,0,0,0,0]
                  ++[0,2,0,0,0,0,0,0,0,0]
                  ++[0,2,0,0,5,5,5,5,5,0]
                  ++[0,0,0,0,0,0,0,0,0,0]
                  ++[0,0,0,0,0,0,0,0,0,0]
                  ++[0,0,4,4,4,4,0,0,0,0]
                  ++[0,0,0,0,0,0,0,0,3,0]
                  ++[0,0,0,0,0,0,0,0,3,0]
                  ++[0,0,0,0,0,0,0,0,3,0]

  {-|
      Return a string representation of a board bd. It is a higher-order
      function, and markerFunc is a function that converts a square 
      to a character representation
  -}
  boardToStr :: (Int -> [Char]) -> [Int] -> [Char]
  boardToStr markerFunc bd = allRowsToStrings markerFunc bd (size bd)

  allRowsToStrings :: (Int -> [Char]) -> [Int] -> Int -> [Char]
  allRowsToStrings markerFunc bd 0       = []
  allRowsToStrings markerFunc bd currRow = (allRowsToStrings markerFunc bd (currRow - 1)) ++ (rowToString markerFunc (row currRow bd)) ++ ['\n'] 

  rowToString :: (Int -> [Char]) -> [Int] -> [Char]
  rowToString _            []      = []
  rowToString markerFunc (h:rem) = (markerFunc h) ++ [' '] ++ (rowToString markerFunc rem)