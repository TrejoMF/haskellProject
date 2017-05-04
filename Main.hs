-- Battleship Haskel Project
-- Module       : Main
-- Authors      : Guillermo Ramirez, Eulalio Garcia, Alan Motta

module Main where
    
    import Board
    import System.IO
    import System.Exit
    import System.Random

    main = do
        putStrLn("----------------------------\n         BATTLESHIP"
            ++"\n----------------------------")
        putStrLn "\nStarting new game\n"
        let bd = mkBoard 10
        let gb = placeShips [5,4,3,2,2] bd
        putStrLn("1 2 3 4 5 6 7 8 9 0")
        putStrLn("-------------------")
        putStrLn (boardToStr sqToStr gb)
        play gb sqToStr
    
    mainCheat = do
        putStrLn("----------------------------\n    BATTLESHIP CHEAT MODE"
            ++"\n----------------------------")
        let cbd = mkBoard 10
        let cgb = placeShips [5,4,3,2,2] cbd
        putStrLn("1 2 3 4 5 6 7 8 9 0")
        putStrLn("-------------------")
        putStrLn (boardToStr sqToStrCheat cgb)
        play cgb sqToStrCheat
             
    {-
    play bd markerFunc

    Recursive function that reads x and y from player and hits the place
    on the board. Then it checks to see if game is over.
    -}
    play bd markerFunc = do
        c <- readXY bd
        let board = hitBoard (fst c) (snd c) bd
        putStrLn("\n1 2 3 4 5 6 7 8 9 0")
        putStrLn("-------------------")
        putStrLn (boardToStr markerFunc board)
        checkGameOver board
        play board markerFunc

    -- placeShips ships bd = placeShip bd
    placeShips ships bd = do
    	let x = randomRIO(1,10)
    	let y = randomRIO(1,10)
    	let dir = randomRIO(True,False)
    
    {-
    readXY bd p

    Reads a 1-based pair of indices (x, y) for player p, denoting an 
    unhit place in a board bd. The function reads inputs from the
    standard input (stdin) and returns an Int tuple. Also checks for 
    validity of coordinates.
    -}
    --readXY:: a -> b -> IO(Int, Int)
    readXY bd = do
        putStr "Enter x coordinate: "
        x <- getX
        putStr "Enter y coordinate: "
        y <- getX
        --Checking if valid
        if (not (isHit x y bd))
        then return (x,y)
        else do
            putStrLn "\n\nPlace was already hit, Try again!\n\n"
            readXY bd

    getX = do
        putStrLn "Enter a positive value?"
        line <- getLine
        let parsed = reads line :: [(Int, String)] in
            if length parsed == 0
            then getX'
            else 
                let (x, _) = head parsed in
                    if x == -1 
                    then exitProgram
                    else if (x > 0) && (x < 11)
                    then return x
                    else getX'
        where getX' = do
               putStrLn "Invalid input!"
               getX

    sqToStr :: Int -> [Char]
    sqToStr sq = if sq == (-1) then ['X'] else if sq < (-1) then ['O'] else ['.']

    sqToStrCheat :: Int -> [Char]
    sqToStrCheat sq = if sq == (-1) then ['X'] else if (sq > 0) then (show (sq)) else if (sq < (-1)) then ['O'] else ['.']

    {-
    checkGameOver bd

    This function checks to see if the game is over, and returns a boolean
    -}
    checkGameOver bd = do
        if (isGameOver bd)
        then do
            putStrLn("++++++++++++++++++++++++++++++++++")
            putStrLn ("\t    GAME OVER!!")
            putStrLn ("+++++++++++++++++++++++++++++++++\n\n")
            main
        else return()

    exitProgram = do
        putStrLn "----------------------------\n         GOODBYE"
        putStrLn "----------------------------"
        exitFailure