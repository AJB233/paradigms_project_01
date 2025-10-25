-- ###
-- Mode Detection and Basic Loop
-- ###

module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout, isEOF)

-- Detect interactive or batch mode
isInteractive :: [String] -> Bool
isInteractive args = not ("-b" `elem` args || "--batch" `elem` args)

printResult :: Bool -> [Double] -> Double -> IO ()
printResult interactive history result = do
        let newId = length history + 1
        let output = show newId ++ ": " ++ show (realToFrac result :: Double)
        if interactive then putStrLn output else putStrLn output

-- Eval loop (REPL)
evalLoop :: Bool -> [Double] -> IO ()
evalLoop interactive history = do
    if interactive then putStr "> " else return ()
    hFlush stdout
    eof <- isEOF
    if eof
      then putStrLn "BYE!"
      else do
        input <- getLine
        if input == "QUIT"
          then putStrLn "BYE!"
          else do
            -- Placeholder for now
            let result = fromIntegral (length history + 1) * 1.5
            printResult interactive history result

            -- Add result to history
            let newHistory = result : history
            evalLoop interactive newHistory

-- Main entry
main :: IO ()
main = do
    args <- getArgs
    let interactive = isInteractive args
    putStrLn ("Running in " ++ (if interactive then "interactive" else "batch") ++ " mode.")
    if interactive
        then putStrLn "Welcome to the Prefix Calculator!"
        else return ()
    evalLoop interactive []

