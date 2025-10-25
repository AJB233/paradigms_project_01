-- ###
-- Mode Detection and Basic Loop
-- ###

module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout, isEOF)

-- Detect interactive or batch mode
isInteractive :: [String] -> Bool
isInteractive args = not ("-b" `elem` args || "--batch" `elem` args)

-- Placeholder for the main loop
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
            putStrLn ("You entered: " ++ input)
            evalLoop interactive history

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

