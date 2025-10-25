-- ###
-- Mode Detection and Basic Loop
-- ###

module Main where

import System.Environment (getArgs)
import System.IO (hFlush, stdout, isEOF)
import Text.Read (readMaybe)
import Data.Char (isDigit)

-- history is [Double], index 1-based
getHistoryValue :: String -> [Double] -> Either String Double
getHistoryValue ('$':rest) history
  | all isDigit rest =
      let idx = read rest :: Int
      in if idx > 0 && idx <= length history
           then Right (history !! (idx - 1))
           else Left ("Error: Invalid history reference " ++ '$':rest)
  | otherwise = Left "Error: Invalid history reference format"
getHistoryValue _ _ = Left "Error: Invalid Expression"


-- Detect interactive or batch mode
isInteractive :: [String] -> Bool
isInteractive args = not ("-b" `elem` args || "--batch" `elem` args)

-- Expression evaluation
--Parse and eval a prefix expression recursively
evalExpr :: [String] -> [Double] -> Either String (Double, [String])
evalExpr [] _ = Left "Invalid Expression"
evalExpr (tok:rest) history =
  case tok of
    "+" -> do
      (v1, rest1) <- evalExpr rest history
      (v2, rest2) <- evalExpr rest1 history
      return (v1 + v2, rest2)

    "*" -> do
      (v1, rest1) <- evalExpr rest history
      (v2, rest2) <- evalExpr rest1 history
      return (v1 * v2, rest2)

    "/" -> do
      (v1, rest1) <- evalExpr rest history
      (v2, rest2) <- evalExpr rest1 history
      if v2 == 0
        then Left "Division by zero"
        else return (v1 / v2, rest2)

    "-" -> do 
      (v, rest1) <- evalExpr rest history
      return (-v, rest1)

    ('$':_) ->
        case getHistoryValue tok history of
          Right val -> Right (val, rest)
          Left err  -> Left err

    _ ->
      case readMaybe tok :: Maybe Double of
        Just num -> Right (num, rest)
        Nothing  -> Left "Invalid Expression"

-- Helper to tokenize input
tokenize :: String -> [String]
tokenize = words

-- Wrapper to evaluate a full line
evaluateLine :: String -> [Double] -> Either String Double
evaluateLine input history = do
  let tokens = tokenize input
  (value, remaining) <- evalExpr tokens history
  if null remaining
    then Right value
    else Left "Invalid Expression"

printResult :: Bool -> [Double] -> Double -> IO ()
printResult interactive history result = do
        let newId = length history + 1
        let output = show newId ++ ": " ++ show (realToFrac result :: Double)
        putStrLn output

-- Eval loop (REPL)
evalLoop :: Bool -> [Double] -> IO ()
evalLoop interactive history = do
    if interactive then putStr "> " else return ()
    hFlush stdout
    eof <- isEOF
    if eof
      then putStrLn "Goodbye!"
      else do
        input <- getLine
        if input == "quit"
          then putStrLn "Goodbye!"
          else do
            case evaluateLine input history of
              Left err -> putStrLn ("Error: " ++ err)
              Right result -> do
                  printResult interactive history result
                  let newHistory = history ++ [result]
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

