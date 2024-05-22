module Main where

import System.Environment
import System.Exit
import System.IO
import Data.Char
import Text.Read
import Text.Printf

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
                "" -> []
                s' -> w : splitOn c s''
                  where (w, s'') = break (== c) s'

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

data Expr
  = ExprNumber Float
  | ExprCell String
  | ExprPlus (Expr, Expr)
  deriving (Show)

data Cell
  = CellText String
  | CellNumber Float
  | CellExpr Expr
  deriving (Show)

type Table = [[Cell]]

parseExpr :: String -> Expr
parseExpr s =
  case break (== '+') s of
    (left, "") ->
      case readMaybe left of
        Just n -> ExprNumber n
        Nothing -> ExprCell s
    (left, right) -> ExprPlus (parseExpr left, parseExpr $ drop 1 right)

parseCell :: String -> Cell
parseCell ('=' : expr) = CellExpr $ parseExpr expr
parseCell s | Just n <- readMaybe s = CellNumber n
            | otherwise = CellText s

parseTable :: String -> Table
parseTable = map (map parseCell . map trim . splitOn '|') . lines

estimate :: Table -> (Int, Int)
estimate table = (length table, maximum $ map length table)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      contents <- readFile input
      let table = parseTable contents
      putStrLn $ show table
    _ -> do
      program <- getProgName
      hPutStrLn stderr $ printf "Usage: %s <input.csv>" program
      exitFailure
