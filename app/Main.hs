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
  | ExprCell (Int, Int)
  | ExprPlus (Expr, Expr)
  deriving (Show)

data Cell
  = CellText String
  | CellNumber Float
  | CellExpr Expr
  deriving (Show)

type Table = [[Cell]]

estimate :: Table -> (Int, Int)
estimate table = (length table, maximum $ map length table)

convertToIndex :: String -> (Int, Int)
convertToIndex (c : r) = (read r - 1, ord c - ord 'A')
convertToIndex _ = error "Invalid cell index"

parseExprCell :: String -> Expr
parseExprCell (c : rest)
  | not (null rest) && isUpper c = ExprCell $ convertToIndex (c : rest)
  | otherwise = error "Cell reference must start with capital letter"
parseExprCell _ = error "Expression can't be empty"

parseExpr :: String -> Expr
parseExpr s =
  case break (== '+') s of
    (left, "") ->
      if null left
      then error "Expression can't be empty"
      else case readMaybe left of
        Just n -> ExprNumber n
        Nothing -> parseExprCell s
    (left, right) -> ExprPlus (parseExpr left, parseExpr $ drop 1 right)

parseCell :: String -> Cell
parseCell ('=' : expr) = CellExpr $ parseExpr expr
parseCell s
  | Just n <- readMaybe s = CellNumber n
  | otherwise = CellText s

parseTable :: String -> Table
parseTable = map (map parseCell . map trim . splitOn '|') . lines

evalExpr :: Table -> Expr -> Float
evalExpr t (ExprCell (row, col)) =
  case (t !! row) !! col of
    CellText _ -> undefined
    CellNumber n -> n
    CellExpr e -> evalExpr t e
evalExpr _ (ExprNumber n) = n
evalExpr t (ExprPlus (left, right)) =
  case (left, right) of
    (ExprNumber a, ExprNumber b) -> a + b
    (ExprCell _, ExprCell _) -> (evalExpr t left) + (evalExpr t right)
    (ExprPlus _, ExprPlus _) -> (evalExpr t left) + (evalExpr t right)
    _ -> undefined

evalCell :: Table -> Cell -> Cell
evalCell t (CellExpr e) = CellNumber $ evalExpr t e
evalCell _ c = c

evalTable :: Table -> Table
evalTable t = map (map $ evalCell t) t

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] -> do
      contents <- readFile input
      let table = evalTable $ parseTable contents
      putStrLn $ show table
    _ -> do
      program <- getProgName
      hPutStrLn stderr $ printf "Usage: %s <input.csv>" program
      exitFailure
