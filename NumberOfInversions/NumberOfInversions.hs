import System.Environment
import Control.Monad.Writer
import Data.List
import Data.List.Split

showHelp :: IO ()
showHelp = putStrLn "Usage: inversions <v1,v2,v3 ... vn>"

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

halve :: [Int] -> ([Int], [Int])
halve [] = error "Can not split an empty list!"
halve xs@(_:[]) = (xs, [])
halve xs = splitAt (length xs `div` 2) xs

merge' :: [Int] -> [Int] -> ([Int], Int) -> ([Int], Int)
merge' [] [] (state, inversions) = (reverse state, inversions)
merge' (x:xs) [] (state, inversions) = merge' xs [] ((x:state), inversions)
merge' [] (y:ys) (state, inversions) = merge' [] ys ((y:state), inversions)
merge' xs ys (state, inversions)
  | x <= y = merge' txs ys ((x:state), inversions + 1)
  | otherwise = merge' xs tys ((y:state), inversions)
  where x:txs = xs
        y:tys = ys

merge :: [Int] -> [Int] -> ([Int], Int)
merge l1 l2 = merge' l1 l2 ([], 0)

mergeSort :: [Int] -> Writer [String] ([Int], Int)
mergeSort [] = return ([], 0)
mergeSort xs@(_:[]) = return (xs, 0)
mergeSort xs = do
  tell ["Sorting " ++ show l2 ++ " and " ++ show l1]
  (l1', _) <- mergeSort l1
  (l2', _) <- mergeSort l2
  tell ["Merging " ++ show l2' ++ " and " ++ show l1']
  let result@(result', inversions) = merge l1 l2
  tell ["Merge result " ++ show result' ++ ", found " ++ show inversions ++ " inversions"]
  return result
  where (l1, l2) = halve xs

parseArgs :: [String] -> Maybe [Int]
parseArgs (arg:[]) = mapM readMaybe $ (splitOn "," arg)
parseArgs _ = Nothing

parseTrace :: [String] -> String
parseTrace trace = concat $ intersperse "\n" $ trace

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> showHelp
    Just xs -> let (_, trace) = runWriter (mergeSort xs) in
      putStrLn $ parseTrace $ trace
