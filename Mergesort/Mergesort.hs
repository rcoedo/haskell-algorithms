import System.Environment
import Control.Monad.Writer
import Data.List
import Data.List.Split

showHelp :: IO ()
showHelp = putStrLn "Usage: mergesort <v1,v2,v3 ... vn>"

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

halve :: [Int] -> ([Int], [Int])
halve [] = error "Can not split an empty list!"
halve xs@(_:[]) = (xs, [])
halve xs = splitAt (length xs `div` 2) xs

merge' :: [Int] -> [Int] -> [Int] -> [Int]
merge' [] [] state = reverse state
merge' (x:xs) [] state = merge' xs [] (x:state)
merge' [] (y:ys) state = merge' [] ys (y:state)
merge' xs ys state
  | x <= y = merge' txs ys (x:state)
  | otherwise = merge' xs tys (y:state)
  where x:txs = xs
        y:tys = ys

merge :: [Int] -> [Int] -> [Int]
merge xs ys = merge' xs ys []

mergeSort :: [Int] -> Writer [String] [Int]
mergeSort [] = return []
mergeSort xs@(_:[]) = return xs
mergeSort xs = do
  tell ["Sorting " ++ show l1 ++ " and " ++ show l2]
  l1' <- mergeSort l1
  l2' <- mergeSort l2
  tell ["Merging " ++ show l1' ++ " and " ++ show l2']
  let result = merge l1' l2'
  tell ["Merge result " ++ show result]
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
