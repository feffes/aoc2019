

import Data.List.Split
import Data.Sequence

-- splits a program into operations
prog :: Int -> [Integer] -> [Integer]
prog i codes    | codes!!i == 99 = codes
                | codes!!i == 1 = prog (i+4) addition
                | codes!!i == 2 = prog (i+4) mult
                | otherwise = error $ "opcode " ++ show i ++ " not specified: " ++ show (codes!!i)
    where
        addition = replaceNth p (codes!!f + codes!!s)  codes
        mult = replaceNth p (codes!!f * codes!!s)  codes
        f = fromIntegral $ codes !! (i+1) 
        s = fromIntegral $ codes !! (i+2)
        p = fromIntegral $ codes !! (i+3)

part2 :: [Integer] -> [Integer] -> Integer
part2 (n:ns) m = case ret of
    Nothing  -> part2 ns m 
    Just out -> out
    where ret = part2' n 0 m
        

part2' :: Integer -> Integer -> [Integer] -> Maybe Integer
part2' n v m | (prog 0 (replaceNth 1 n (replaceNth 2 v m)))!!0 == 19690720 = Just $ 100 * n + v
             | v < 100 = part2' n (v+1) m
             | otherwise = Nothing
             
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

main :: IO ()
main = do 
  file <- readFile "input1"
  let input = map read (splitOn "," file) :: [Integer]
  print $ prog 0 input

main2 :: IO ()
main2 = do 
  file <- readFile "input1"
  let input = map read (splitOn "," file) :: [Integer]
  let i = [0..100]
  print $  part2 i input