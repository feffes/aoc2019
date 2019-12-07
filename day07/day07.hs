

import Data.List.Split
import Data.Sequence



input :: IO Integer
input =  do 
  g <- getLine
  return $ read g
             
replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

main :: IO ()
main = do 
  file <- readFile "input"
  let input = map read (splitOn "," file) :: [Integer]
  out <- ioProg 0 (return input)
  --print out
  return ()

{-phaser :: [Integer] -> IO [Integer]
phaser modes = do
  ioProg 0 
-}


-----------------------------------



ioProg :: Int -> IO [Integer] -> IO [Integer]
ioProg i iocodes = do 
  codes <- iocodes
  let 
    f = fromIntegral $ codes !! (i+1) 
    s = fromIntegral $ codes !! (i+2)
    t = fromIntegral $ codes !! (i+3)
    c = codes !! i
    op = mod c 100
    fm = mod (div c 100) 10
    sm = mod (div c 1000) 10
    tm = mod (div c 10000) 10
    pval p m = case m of 
      1 -> fromIntegral p 
      0 -> fromIntegral $ codes !! p
    ttr = case tm of 
      1 -> i+3
      0 -> t
    ffr = case fm of
      1 -> i+1
      0 -> f
  case op of
    99 -> return codes
    1  -> ioProg (i+4) (return $ replaceNth t ((pval f fm) + (pval s sm)) codes)
    2  -> ioProg (i+4) (return $ replaceNth t ((pval f fm) * (pval s sm)) codes)
    3  -> do 
      inp <- input
      ioProg (i+2) (return $ replaceNth f inp codes)
    4  -> do
      let out = show (pval f fm)
      putStrLn $ out
      ioProg (i+2) (return $ codes)
    5 -> case pval f fm of
      0   -> ioProg (i+3) (return codes)
      _   -> ioProg (pval s sm) (return codes)
    6 -> case pval f fm of -- reverse of 5
      0   -> ioProg (pval s sm) (return codes)
      _   -> ioProg (i+3) (return codes)
    7 -> if (pval f fm) < (pval s sm) 
      then ioProg (i+4) (return $ replaceNth t 1 codes)
      else ioProg (i+4) (return $ replaceNth t 0 codes)
    8 -> if (pval f fm) == (pval s sm)
      then ioProg (i+4) (return $ replaceNth t 1 codes)
      else ioProg (i+4) (return $ replaceNth t 0 codes)