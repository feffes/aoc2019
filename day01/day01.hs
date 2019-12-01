reqFuel :: Integer -> Integer
reqFuel mass  | req mass < 0 = 0
              | otherwise = req mass + (reqFuel (req mass))
        where req f = (quot mass 3) - 2



main :: IO String
main = do 
  file <- readFile "input"
  let input = map read (lines file) :: [Integer]
  let answer = sum (map reqFuel input)
  return $ show answer