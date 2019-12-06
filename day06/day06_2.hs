import Data.Map (Map, (\\))
import qualified Data.Map as Map (insert, empty, lookup, keys, intersection, delete, filter)
import Data.List.Split

type Orbits = Map String String


-- counts allorbits
part1 o = sum lns
  where orbs = allOrbits o
        lns  = map length orbs




-- List of each objects orbit map
allOrbits :: Orbits ->  [Orbits]
allOrbits o = map (findOrbits o ) (Map.keys o)


findOrbits :: Orbits -> String -> Orbits
findOrbits p s = findOrbits' p Map.empty s 

findOrbits' :: Orbits -> Orbits -> String -> Orbits
findOrbits' inp out s  = case Map.lookup s inp of 
  Nothing -> out
  Just orb -> findOrbits' inp (Map.insert orb s out) orb 

insStrings :: [[String]] -> Orbits -> Orbits
insStrings [] m = m
insStrings (x:xs) m = insStrings xs $ Map.insert (x!!1) (x!!0) m


main :: IO Integer
main = do
  input <- readFile "input"
  let 
    lns = lines input
    wrds = (map (splitOn ")") lns)
    mp = insStrings wrds Map.empty -- stuff under is p2
    yourbits = findOrbits mp "YOU"
    sanbits = findOrbits mp "SAN"
    inter = Map.intersection yourbits sanbits
    nyourbits = yourbits \\ inter
    nsanbits = sanbits \\ inter
    
  return $ fromIntegral $ length nyourbits + length nsanbits