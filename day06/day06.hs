{- 
Always describe as "object orbits object" or the other way around? 

 COM
^   ^
|  | 
A  B
^
|
C

A and B orbit Com, they point to COM

store it in a Map, i guess? maybe just store it in a map to begin with, aka lookup looks like

find from C to Com, find C in map, it's value is A, find what A's value is, COM, find COM's value, Nothing


input is in form AAA)BBB , BBB is the key AAA is the value


-}
import Data.Map (Map)
import qualified Data.Map as Map (insert, empty, lookup, keys)
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
    mp = insStrings wrds Map.empty
  return $ (fromIntegral $ part1 mp)

