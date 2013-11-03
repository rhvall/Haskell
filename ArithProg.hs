import System.IO

initial :: (Int, Int)
initial = (0, 1)

pairSum :: [Int] -> (Int, Int) -> (Int, Int)
pairSum [] _ = (0, 0)
pairSum (x:[]) (u, v) = (u, v)
pairSum (x:y:ys) (u, v) = 
    if (u /= 0 && diff /= u)
    then
        if (v > 2)
        then
            (u, v + 1)
        else
            (diff, v)
    else
        pairSum (y:ys) (diff, v + 1)
    where
        diff = abs(x - y)


missing :: [Int] -> Int
missing [] = 0
missing ys = (head ys) + (snd val - 1) * (fst val)
        where 
            val = pairSum ys initial
            
					
strToInt :: String -> [Int]
strToInt = (map read) . tail . words

main = do
--    zz <- readFile "input.txt"
    zz <- getLine
    let z = strToInt zz
--    print z
--    let inc = increment z
--    print inc
    let x = missing z
    print x 
--    writeFile "output.txt" $ show x
    
  
