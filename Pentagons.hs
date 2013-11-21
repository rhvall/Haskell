import Data.Ratio

simple :: [Integer]
simple = [1..10000]

pentagons :: [Integer]
pentagons = calcPentagons simple

pentagon :: Integer -> Integer
pentagon n = truncate(value / 2)
    where
        value = fromIntegral (n * (3 * n - 1)) :: Double

calcPentagons :: [Integer] -> [Integer]
calcPentagons = map pentagon

hasFraction' :: Rational -> Bool
hasFraction' = (/= 1) . denominator

hasFraction :: (Real a) => a -> Bool
hasFraction = hasFraction' . toRational

antiPentagon :: Integer -> Integer
antiPentagon p = 
    if (p <= 0)
    then
        0
    else
        trunInt
    where
        floatInt = fromIntegral (24 * p + 1) :: Double
        toSqrt = sqrt(floatInt) + 1
        isInteger = hasFraction toSqrt
        trunInt =
            if (isInteger == False && (mod (truncate toSqrt) 6) == 0)
            then
                truncate(toSqrt / 6)
            else
                0
                
searchPentagon :: Integer -> [Integer] -> Bool
searchPentagon x ys = not(result == [])
    where 
        result = filter (== x) ys
        
pmPentagon :: Integer -> [Integer] -> (Integer, Integer)
pmPentagon 0 _ = (0, 0)
pmPentagon _ [] = (0, 0)
pmPentagon x (y:ys) = 
--    if  (antiDiff == True && antiSumm == True)
    if  (antiDiff /= 0 && antiSumm /= 0)
    then
        (x,y)
    else
        pmPentagon x ys
    where
        diff = y - x
        summ = y + x
        antiDiff = antiPentagon(diff) -- searchPentagon diff (y:ys)
        antiSumm = antiPentagon(summ) -- searchPentagon summ (y:ys)
        
minDiff :: [Integer] -> (Integer, Integer, Integer)
minDiff [] = (0, 0, 0)
minDiff (x:[]) = (0, 0, 0) 
minDiff (x:y:ys) = 
    if  (penX /= 0 && penY /= 0)
    then 
        (abs $ penY - penX, penX, penY)
    else
        minDiff (y:ys)
    where
        (penX, penY) = pmPentagon x (y:ys)

        
main = do   
    --print pentagons
    print $ minDiff pentagons
    
