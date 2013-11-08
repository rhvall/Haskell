import System.IO

type Balance = (Int, Int, Int, [Int], [Int])

generalBalanceWeight = 10    
bal1 = [3]
bal2 = [0,5]
bal3 = [0]
bal4 = [0]
bal5 = [0]
bal6 = [0]
bal7 = [0]
bal8 = [1,4]
bal9 = [7]
bal10 = [0]
bal11 = [0,2,3]
bal12 = [6,1]
allBal = [bal1,bal2, bal3, bal4, bal5, bal6, bal7, bal8, bal9, bal10, bal11, bal12]
bal0 :: Balance
bal0 = (0, 0, 0, [], [])
balA = (clasifyBalance 0 allBal)!!0
balB = (clasifyBalance 0 allBal)!!5
balC = (clasifyBalance 0 allBal)!!3
balD = (clasifyBalance 0 allBal)!!4
bals = (clasifyBalance 0 allBal)

test1 = showBalance $ separateBalance 0 bal1 bal2
test2 = clasifyBalance 0 allBal
test3 = balanceTheBalance (test2 !! 0) test2

headZero :: [Int] -> Int
headZero []     = 0
headZero (x:xs) = x

headZeroBal :: [Balance] -> Balance
headZeroBal []     = bal0
headZeroBal (x:xs) = x

tailZero :: [a] -> [a]
tailZero []     = []
tailZero (x:xs) = xs

separateBalance :: Int -> [Int] -> [Int] -> Balance
separateBalance num xs ys = (num, headZero xs, headZero ys, tailZero xs, tailZero ys)

balanceWeight :: Balance -> [Balance] -> Int
balanceWeight (num,x,y,xs,ys) bs = biggest + generalBalanceWeight
    where
        leftBalanced = totalBalance xs bs
        rightBalanced = totalBalance ys bs
        leftBal = leftBalanced + x
        rightBal = rightBalanced + y
        biggest = (max leftBal rightBal) * 2

totalBalance :: [Int] -> [Balance] -> Int
totalBalance [] _ = 0
totalBalance _ [] = 0
totalBalance (x:xs) ys = (balanceWeight (ys!!x) ys) + totalBalance xs ys

balanceNumber :: Balance -> Int
balanceNumber (_,x,y,_,_) = bigger + generalBalanceWeight
    where 
        bigger = (max x y) * 2

showBalance :: Balance -> String
showBalance (num,x,y,xy,ys) = show num ++ ": " ++ show (x) ++ " " ++ show (y)

clasifyBalance :: Int -> [[Int]] -> [Balance]
clasifyBalance _ [] = []
clasifyBalance num (x:[]) = [separateBalance num x []]
clasifyBalance num (x:y:[]) = [separateBalance num x y]
clasifyBalance num (x:y:ys) = separateBalance num x y : clasifyBalance (num + 1) ys

balanceTheBalance :: Balance -> [Balance] -> Balance
balanceTheBalance (num,x,y,xs,ys) bals =
    if (leftWeight == rightWeight)
    then (num,x,y,xs,ys) 
    else        
        if (leftWeight > rightWeight)
        then (num, 0, difference, xs, ys)
        else (num, difference, 0, xs, ys)   
    where
        leftWeight = (x + totalBalance xs bals)
        rightWeight = (y + totalBalance ys bals)
        difference = abs (leftWeight - rightWeight)
        
orderBalance :: [Balance] -> [Balance] -> [Balance]
orderBalance [] _ = []
orderBalance _ [] = []
orderBalance ordered balances = balanceTheBalance (headZeroBal ordered) balances : orderBalance (tailZero ordered) balances
        
strToInt :: String -> [Int]
strToInt = (map read) . words

main = do
    zz <- readFile "input001.txt"
--    zz <- getLine
    print zz
    let z = map strToInt $ tail $ lines zz
    print z
--    let inc = increment z
--    print inc
--    let x = missing z
    let allBalances = clasifyBalance 0 z
    print $ map showBalance allBalances
    let x = orderBalance allBalances allBalances 
    print x 
--    writeFile "output.txt" $ show x
    
  
