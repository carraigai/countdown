-- Channel 4 countdown solver
-- Given a set of numbers, and solution, figure out how the numbers were combined to arrive at solution

-- show (Plus (Raw 15) (Multiply ( Minus (Raw 5) (Raw 1) ) (Raw 2)))
-- "(15+((5-1)*2))"  
data Exp = Raw Int | Plus Exp Exp | Minus Exp Exp | Multiply Exp Exp | Divide Exp Exp deriving Eq
instance Show Exp where
    show ( Raw a) = show a
    show ( Plus a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show ( Minus a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show ( Multiply a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
    show ( Divide a b) = "(" ++ show a ++ "/" ++ show b ++ ")"


-- splits [25,6,9,1,8,3]
-- split a list of numbers into all possible combinations of numbers
splits :: [a] -> [(a,[a])]
splits xs = splits' (head xs) (tail xs)
    where 
        splits' n [x] = [(n,[x])]
        splits' n xs  = (n,xs) : splits' (head xs) (tail xs)


-- convertToExpression (25,[6,9,1,8,3])
convertToExpression :: (Int, [Int]) -> (Exp, [Exp])
convertToExpression (x, ys) = ( Raw x, map (\y -> Raw y) ys )

-- this function will evaluate a/b and b/a before creating expression.
divisionHelper :: Exp -> Exp -> [Exp] -> [(Exp, [Exp])]
divisionHelper a b xs
    | eb > ea      = divisionHelper b a xs
    | otherwise    = if ea `mod` eb == 0 then [(Divide a b, [])] else []
    where ea = evalExpression a
          eb = evalExpression b

-- createExpressions (25,[6,9,1,8,3])
createExpressions :: (Exp, [Exp]) -> [(Exp, [Exp])]
createExpressions (x, [])  = [(x, [])]
createExpressions (x, ys)  = 
        let 
         step = pair' x ys 0
        in concatMap createExpressions step ++ step

    where pair' x ys n  
            | n == length ys       = []
            | otherwise            = 
                 let    
                   rest'              = take n ys ++ drop (n+1) ys
                   plus_exp'          = ( (Plus  x (ys!!n)), rest')
                   minus_exp'         = ( (Minus x (ys!!n)), rest')
                   multiply_exp'      = ( (Multiply x (ys!!n)), rest')
                   division_exp'      = divisionHelper x (ys!!n) rest' 
                 in 
                   division_exp' ++ [ plus_exp',  minus_exp', multiply_exp' ] ++ pair' x ys (n+1)


-- evalExpression
-- reduce a `mod` reduce b == 0
evalExpression :: Exp -> Int
evalExpression ( Raw a )        =  a
evalExpression ( Plus a b)      = evalExpression a + evalExpression b
evalExpression ( Minus a b)     = evalExpression a - evalExpression b
evalExpression ( Multiply a b)  = evalExpression a * evalExpression b
evalExpression ( Divide a b)    = evalExpression a `div` evalExpression b




-- countdownSolver [25,6,9,1,8,3] 52
-- countdownSolver [25,6,9,1,8,3] 303
-- countdownSolver [25,6,9,1,8,3] 26
-- countdownSolver [100, 4, 5, 10] 20
countdownSolver :: [Int] -> Int -> Exp
countdownSolver nums target = fst $ head $ filter (\(x,y) ->  evalExpression x == target) . concatMap createExpressions . map convertToExpression $ splits nums

