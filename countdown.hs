-- Channel 4 countdown solver
-- Given a set of numbers, and solution, figure out how the numbers were combined to arrive at solution
-- countdown_solver [25,6,9,1,8,3] 303
-- countdown_solver [25,6,9,1,8,3] 26

-- show (Plus (Raw 15) (Multiply ( Minus (Raw 5) (Raw 1) ) (Raw 2)))
-- "(15+((5-1)*2))"  
data Exp = Raw Int | Plus Exp Exp | Minus Exp Exp | Multiply Exp Exp deriving Eq
instance Show Exp where
    show ( Raw a) = show a
    show ( Plus a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show ( Minus a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
    show ( Multiply a b) = "(" ++ show a ++ "*" ++ show b ++ ")"


-- splits [25,6,9,1,8,3]
-- split a list of numbers into all possible combinations of numbers
splits :: [a] -> [(a,[a])]
splits xs = splits' (head xs) (tail xs)
    where 
        splits' n [x] = [(n,[x])]
        splits' n xs  = (n,xs) : splits' (head xs) (tail xs)


-- convert_to_expression (25,[6,9,1,8,3])
convert_to_expression :: (Int, [Int]) -> (Exp, [Exp])
convert_to_expression (x, ys) = ( Raw x, map (\y -> Raw y) ys )


-- create_expressions (25,[6,9,1,8,3])
create_expressions :: (Exp, [Exp]) -> [(Exp, [Exp])]
create_expressions (x, [])  = [(x, [])]
create_expressions (x, ys)  = 
        let 
         step = pair' x ys 0
        in concatMap create_expressions step ++ step

    where pair' x ys n  
            | n == length ys       = []
            | otherwise            = 
                 let    
                   rest'              = take n ys ++ drop (n+1) ys
                   plus_exp'          = ( (Plus  x (ys!!n)), rest')
                   minus_exp'         = ( (Minus x (ys!!n)), rest')
                   multiply_exp'      = ( (Multiply x (ys!!n)), rest')
                  in [ plus_exp',  minus_exp', multiply_exp' ] ++ pair' x ys (n+1)



-- eval_expression
eval_expression :: Exp -> Int
eval_expression ( Raw a )        =  a
eval_expression ( Plus a b)      = eval_expression a + eval_expression b
eval_expression ( Minus a b)     = eval_expression a - eval_expression b
eval_expression ( Multiply a b)  = eval_expression a * eval_expression b


-- countdown_solver [25,6,9,1,8,3] 52
-- all +
countdown_solver :: [Int] -> Int -> Exp
countdown_solver nums target = fst $ head $ filter (\(x,y) ->  eval_expression x == target) $ concatMap create_expressions $ map convert_to_expression $ splits nums

