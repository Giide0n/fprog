compare1 :: (Ord a) => a -> a -> Ordering
compare1 a b = if a> b 
    then GT 
    else 
        if a == b 
        then EQ
        else LT

compare2 :: (Ord a) => a -> a -> Ordering
compare2 a b 
    | a> b = GT
    | a== b = EQ
    | otherwise = LT


compare3 :: (Ord a) => a -> a -> Ordering
compare3 a b = case a > b of 
    True -> GT
    False -> case a == b of
        True -> EQ 
        False -> LT
 

type M22 = ((Int, Int), (Int, Int))

add :: M22 -> M22 -> M22
add ((a1, b1), (c1,d1)) ((a2, b2), (c2,d2)) = ((a1+a2, b1+b2), (c1+c2,d1+d2))

sub :: M22 -> M22 -> M22
sub ((a1, b1), (c1,d1)) ((a2, b2), (c2,d2)) = ((a1-a2, b1-b2), (c1-c2,d1-d2))

mulS :: M22 -> Int -> M22
mulS ((a1, b1), (c1,d1)) x = ((a1*x, b1*x), (c1*x,d1*x))


mul :: M22 -> M22 -> M22
mul ((a1, b1), (c1,d1)) ((a2, b2), (c2,d2)) = ((a1*a2 + b1*c2, a1*b2 +b1*d2), (a2*c1+d1*c2,d1*d2+c1*b2))



