compareIf :: (Ord a) => a -> a -> Ordering
compareIf x y = if x > y then GT else if x == y then EQ else LT

compareGuard :: (Ord a) => a -> a -> Ordering
compareGuard x y
    | x > y = GT
    | x < y = LT
    | otherwise = EQ

compareCase :: (Ord a) => a -> a -> Ordering
compareCase x y = case x > y of 
                    True -> GT 
                    False -> case x == y of 
                      True -> EQ
                      False -> LT

type M22 = (Int, Int, Int, Int);

add :: M22 -> M22 -> M22
add (a1, a2, a3, a4) (b1, b2, b3, b4) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4)

sub :: M22 -> M22 -> M22
sub (a1, a2, a3, a4) (b1, b2, b3, b4) = (a1 - b1, a2 - b2, a3 - b3, a4 - b4)

mulS :: M22 -> Int -> M22
mulS (a1, a2, a3, a4) x = (a1 * x, a2 * x, a3 * x, a4 * x)

mul :: M22 -> M22 -> M22
mul (a1, a2, a3, a4) (b1, b2, b3, b4) = (a1 * b1 + a2 * b3, 0, 0, 0)