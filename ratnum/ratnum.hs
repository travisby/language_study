module RatMod where

data RatNum =  RatNum { numerator :: Integer, denominator :: Integer }
-- instance Ord RatNum where

ratnum _ 0 = error "Cannot divide by zero!"
ratnum a b = RatNum a b

instance Eq RatNum where
    (==) first second = numerator first * denominator second == numerator second * denominator first


instance Ord RatNum where
    (<=) first second = (numerator first * denominator second) <= (numerator second * denominator first)


instance Show RatNum where
    show ratnum = show (numerator ratnum) ++ "/" ++ show (denominator ratnum)


instance Num RatNum where
    (+) first second = RatNum (numerator first * denominator second + numerator second * denominator first) (denominator first * denominator second)
    (*) (RatNum a b) (RatNum c d) = RatNum (a * c) (b * d)

    negate (RatNum a b) = RatNum (-1 * a) b

    abs (RatNum a b) = RatNum (abs a) (abs b)

    signum first
        | first > 1 = 1
        | first == 0 = 0
        | otherwise = -1

    fromInteger a = RatNum a 1
