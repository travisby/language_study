module RatMod where

data RatNum =  RatNum { numerator :: Integer, denominator :: Integer }
-- instance Ord RatNum where

ratnum _ 0 = error "Cannot divide by zero!"
ratnum a b = RatNum a b

instance Eq RatNum where
    (==) (RatNum a b) (RatNum c d) = a * d == c * b


instance Ord RatNum where
    (<=) (RatNum a b) (RatNum c d) = (a * d) <= (c * b)


instance Show RatNum where
    show (RatNum a b) = show a ++ "/" ++ show b


instance Num RatNum where
    (+) (RatNum a b) (RatNum c d) = RatNum (a * d + c * b) (b * d)
    (*) (RatNum a b) (RatNum c d) = RatNum (a * c) (b * d)

    negate (RatNum a b) = RatNum (-1 * a) b

    abs (RatNum a b) = RatNum (abs a) (abs b)

    signum first
        | first > 1 = 1
        | first == 0 = 0
        | otherwise = -1

    fromInteger a = RatNum a 1
