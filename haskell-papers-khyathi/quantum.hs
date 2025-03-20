oneOverSqrt2 :: Double -- an useful way to write 1/√2
oneOverSqrt2 = 1 / sqrt 2

-- 2.1 Enumerated Types

data Bool = False | True -- declaring bool

class (Eq a, Ord a) => Basis a where -- overloaded operator 
    basis :: [a]

data Move = Vertical | Horizontal
data Rotation = CtrClockwise | Cloclwise
data Color = Red | Yellow | Blue

instance Basis Bool where
    basis = [False, True]
instance Basis Move where
    basis = [Vertical, Horizontal]
instance Basis Rotation where
    basis = [CtrClockwise, Cloclwise]
instance Basis Color where
    basis = [Red, Yellow, Blue]
-- Given the unit vectors for type a, values of type QV a are maps which associate each unit vector with a probability amplitude.

type PA = Complex Double
type QV a = FiniteMap a PA

-- The function pr returns the probability amplitude associated with a given unit vector:

qv :: Basis a => [(a, PA)] - > QV a
qv = listToFM

pr :: Basis a = > FiniteMap a PA -> a -> PA
pr fm k= lookupWithDefaultFM fm 0 k

-- simple examples:

qFalse, qTrue, qFT :: QV Bool
qFalse = unitFM False 1
qTrue = unitFM True 1
qFT = qv [(False, oneOverSqrt2), (True, oneOverSqrt2)]
qUp :: QV Move
qUp = unitFM Vertical 1

-- 2.2 Infinite Types


instance Basis Integer where
basis= [0..] -- this means is an infinite list starting from 0 and counting upwards (0, 1, 2, 3, ...).
