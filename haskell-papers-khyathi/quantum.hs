

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

