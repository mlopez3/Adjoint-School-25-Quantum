
import Data.Complex (Complex(..), conjugate)
import Prelude hiding ((>>=), (<*>), return)

class Eq a => Basis a where
    basis :: [a]

type PA = Complex Double
type Vec a = a -> PA

(>>=) :: Basis a => Vec a -> (a -> Vec b) -> Vec b
va >>= f = \b -> sum [va a * f a b | a <- basis]

return :: Basis a => a -> Vec a
return a b =
    if a == b
    then 1
    else 0

mzero :: Vec a
mzero = const 0

mplus :: Vec a -> Vec a -> Vec a
mplus v1 v2 a = v1 a + v2 a

mminus :: Vec a -> Vec a -> Vec a
mminus v1 v2 a = v1 a - v2 a

($*) :: PA -> Vec a -> Vec a
pa $* v = \a -> pa * v a

(<*>) :: Vec a -> Vec b -> Vec (a,b)
v1 <*> v2 = \(a,b) -> v1 a * v2 b

(<.>) :: Basis a => Vec a -> Vec a -> PA
v1 <.> v2 = sum $ (\a -> conjugate (v1 a) * v2 a) <$> basis


--[just for better printing]--------------------
class ToBit a where
    toBit :: a -> String

instance ToBit Bool where
    toBit False = "0"
    toBit True = "1"

instance (ToBit a, ToBit b) => ToBit (a,b) where
    toBit (a,b) = foldl1 (<>) ["(", toBit a, ",", toBit b, ")"]

showVec :: ToBit a => Basis a => Vec a -> String
showVec v = unlines $ [show (v a, toBit a) | a <- basis, v a /= 0]
-------------------------------------------------


instance Basis Bool where
    basis = [True, False]

instance (Basis a, Basis b) => Basis (a,b) where
    basis = [(a,b) | a <- basis, b <- basis]

qFalse, qTrue, qFT, qFmT :: Vec Bool

qFalse = return False
qTrue = return True
qFT = (1/sqrt 2) $* (qFalse `mplus` qTrue)
qFmT = (1/sqrt 2) $* (qFalse `mminus` qTrue)

type Lin a b = a -> Vec b

fun2lin :: (Basis a, Basis b) => (a -> b) -> Lin a b
fun2lin f a = return (f a)

qnot :: Lin Bool Bool
qnot = fun2lin not

phase :: Lin Bool Bool
phase False = return False
phase True = (0 :+ 1) $* return True

hadamard :: Lin Bool Bool
hadamard False = qFT --this is not the fourier transform, it's "FalseTrue"
hadamard True = qFmT

controlled :: Basis a => Lin a a -> Lin (Bool, a) (Bool, a)
controlled f (b1, b2) = return b1 <*> (if b1 then f b2 else return b2)

adjoint :: Lin a b -> Lin b a
adjoint f b a = conjugate (f a b)

(>*<) :: Basis a => Vec a -> Vec a -> Lin a a
v1 >*< v2 = \a1 a2 -> v1 a1 * conjugate (v2 a2)

linplus :: (Basis a, Basis b) => Lin a b -> Lin a b -> Lin a b
linplus f g a = f a `mplus` g a

lintens :: (Basis a, Basis b, Basis c, Basis d) =>
    Lin a b -> Lin c d -> Lin (a, c) (b, d)
lintens f g (a,c) = f a <*> g c

o :: (Basis b) => Lin a b -> Lin b c -> Lin a c
o f g a = f a >>= g

type Dens a = Vec (a,a)

lin2vec :: (a -> Vec b) -> Vec (a, b)
lin2vec = uncurry

pureD :: Basis a => Vec a -> Dens a
pureD v = lin2vec (v >*< v)

type Super a b = (a, a) -> Dens b

lin2super :: (Basis a, Basis b) => Lin a b -> Super a b
lin2super f (a1, a2) = f a1 <*> dual (adjoint f) a2
    where dual f a b = f b a

trL :: (Basis a, Basis b) => Super (a, b) b
trL ((a1, b1), (a2, b2)) = if a1 == a2 then return (b1, b2) else mzero

meas :: Basis a => Super a (a,a)
meas (a1, a2) = if a1 == a2 then return ((a1,a1), (a1,a1)) else mzero

-----------

arr :: (Basis b, Basis c) => (b -> c) -> Super b c
arr f = fun2lin (\(b1, b2) -> (f b1, f b2))

(>>>) :: (Basis c, Basis d, Basis b) =>
    Super b c -> Super c d -> Super b d
a >>> b = a `o` b
infixr 3 >>>

first :: (Basis b, Basis c, Basis rest) =>
    Super b c -> Super (b, rest) (c, rest)
first f ((b1, d1), (b2, d2)) = permute (f (b1, b2) <*> return (d1, d2))
    where permute v ((b1, b2), (d1, d2)) = v ((b1, d1), (b2, d2))

returnA :: Basis a => Super a a
returnA (x, y) (x', y') = if x == x' && y == y' then 1 else 0

-----------


e1 :: Super ((Bool, Bool), Bool) ((Bool, Bool), Bool)
e1 = first (first (lin2super hadamard))

e1' ::  Super ((Bool, Bool), Bool) ((Bool, Bool), Bool)
e1' = 
    arr (\((a,b), c) -> (a, (b, c)))
    >>> first (lin2super hadamard)                   
    >>> arr (\(a,(b,c)) -> ((a,b),c))

entangle :: Super (Bool, Bool) (Bool, Bool)
entangle = 
    first (lin2super hadamard)
    >>> lin2super cnot
    >>> \(x', y') -> returnA (x',y')
    where cnot = controlled qnot

e2 :: Basis b => Super (Bool, b) (Bool, b)
e2 = arr (\(a,b) -> (a,b)) >>>   
    first (lin2super hadamard)



{-

1 0 0 1 
0 0 0 0
0 0 0 0
1 0 0 1

-}
testEnt :: IO ()
testEnt = do
    let b = entangle ((False, False), (False, False))
    putStrLn $ showVec b 

main :: IO ()
main = testEnt