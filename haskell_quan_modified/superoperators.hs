{-# LANGUAGE Arrows #-}

import Data.Complex (Complex(..), conjugate)
import Prelude hiding (return, (<*>), (>>=))
import Control.Arrow (returnA, (>>>), arr, first)

-- basis class and vector definitions --

class Eq a => Basis a where
    basis :: [a]

type PA = Complex Double
type Vec a = a -> PA

return :: Basis a => a -> Vec a
return a b = if a == b then 1 else 0

(>>=) :: Basis a => Vec a -> (a -> Vec b) -> Vec b
va >>= f = \b -> sum [(va a) * (f a b) | a <- basis]

mzero :: Vec a
mzero = const 0

mplus, mminus :: Vec a -> Vec a -> Vec a
mplus v1 v2 a = v1 a + v2 a
mminus v1 v2 a = v1 a - v2 a

($*) :: PA -> Vec a -> Vec a
pa $* v = \a -> pa * v a

(<*>) :: Vec a -> Vec b -> Vec (a,b)
v1 <*> v2 = \(a,b) -> v1 a * v2 b

(<.>) :: Basis a => Vec a -> Vec a -> PA
v1 <.> v2 = sum [conjugate (v1 a) * v2 a | a <- basis]

(<+>) :: Double -> Double -> Complex Double
r <+> i = r :+ i

-- boolean basis and sample vectors --

instance Basis Bool where
    basis = [False, True]

qFalse, qTrue, qFT, qFmT :: Vec Bool
qFalse = return False
qTrue  = return True
qFT    = (1 / sqrt 2) $* (qFalse `mplus` qTrue)
qFmT   = (1 / sqrt 2) $* (qFalse `mminus` qTrue)

instance (Basis a, Basis b) => Basis (a, b) where
    basis = [(a, b) | a <- basis, b <- basis]

p1, p2, p3 :: Vec (Bool, Bool)
p1 = qFT <*> qFalse
p2 = qFalse <*> qFT
p3 = qFT <*> qFT

epr :: (Bool, Bool) -> PA
epr (False, False) = 1 / sqrt 2
epr (True,  True)  = 1 / sqrt 2
epr _              = 0

-- linear operators --

type Lin a b = a -> Vec b

fun2lin :: (Basis a, Basis b) => (a -> b) -> Lin a b
fun2lin f a = return (f a)

qnot :: Lin Bool Bool
qnot = fun2lin not

phase :: Lin Bool Bool
phase False = return False
phase True  = (0 <+> 1) $* return True

z :: Lin Bool Bool
z False = return False
z True  = (-1) $* return True

hadamard :: Lin Bool Bool
hadamard False = qFT
hadamard True  = qFmT

controlled :: Basis a => Lin a a -> Lin (Bool, a) (Bool, a)
controlled f (b1, b2) = return b1 <*> (if b1 then f b2 else return b2)

adjoint :: Lin a b -> Lin b a
adjoint f b a = conjugate (f a b)

(>*<) :: Basis a => Vec a -> Vec a -> Lin a a
(v1 >*< v2) a1 a2 = v1 a1 * conjugate (v2 a2)

linplus :: (Basis a, Basis b) => Lin a b -> Lin a b -> Lin a b
linplus f g a = f a `mplus` g a

lintens :: (Basis a, Basis b, Basis c, Basis d) => Lin a b -> Lin c d -> Lin (a, c) (b, d)
lintens f g (a, c) = f a <*> g c

o :: (Basis a, Basis b, Basis c) => Lin a b -> Lin b c -> Lin a c
o f g a = f a >>= g

-- toffoli circuit --

toffoli :: Lin (Bool,Bool,Bool) (Bool,Bool,Bool)
toffoli (top, middle, bottom) =
    let cnot = controlled qnot
        cphase = controlled phase
        caphase = controlled (adjoint phase)
    in hadamard bottom >>= \b1 ->
       cphase (middle, b1) >>= \(m1, b2) ->
       cnot (top, m1) >>= \(t1, m2) ->
       caphase (m2, b2) >>= \(m3, b3) ->
       cnot (t1, m3) >>= \(t2, m4) ->
       cphase (t2, b3) >>= \(t3, b4) ->
       hadamard b4 >>= \b5 ->
       \b -> if (t3, m4, b5) == b then 1 else 0

-- density matrices --

type Dens a = Vec (a, a)

pureD :: Basis a => Vec a -> Dens a
pureD v = lin2vec (v >*< v)

lin2vec :: (a -> Vec b) -> Vec (a, b)
lin2vec = uncurry

-- superoperators --

type Super a b = (a, a) -> Dens b

lin2super :: (Basis a, Basis b) => Lin a b -> Super a b
lin2super f (a1, a2) = lin2vec (f a1 >*< f a2)

trL :: (Basis a, Basis b) => Super (a, b) b
trL ((a1, b1), (a2, b2)) = if a1 == a2 then return (b1, b2) else mzero

meas :: Basis a => Super a (a, a)
meas (a1, a2) = if a1 == a2 then return ((a1, a1), (a1, a1)) else mzero

-- superoperators as arrows --

first :: (Basis b, Basis c, Basis d) => Super b c -> Super (b, d) (c, d)
first f ((b1, d1), (b2, d2)) =
    let v = f (b1, b2) <*> return (d1, d2)
    in \((c1, d1'), (c2, d2')) -> v ((c1, c2), (d1', d2'))

-- toffoli via arrows --

toffoli1 :: Super (Bool, Bool, Bool) (Bool, Bool, Bool)
toffoli1 =
    let hadS    = lin2super hadamard
        cnotS   = lin2super (controlled qnot)
        cphaseS = lin2super (controlled phase)
        caphaseS = lin2super (controlled (adjoint phase))
    in proc (a0, b0, c0) -> do
        c1        <- hadS -< c0
        (b1, c2)  <- cphaseS -< (b0, c1)
        (a1, b2)  <- cnotS -< (a0, b1)
        (b3, c3)  <- caphaseS -< (b2, c2)
        (a2, b4)  <- cnotS -< (a1, b3)
        (a3, c4)  <- cphaseS -< (a2, c3)
        c5        <- hadS -< c4
        returnA -< (a3, b4, c5)

-- teleportation protocol --

alice :: Super (Bool, Bool) (Bool, Bool)
alice = proc (eprL, q) -> do
    (q1, e1)     <- lin2super (controlled qnot) -< (q, eprL)
    q2           <- lin2super hadamard -< q1
    ((q3, e2), m) <- meas -< (q2, e1)
    m'           <- trL -< ((q3, e2), m)
    returnA -< m'

bob :: Super (Bool, Bool, Bool) Bool
bob = proc (eprR, m1, m2) -> do
    (m2', e1) <- lin2super (controlled qnot) -< (m2, eprR)
    (m1', e2) <- lin2super (controlled z) -< (m1, e1)
    q         <- trL -< ((m1', m2'), e2)
    returnA -< q

teleport :: Super (Bool, Bool, Bool) Bool
teleport = proc (eprL, eprR, q) -> do
    (m1, m2) <- alice -< (eprL, q)
    q'       <- bob -< (eprR, m1, m2)
    returnA -< q'

-- main

main :: IO ()
main = putStrLn "program compiled successfully"
