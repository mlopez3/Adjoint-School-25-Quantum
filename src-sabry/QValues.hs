module QValues where

import Data.List
import Data.Complex 
import Data.Map    as Map

class (Eq a, Ord a) => Basis a where
    basis :: [a]


instance Basis Bool where
    basis = [False,True]

instance (Basis a, Basis b) => Basis(a,b) where 
    basis = [(a,b)| a <- basis, b <- basis]   

type PA = Complex Double
type QV a = Map a PA

qv :: Basis a => [(a,PA)] -> QV a
qv = fromList

pr :: Basis a => Map a PA -> a -> PA
pr m k = findWithDefault 0 k m

qFalse, qTrue, qFT :: QV Bool
qFalse = qv [(False, 1)]
qTrue = qv [(True,1)]
qFT = qv [(False,1/sqrt(2)),(True, 1/sqrt(2))]

p1,p2,p3 :: QV (Bool,Bool)
p1 = qv [((False,False),1),((False,True),1)]

p2 = qv [((False,False),1),((False,False),1)]

p3 = qv [((False,False),1),
         ((False,True),1),
         ((True, False),1),
         ((True,True),1)]

-- tensor product: multiple quantum systems
(&*) :: (Basis a, Basis b) => QV a -> QV b -> QV (a,b)
qa &* qb = qv [((a,b), pr qa a * pr qb b) | (a,b) <- basis]

-- quantum operations
qnot :: QV Bool -> QV Bool
qnot v = qv[(False, pr v True),
            (True, pr v False)]

hadamard :: QV Bool -> QV Bool
hadamard v = let alpha = pr v False
                 beta = pr v True
             in qv[(False,alpha+beta),(True,alpha-beta)]

data Qop a b = Qop(Map (a,b) PA)

qop :: (Basis a, Basis b) => [((a,b),PA)] -> Qop a b
qop = Qop . fromList

qApp :: (Basis a, Basis b) => Qop a b -> QV a -> QV b
qApp = undefined