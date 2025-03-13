-- definition of vector --
import Data.Complex (Complex, conjugate) -- importing complex number type for line 3
import Prelude hiding (return, (<*>),(>>=)) -- avoid conflicts with haskell using it's own return function from the prelude, allowing us to use our custom return function, and it's definition of <*>
class Eq a => Basis a where basis :: [a]   -- Define the class "Basis", saying that every element in "basis" is an instance of "Eq" and is a list of elements of type "a"
type PA = Complex Double  -- Type alias for Complex Double
type Vec a = a -> PA  -- A vector of type "a" returns a Complex Double

-- monad properties -- 
return :: Basis a => a -> Vec a  
return a b = if a == b then 1 else 0  -- If "a" equals "b", return 1, else return 0

(>>=) :: Basis a => Vec a -> (a -> Vec b) -> Vec b  -- defined as a part of monad-like behavior
va >>= f = \b -> sum [(va a) * (f a b) | a <- basis]  -- apply "f" to each basis element and sum the results, <- is sort of like an inclusion operation here

-- operations in vector space --
mzero :: Vec a -- defining zero vector
mzero = const 0 -- defining a function that always returns zero
mplus :: Vec a -> Vec a -> Vec a -- adds 2 vectors of type A
mplus v_1 v_2 a = v_1 a + v_2 a -- defined as sum of corresponding components
mminus :: Vec a -> Vec a -> Vec a -- subtracts 2 vectors of type A
mminus v_1 v_2 a = v_1 a - v_2 a -- defined as subtraction of corresponding components

-- products in vector space --

($*) :: PA -> Vec a -> Vec a -- defines scalar multiplication on vectors
pa $* v = \a -> pa * v a -- scalar multiplication is implemented by multiplying each component of va by the scalar

(<*>) :: Vec a -> Vec b -> Vec (a,b) -- defines tensor product on vectors 
v1 <*> v2 = \(a,b) -> v1 a * v2 b -- multiplies the components in v1 with their respective components in v2

(<.>) :: Basis a => Vec a -> Vec a -> PA -- defines the dot product, which gives a scalar result
v1 <.> v2 = sum(map(\a -> conjugate (v1 a)* (v2 a)) basis) -- computes dot product by summing over basis elements, multiplying the conjugate of each element in v1 with the corresponding component in v2

(<+>) :: a -> a -> Complex a
r <+> i = r <+> i

-- examples of vectors over the set of booleans --

instance Basis Bool where basis = [False, True] -- defined the basis in the set of booleans
qFalse,qTrue,qFT,qFmT :: Vec Bool -- defining operations in vector space of booleans
qFalse = return False 
qTrue = return True
qFT = (1 / sqrt 2) $* (qFalse `mplus` qTrue) -- superposition defined with +
qFmT = (1 / sqrt 2) $* (qFalse `mminus` qTrue) -- superposition defined with -

instance (Basis a, Basis b) => Basis (a, b) where -- definition of instance of basis
    basis :: (Basis a, Basis b) => [(a, b)]
    basis = [(a, b) | a <- basis, b <- basis]

p1,p2,p3:: Vec(Bool, Bool) -- defining vectors 

p1 = qFT <*> qFalse -- tensor product of qFT and qFalse
p2 = qFalse <*> qFT -- tensor product of qFalse and qFT
p3 = qFT <*> qFT -- tensor product of qFT and qFT

epr :: (Bool, Bool) -> PA -- entangled states, epr takes a tuple and returns a complex number
epr (False,False) = 1 / sqrt 2
epr (True,True) = 1 / sqrt 2
epr _             = 0 -- for all other combinations of true and false

-- defining linear operators --

type Lin a b = a -> Vec b -- linear function from a to a vector b

fun2lin :: (Basis a, Basis b) => (a -> b) -> Lin a b -- takes a regular function and turns it to a linear function
fun2lin f a = return (f a) -- applied the function to a and returns it as a vector b

qnot :: Lin Bool Bool -- quantum version of boolean negation
qnot = fun2lin (\x -> if x then False else True)

phase :: Lin Bool Bool -- linear function from bool to bool
phase False = return False -- for input false, returns false as a vector
phase True = (0 <+> 1) $* (return True) -- for true returns complex scalar multiplication of return True

hadamard :: Lin Bool Bool -- linear function from bool to bool
hadamard False = qFT -- quantum fourier transform
hadamard True = qFmT

controlled :: Basis a => Lin a a -> Lin (Bool,a) (Bool,a) -- linear function that takes a function and applies it to a bool based tuple
controlled f (b1,b2) = (return b1) <*> (if b1 then f b2 else return b2) -- applied f to b2 if b1 is true otherwise b2 remains unchanged

adjoint :: Lin a b -> Lin b a -- conjugates the result of applying f to the inputs of a and b
adjoint f b a = conjugate (f a b)
(>*<) :: Basis a => Vec a -> Vec a -> Lin a a -- inner product definition
(v1 >*< v2) a1 a2 = v1 a1 * conjugate (v2 a2)
linplus :: (Basis a, Basis b) => Lin a b -> Lin a b -> Lin a b -- summing linear functions
linplus f g a = f a `mplus` g a
lintens :: (Basis a, Basis b, Basis c, Basis d) => Lin a b -> Lin c d -> Lin (a,c) (b,d) -- tensor product of linear functions
lintens f g (a,c) = f a <*> g c
o :: (Basis a, Basis b, Basis c) => Lin a b -> Lin b c -> Lin a c -- composition of linear functions
o f g a = (f a >>= g)

-- toffoli circuit example --

toffoli :: Lin (Bool,Bool,Bool) (Bool,Bool,Bool)
toffoli (top,middle,bottom) =
    let cnot = controlled qnot
        cphase = controlled phase
        caphase = controlled (adjoint phase)
    in hadamard bottom >>= \b1 ->
        cphase (middle, b1) >>= \(m1,b2) ->
        cnot (top,m1) >>= \(t1,m2) ->
        caphase (m2,b2) >>= \(m3,b3) ->
        cnot (t1,m3) >>= \(t2,m4) ->
        cphase (t2,b3) >>= \(t3,b4) ->
        hadamard b4 >>= \b5 ->
        \ b -> if (t3, m4, b5) == b then 1 else 0


-- main function to test program --
main :: IO () 
main = do
    putStrLn "program compiled successfully"