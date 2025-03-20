-- definition of vector --
import Data.Complex (Complex, conjugate) -- importing complex number type for line 3
import Prelude hiding (return, (<*>),(>>=)) -- avoid conflicts with haskell using it's own return function from the prelude, allowing us to use our custom return function, and it's definition of <*>
{-# LANGUAGE Arrows #-}
import Control.Arrow(first, Arrow)
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

toffoli :: Lin (Bool,Bool,Bool) (Bool,Bool,Bool) -- defined the toffoli function with a linear type signature, taking a tuple of three boolean values and returning three boolean
toffoli (top,middle,bottom) = -- three qubits in toffoli gate
    let cnot = controlled qnot -- controlled not gate
        cphase = controlled phase -- controlled phase shift gate
        caphase = controlled (adjoint phase) -- adjoint controlled phase shift gate
    in hadamard bottom >>= \b1 -> -- hadamard gate applied to bottom qubit, creating a superposition and allowing interferance
        cphase (middle, b1) >>= \(m1,b2) -> -- cphase is applied to middle and the transformed bottom qubit (initially b1) gives b2
        cnot (top,m1) >>= \(t1,m2) -> -- cnot gate is applied to top and m1 giving t1 and m2
        caphase (m2,b2) >>= \(m3,b3) -> -- caphase gate applied t m2 and b2, giving m3 and b3
        cnot (t1,m3) >>= \(t2,m4) -> -- cnot operation applied to t1 and m3
        cphase (t2,b3) >>= \(t3,b4) -> -- cphase applied to t2 and b3
        hadamard b4 >>= \b5 -> -- final hadamard gate applied to b4
        \ b -> if (t3, m4, b5) == b then 1 else 0 -- checks if final tuple matches b returning 1 if it does and 0 otherwise


-- density matrices --

type Dens a = Vec(a,a) -- defines a density matrix as a vector in which each element is indexed by (a,a) which represents the outer product structure

pureD :: Basis a => Vec a -> Dens a -- pureD takes a vector and returns a density matrix, the point of Basis is to ensure that each vector has a well-defined basis
pureD v = lin2vec (v >*< v) -- computes the outer product of v with itself and then uses lin2vec

lin2vec :: (a-> Vec b) -> Vec(a,b) -- takes in a linear function and outputs a vector indexed over (a,b)
lin2vec = uncurry -- transforms the function of 2 arguments to a function of single tuple argument

-- superoperators --

type Super a b = (a,a) -> Dens b -- superoperator which maps density matrices to density matrices

lin2super :: (Basis a, Basis b) => Lin a b -> Super a b -- converts a linear map to a superoperator
lin2super f (a1, a2) = lin2vec(f a1 >*< f a2) -- defining f to be compatible with the outer product

-- tracing and measurement --

trL :: (Basis a, Basis b) => Super(a,b) b -- defines trL, a superoperator that traces out just the a component, leaving only b
trL ((a1,b1),(a2,b2)) = if a1 == a2 then return (b1,b2) else mzero -- if a1 and a2 are equal, then returns the pair (b1, b2), otherwise returns mzero
meas :: Basis a => Super a (a,a) -- defines meas, a superoperator that measures a in the computational basis and encodes the result as a density matrix over (a,a)
meas (a1,a2) = if a1 == a2 then return ((a1,a1),(a1,a1)) else mzero -- if a1 and a2 are equal, it returns ((a1,a1),(a1,a1)), representing classical measurement outcome. if it returns mzero (which happens in the other case), then off-diagonal terms vanish in the measurement basis



-- superoperators as arrows (note: i skipped the haskell definition of arrows as given in the paper since i kept getting multiple declaration errors for >>>, first and arr) --

arr :: (Basis b, Basis c) => (b -> c) -> Super b c -- takes a function and returns a value of type super
arr f = fun2lin (\(b1,b2) -> (f b1, f b2)) -- defn of arr function, basically takes a tuple and applies f to each element in it

(>>>) :: (Basis b, Basis c, Basis d) => Super b c -> Super c d -> Super b d -- takes two supers from b to c and c to d and combines them into a new super from b to d
(>>>) = o -- saying >>> = o, which combines two transformations sequentially

first :: (Basis b, Basis c, Basis d) => Super b c -> Super(b,d)(c,d) -- takes a super type that maps from b to c and returns a new super type that operators on tuples of (b,d) and produces tuples (c,d)
first f ((b1, d1), (b2, d2)) = permute ((f(b1,b2))<*> (return(d1,d2))) -- <*> combines results of f and passes through permute function
  where permute v ((b1,b2),(d1,d2)) = v ((b1,d1),(b2,d2)) --  defined permute function to reorder the elements so that it looks as desired for the output

-- we can replace this defn with the code in the toffoli circuit example and write it in terms of this superoperator defn --
proc :: (Arrow a) => (b -> c) -> a b c
(-<) :: Super a b -> a -> b


e1 :: Super (Bool, a) (Bool, a)
e1 = proc (a, b) → do
    r <- lin2super hadamard -≺ a
    returnA -≺ (r, b)

e2 :: Super (Bool, a) (Bool, a)
e2 = first (lin2super hadamard)

-- superoperators are probably not monads --

class Arrow a => ArrowApply a where
    app :: a (a b c, b) c

-- toffoli revisited and redefined --

toffoli1 :: Super (Bool, Bool, Bool) (Bool, Bool, Bool)
toffoli1 = 
  let hadS = lin2super hadamard
      cnotS = lin2super (controlled qnot)
      cphaseS = lin2super (controlled phase)
      caphaseS = lin2super (controlled (adjoint phase))
  in proc (a0, b0, c0) → do
      c1 <- hadS ≺ c0
      (b1, c2) <- cphaseS ≺ (b0, c1)
      (a1, b2) <- cnotS ≺ (a0, b1)
      (b3, c3) <- caphaseS ≺ (b2, c2)
      (a2, b4) <- cnotS ≺ (a1, b3)
      (a3, c4) <- cphaseS ≺ (a2, c3)
      c5 <- hadS ≺ c4
      returnA ≺ (a3, b4, c5)




-- teleportation --

alice :: Super (Bool, Bool) (Bool, Bool)
alice = proc (eprL, q) → do
    (q1, e1) <- (lin2super (controlled qnot)) -≺ (q, eprL)
    q2 <- (lin2super hadamard) -≺ q1
    ((q3, e2), (m1, m2)) <- meas -≺ (q2, e1)
    (m1', m2') <- trL ((q3, e2), (m1, m2))
    returnA -≺ (m1', m2')

bob :: Super (Bool, Bool, Bool) Bool
bob = proc (eprR, m1, m2) → do
    (m2', e1) <- (lin2super (controlled qnot)) -≺ (m2, eprR)
    (m1', e2) <- (lin2super (controlled z)) -≺ (m1, e1)
    q <- trL -≺ ((m1', m2'), e2)
    returnA -≺ q


teleport :: Super (Bool, Bool, Bool) Bool
teleport = proc (eprL, eprR, q) → do
    (m1, m2) <- alice -≺ (eprL, q)
    q <- bob -≺ (eprR, m1, m2)
    returnA -≺ q


-- main function to test program --

main :: IO () 
main = do
    putStrLn "program compiled successfully"