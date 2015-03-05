-- Author: Stefan Knudsen
-- Description: Matrix multiplication for quantum circuits

-- m' is a matrix, m is a function that applies m'

-- TODO
-- change floating point
-- display vectors with constants factored out
-- split into multiple files

import Data.Matrix
import Data.Bits
import Data.Char
import Data.Complex
import Numeric

o,l,p,m,oo,ol,lo,ll,phip,psip,psim :: Num a => Matrix a
i,x,z,h,cnot,cz :: Num a => Matrix a -> Matrix a
i',x',z',h',cz',cnot' :: Num a => Matrix a
-- o,l,p,m,oo,ol,lo,ll,phip,psip,psim :: RealFloat a => Matrix a
-- i,x,z,h,cnot,cz :: RealFloat a => Matrix a -> Matrix a
-- i',x',z',h',cz',cnot' :: RealFloat a => Matrix a

-- print x = putStr $ show x
-- mult xs = foldr1 multStd xs

normFactor :: Num a => a -> a
normFactor = (*1)
-- Replace with the line below to normalize 2 qubit matrices and vectors
-- normFactor :: (RealFloat a) => a -> a
-- normFactor = (/sqrt 2)

toKet :: Int -> [a] -> Matrix a
toKet x xs = fromList x 1 xs
toBra x xs = fromList 1 x xs
t = transpose

-- Vectors
o = toKet 2 [1,0]			-- |0>
l = toKet 2 [0,1]			-- |1>
p = toKet 2 $ map normFactor [1,1]	-- |+>
m = toKet 2 $ map normFactor [1,-1]	-- |->

-- Bell states
phip = toKet 4 $ map normFactor [1,0,0,1]
phim = toKet 4 $ map normFactor [1,0,0,-1]
psip = toKet 4 $ map normFactor [0,1,1,0]
psim = toKet 4 $ map normFactor [0,1,-1,0]

-- Inner and outer product on 2 kets
inner k1 k2 = multStd (transpose k1) k2
outer k1 k2 = multStd k1 (transpose k2)

-- 2 Qubit Gates
x = multStd $ x'
x' = fromList 2 2 [0,1,
                  1,0]
z = multStd $ z'
z' = fromList 2 2 [1,0,
                  0,-1]
h = multStd $ h'
h' = fromList 2 2 $ map normFactor [1,1,1,-1]

i = multStd $ i'
i' = identity 2

cnot12 = cnot1
cnot1 = cnot
cnot = multStd $ cnot'
cnot' = fromList 4 4 [1,0,0,0,
                      0,1,0,0,
                      0,0,0,1,
                      0,0,1,0]
cnot21 = cnot2
cnot2 = multStd cnot2'
cnot2' = fromList 4 4 [1,0,0,0,
                       0,0,0,1,
                       0,0,1,0,
                       0,1,0,0]
swap = cnot12.cnot21.cnot12

cz = multStd cz'
cz' = fromList 4 4 [1,0,0,0,
                    0,1,0,0,
                    0,0,1,0,
                    0,0,0,-1]
ch12 = ch1
ch1 = ch
ch = multStd ch'
ch' = fromList 4 4 [1,0,0,0,
                    0,1,0,0,
                    0,0,normFactor 1,normFactor 1,
                    0,0,normFactor 1, normFactor (-1)]

--ch2 = swap.ch.swap
ch21 = ch2
ch2 = ch2'
ch2' = fromList 4 4 [1,	0,		0,	0,
                     0,	normFactor 1,	0,	normFactor 1,
                     0,	0,		1,	0,
                     0,	normFactor 1,	0,	normFactor (-1)]


-- Common 2 qubit vectors
oo = k' o o
ol = k' o l
op = k' o p
om = k' o m
lo = k' l o
ll = k' l l
lp = k' l p
lm = k' l m
po = k' p o
pl = k' p l
pp = k' p p
pm = k' p m
mo = k' m o
ml = k' m l
mp = k' m p
mm = k' m m

-- Basis state matrices
ii = k' i' i'
hh = k' h' h'

---------------- Matrix functions ----------------
-- k is a functions that tries to multiply by whatever it to its right
k a b = multStd $ k' a b

-- The matrix or vector equivalent is k'
k' :: Num a => Matrix a -> Matrix a -> Matrix a
k' a b =
  let tensorFunc (i,j) = a!((i-1) `quot` (nrows b) + 1, (j-1) `quot` (ncols b) + 1) *
                         b!((i-1) `mod` (nrows b) + 1, (j-1) `mod` (ncols b) + 1)
  in
   matrix ((nrows a)*(nrows b)) ((ncols a)*(ncols b)) tensorFunc


-- class (Num a) => Control a b where
--   c :: Matrix a -> Int -> b -> Int -> Matrix a

-- instance Control Int where
--   c :: Matrix a -> Int -> Int -> Int -> Matrix a

-- instance Control [Int] where
--   c :: Matrix a -> Int -> [Int] -> Int -> Matrix a

-- Control gate for more than 2 qubits
-- Takes matrix to apply, control bit, and output bit
--c :: Num a => Matrix a -> Int -> Int -> Int -> Matrix a
c g' n x y = matrix (2^n) (2^n) (control g' n (n - x) y)
  where
    -- If the control bit is on, get the entry for an identity matrix, otherwise treat it as a tensor product of with gate g at the y bit
    -- Takes a coordinate pairs and returns the matrix value at that coordinate
    --control :: RealFloat a => Matrix a -> Int -> Int -> Int -> (Int,Int) -> a
    control :: Num a => Matrix a -> Int -> Int -> Int -> (Int,Int) -> a
    control g' n x y (i,j)
      | not (bitIsOne x i j) && i == j = 1
      | not (bitIsOne x i j) = 0
      | otherwise = (putGate g' n y)!(i,j)
      where
        -- checks if the control bit is on
        bitIsOne :: Int -> Int -> Int -> Bool
        bitIsOne c i j = ((i-1) .&. (j-1) .&. 2^c) /= 0


-- TODO: turn cs and c into one function through typeclassing
-- Control gate for more than 2 qubits with multiple control bits
--cs :: Num a => Matrix a -> Int -> [Int] -> Int -> Matrix a
cs g' n xs y = matrix (2^n) (2^n) (control g' n xs y)
  where
    -- If all control bits are on, make and identity matrix, otherwise treat it as a tensor product of with gate g' at the y bit
    control g' n xs y (i,j)
      | not (bitsAnded xs i j) && i == j = 1
      | not (bitsAnded xs i j) = 0
      | otherwise = (putGate g' n y)!(i,j)
      where
        -- Checks if all control bits are on
--        bitsAnded :: (Bits a, Num a) => [Int] -> a -> a -> Bool
        bitsAnded xs i j =
          foldr (\x acc -> acc && (((i-1) .&. (j-1) .&. 2^(n-x)) /= 0)) True xs

-- Tensor product for n bits with a gate g' at position c (identity elsewhere)
-- change to tail recursive?
--putGate :: RealFloat a => Matrix a -> Int -> Int -> Matrix a
putGate :: Num a => Matrix a -> Int -> Int -> Matrix a
putGate g' n c
  | n < 1 = error "Second argument must be at least 1"
  | n == 1 = newGate
  | otherwise = k' (putGate g' (n-1) c) newGate
      where
        newGate
          | c /= n = i'
          | otherwise = g'


-- Display a column vector in ket notation
ket :: (Show a, Ord a, Num a) => Matrix a -> String
ket m
  | ncols m > 1 = error "Must be a column vector"
  | otherwise = tail $ ket1 m (nrows m)
  where
    ket1 m n
      | n == 0 = ""
    -- don't show the term if the constant is 0
      | cst == 0 = ket1 m (n-1)
    -- don't show the constant if it's 1 or -1
      | abs cst == 1 = ket1 m (n-1) ++ sign ++ "|" ++ binRep n ++ ">"
      | otherwise = ket1 m (n-1) ++ sign ++ showCst ++ "|" ++ binRep n ++ ">"
      where showCst = show $ abs $ cst
            cst = m!(n,1)
            binRep i = (take diff $ repeat '0') ++ toBinary (i-1)
            toBinary i = showIntAtBase 2 intToDigit i ""
            -- allows for padding
            diff = length (toBinary (nrows m)) - length (toBinary (n-1)) - 1
            sign
              | cst > 0 = " + "
              | otherwise = " - "

bra :: (Show a, Ord a, Num a) => Matrix a -> String
bra m
  | nrows m > 1 = error "Must be a row vector"
  | otherwise =
      let
        repl '>' = '|'
        repl '|' = '<'
        repl c = c
      in
       map repl $ ket $ t m

--f :: RealFloat a => Int -> Matrix (Complex a)
-- Creates the n-dimensional Fourier matrix
-- F_{jk} = e^{2*pi*i*j*k/n}, where j,k start at 0
--f n = matrix n n (\(j,k) -> exp (fromIntegral (2*(j-1)*(k-1)) * pi * (0:+1)/fromIntegral n))
-- to normalize: (/sqrt n) $ 

-- from a3q3
-- permR = multStd $ permR'
-- permR' = fromList 6 6 [1:+0,0:+0,0:+0,0:+0,0:+0,0:+0,
--                        0:+0,0:+0,0:+0,0:+0,1:+0,0:+0,
--                        0:+0,0:+0,1:+0,0:+0,0:+0,0:+0,
--                        0:+0,0:+0,0:+0,1:+0,0:+0,0:+0,
--                        0:+0,1:+0,0:+0,0:+0,0:+0,0:+0,
--                        0:+0,0:+0,0:+0,0:+0,0:+0,1:+0]

-- miniPerm = multStd $ miniPerm'
-- miniPerm' = fromList 6 6 [1:+0,0:+0,0:+0,0:+0,0:+0,0:+0,
--                           0:+0,0:+0,1:+0,0:+0,0:+0,0:+0,
--                           0:+0,1:+0,0:+0,0:+0,0:+0,0:+0,
--                           0:+0,0:+0,0:+0,1:+0,0:+0,0:+0,
--                           0:+0,0:+0,0:+0,0:+0,0:+0,1:+0,
--                           0:+0,0:+0,0:+0,0:+0,1:+0,0:+0]

--a = permR $ k (f 2) (f 3) $ permR'
-- let a = permR $ k (f 2) (f 3) $ miniPerm $ permR'
--         a - (f 6)


q3p = multStd $ miniPerm $ permR'

q3q = multStd $ permR'

-- P =
-- ( 1 0 0 0 0 0 )
-- ( 0 0 0 0 1 0 )
-- ( 0 0 1 0 0 0 )
-- ( 0 0 0 1 0 0 )
-- ( 0 1 0 0 0 0 )
-- ( 0 0 0 0 0 1 )

-- Q =
-- ( 1 0 0 0 0 0 )
-- ( 0 0 1 0 0 0 )
-- ( 0 0 0 0 1 0 )
-- ( 0 0 0 1 0 0 )
-- ( 0 0 0 0 0 1 )
-- ( 0 1 0 0 0 0 )

-- base 6 vectors
s0 = toKet 6 [1,0,0,0,0,0]
s1 = toKet 6 [0,1,0,0,0,0]
s2 = toKet 6 [0,0,1,0,0,0]
s3 = toKet 6 [0,0,0,1,0,0]
s4 = toKet 6 [0,0,0,0,1,0]
s5 = toKet 6 [0,0,0,0,0,1]

permR = multStd $ permR'
permR' = fromList 6 6 [1,0,0,0,0,0,
                       0,0,0,0,1,0,
                       0,0,1,0,0,0,
                       0,0,0,1,0,0,
                       0,1,0,0,0,0,
                       0,0,0,0,0,1]

miniPerm = multStd $ miniPerm'
miniPerm' = fromList 6 6 [1,0,0,0,0,0,
                          0,0,1,0,0,0,
                          0,1,0,0,0,0,
                          0,0,0,1,0,0,
                          0,0,0,0,0,1,
                          0,0,0,0,1,0]
