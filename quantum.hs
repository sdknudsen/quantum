-- Author: Stefan Knudsen
-- Description: Matrix multiplication with added features for quantum gates, targeted for 2 qubits

-- https://hackage.haskell.org/package/matrix-0.2.1/docs/Data-Matrix.html
-- x' is a matrix, x is a function that applies x'

-- TODO
-- improve tensor vector syntax for more than 2 qubits
-- change floating point
-- factor out constants

import Data.Matrix
import Data.Bits
import Data.Char
import Numeric

-- print x = putStr $ show x
-- mult xs = foldr1 multStd xs

-- normFactor = (/sqrt 2)
-- o,l,p,m,oo,ol,lo,ll,phip,psip,psim :: Matrix Double
-- i,x,z,h,cnot,cz :: Matrix Double -> Matrix Double
-- c :: Matrix Double -> Int -> Int -> Matrix Double
-- Replace with the lines above to normalize 2 qubit matrices and vectors
normFactor = (*1)
o,l,p,m,oo,ol,lo,ll,phip,psip,psim :: Matrix Int
i,x,z,h,cnot,cz :: Matrix Int -> Matrix Int

toKet x xs = fromList x 1 xs
toBra x xs = fromList 1 x xs
t = transpose

-- Vectors
o = toKet 2 [1,0]			-- | 0 >
l = toKet 2 [0,1]			-- | 1 >
p = toKet 2 $ map normFactor [1,1]	-- | + >
m = toKet 2 $ map normFactor [1,-1]	-- | - >

-- Bell states
phip = toKet 4 $ map normFactor [1,0,
                               0,1]
phim = toKet 4 $ map normFactor [1,0,
                               0,-1]
psip = toKet 4 $ map normFactor [0,1,
                               1,0]
psim = toKet 4 $ map normFactor [0,1,
                               -1,0]

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

-- cnot' = fromList 4 4 [1,0,0,0,
--                       0,1,0,0,
--                       0,0,3,4,
--                       0,0,4,3]

cnot21 = cnot2
cnot2 = multStd cnot2'
cnot2' = fromList 4 4 [1,0,0,0,
                       0,0,0,1,
                       0,0,1,0,
                       0,1,0,0]
-- cnot2' = fromList 4 4 [1,0,0,0,
--                        0,0,0,4,
--                        0,0,1,0,
--                        0,4,0,0]

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


-- Kronecker products are considered functions here, so they're 'hungry'
k a b = multStd $ k' a b

-- The matrix or vector equivalent is k'
k' a b =
  let tensorFunc (i,j) = a!(((i-1) `quot` (nrows b)) + 1, ((j-1) `quot` (ncols b)) + 1) *
                     b!((i-1) `mod` (nrows b) + 1, (j-1) `mod` (ncols b) + 1)
  in
   matrix ((nrows a)*(nrows b)) ((ncols a)*(ncols b)) tensorFunc


-- Control gate for more than 2 qubits
-- Takes matrix to apply, control bit, and output bit
-- TODO: allow for more than one control and one controlled bit
c :: Matrix Int -> Int -> Int -> Int -> Matrix Int
c g n x y = matrix (2^n) (2^n) (control g n (n - x) y) -- x\ ((max x y) - x + 1)
  where
    control :: Matrix Int -> Int -> Int -> Int -> (Int,Int) -> Int
    control g n x y (i,j)
      | not (bitIsOne x i j) && i == j = 1
      | not (bitIsOne x i j) = 0
      | otherwise = kList g n y!(i,j)
      where
        bitIsOne :: Int -> Int -> Int -> Bool
        bitIsOne c i j = ((i-1) .&. (j-1) .&. 2^c) /= 0
        kList g' n v
          | n < 1 = error "Second argument must be at least 1"
          | v == 1 && n == 1 = g'
          | n == 1 && n /= v = i'
          | n /= v = k' (kList g' (n-1) v) i'
          | otherwise = k' (kList g' (n-1) v) g'

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

-- Display a column vector in ket notation
ket :: Matrix Int -> String
ket m
  | ncols m > 1 = error "Must be a column vector"
  | otherwise = tail $ ket1 m (nrows m)
  where
    ket1 :: Matrix Int -> Int -> String
    ket1 m n
      | n == 0 = ""
      | cst == 0 = ket1 m (n-1)
      | abs cst == 1 = ket1 m (n-1) ++ sign ++ "|" ++ binRep n ++ ">"
      | otherwise = ket1 m (n-1) ++ sign ++ showCst ++ "|" ++ binRep n ++ ">"
      where showCst = show $ abs cst
            cst = m!(n,1)
            binRep i = (take diff $ repeat '0') ++ toBinary (i-1)
            toBinary i = showIntAtBase 2 intToDigit i ""
            diff = length (toBinary (nrows m)) - length (toBinary (n-1)) - 1
            sign
              | cst > 0 = " + "
              | otherwise = " - "

bra :: Matrix Int -> String
bra m
  | nrows m > 1 = error "Must be a row vector"
  | otherwise =
      let
        repl '>' = '|'
        repl '|' = '<'
        repl c = c
      in
       map repl $ ket $ t m
