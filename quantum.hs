-- https://hackage.haskell.org/package/matrix-0.2.1/docs/Data-Matrix.html
-- x' is a matrix, x is a function that applies x'

-- TODO
-- add precedence, maybe with $
-- improve tensor vector syntax for more than 2 qubits
-- showKet x
-- change floating point
-- factor out constants

import Data.Matrix
import Numeric

i,x,z,h,o,l,p,m,oo,ol,lo,ll,phip,psip,psim :: Matrix Double
cnot,cz :: Matrix Double -> Matrix Double

print x = putStr $ show x

ket x xs = fromList x 1 xs
bra x xs = fromList 1 x xs

i = identity 2
x = fromList 2 2 [0,1,1,0]
--x = multStd $ x'
z = fromList 2 2 [1,0,0,-1]
--z = multStd $ z'
--h = fromList 2 2 [1,1,1,-1]
h = fromList 2 2 $ map (/sqrt 2) [1,1,1,-1]

o = ket 2 [1,0]				-- | 0 >
l = ket 2 [0,1]				-- | 1 >
p = ket 2 $ map (/sqrt 2) [1,1]		-- | + >
m = ket 2 $ map (/sqrt 2) [1,-1]	-- | - >

-- Bell states
phip = ket 4 $ map (/sqrt 2) [1,0,0,1]
phim = ket 4 $ map (/sqrt 2) [1,0,0,-1]
psip = ket 4 $ map (/sqrt 2) [0,1,1,0]
psim = ket 4 $ map (/sqrt 2) [0,1,-1,0]

-- Gates
cnot' = fromList 4 4 [1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0]
cnot = multStd $ cnot'
cnot1 = cnot
cnot12 = cnot1

cnot2' = fromList 4 4 [1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0]
cnot2 = multStd cnot2'
cnot21 = cnot2

swap = cnot12.cnot21.cnot12

cz' = fromList 4 4 [1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,-1]
cz = multStd cz'

ch' = fromList 4 4 [1,0,0,0,0,1,0,0,0,0,1/sqrt 2,1/sqrt 2,0,0,1/sqrt 2, -1/sqrt 2]
ch = multStd ch'
ch1 = ch
ch12 = ch1

--ch2' = fromList 4 4 [1,1/sqrt 2,0,0,0,1/sqrt 2,0,0,0,0,1,1/sqrt 2,0,0,0,-1/sqrt 2]
ch2 = swap.ch.swap
ch21 = ch2
--ch = fromList 4 4 [1,0,0,0,0,1,0,0,0,0,1,1,0,0,1,-1]

-- k' a b = matrix ((nrows a)*(nrows b)) ((ncols a)*(ncols b)) tensorFunc
--   where tensorFunc (i,j) = a!((i-1) `quot` (nrows a) + 1, (j-1) `quot` (ncols a) + 1) *
--                            b!((i-1) `mod` (nrows b) + 1, (j-1) `mod` (ncols b) + 1)

-- k' a b = matrix ((nrows a)*(nrows b)) ((ncols a)*(ncols b)) tensorFunc
--   where tensorFunc (i,j) = a!(i `quot` (nrows a), j `quot` (ncols a)) *
--                            b!((i-1) `mod` (nrows b) + 1, (j-1) `mod` (ncols b) + 1)


k' a b = matrix ((nrows a)*(nrows b)) ((ncols a)*(ncols b)) tensorFunc
  where tensorFunc (i,j) = a!(((i-1) `quot` (nrows b))+1, ((j-1) `quot` (ncols b))+1) *
                           b!((i-1) `mod` (nrows b) + 1, (j-1) `mod` (ncols b) + 1)
                           -- b!((i `mod` (nrows a)) + 1, (j `mod` (ncols a)) + 1)
        
        
--  where tensorFunc (i,j) = a!((i `quot` (nrows b))+1, (j `quot` (ncols b))+1)
-- *b!((i `mod` (nrows a)-1)+1, (j `mod` (ncols a)-1)+1)





-- Kronecker
k a b = multStd $ k' a b

mult xs = foldr1 multStd xs

ii = k' i i
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



