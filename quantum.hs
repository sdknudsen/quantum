-- https://hackage.haskell.org/package/matrix-0.2.1/docs/Data-Matrix.html

import Data.Matrix
import Numeric

--i,x,z,h,o,l,p,m,oo,ol,lo,ll,phip,psip,psim :: Matrix Double

print x = putStr $ show x

ket x xs = fromList x 1 xs
bra x xs = fromList 1 x xs

i = identity 2
x = fromList 2 2 [0,1,1,0]
z = fromList 2 2 [1,0,0,-1]
--h = fromList 2 2 [1,1,1,-1]
h = fromList 2 2 (map (*(1/(sqrt 2)))[1,1,1,-1])

o = ket 2 [1,0]		-- | 0 >
l = ket 2 [0,1]		-- | 1 >
p = ket 2 [1,1]		-- | + >
m = ket 2 [1,-1]	-- | - >

oo = ket 4 [1,0,0,0]
ol = ket 4 [0,1,0,0]
lo = ket 4 [0,0,1,0]
ll = ket 4 [0,0,0,1]

-- bell states
phip = ket 4 [1,0,0,1]
phim = ket 4 [1,0,0,-1]
psip = ket 4 [0,1,1,0]
psim = ket 4 [0,1,-1,0]

cnot = fromList 4 4 [1,0,0,0,0,1,0,0,0,0,0,1,0,0,1,0]

tens a b = matrix ((nrows a)*(nrows b)) ((ncols a)*(ncols b)) tensorFunc
  where tensorFunc (i,j) = a!((i-1) `quot` (nrows a) + 1, (j-1) `quot` (ncols a) + 1) *
                           b!((i-1) `mod` (nrows b) + 1, (j-1) `mod` (ncols b) + 1)

mult xs = foldr1 multStd xs

-- showKet x
--   | 

-- factor out constants
