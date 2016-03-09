-- functionType.hs
module FunctionType where

data Quad = One
	| Two
	| Three
	| Four
	deriving (Eq, Show)

-- This is a sum type of (4 + 4)
eQuad :: Either Quad Quad
eQuad = undefined

-- This is a product type of (4 * 4)
prodQuad :: (Quad, Quad)
prodQuad = undefined

-- This is a function type of b^a, so it is (4^4)
funcQuad :: Quad -> Quad
funcQuad = undefined

-- This is a product type of (3 * 3 * 3)
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined

-- function type of (c ^ b) ^ a , or c ^ (b * a)
-- so it is (2^2)^2 or 2^(2*2)
gTwo :: Bool -> Bool -> Bool
gTwo = undefined

-- function type of (c ^ b) ^ a , or c ^ (b * a)
-- so it is (4^4)^2 or 2^(4*4) or 65,536
fTwo :: Bool -> Quad -> Quad
fTwo = undefined

