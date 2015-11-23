-- intermission.hs
module Intermission where

-- 1
data TisAnInteger =
  TisAn Integer
instance Eq TisAnInteger where
  (==) (TisAn a) (TisAn b) = a == b
-- 2
data TwoIntegers =
  Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two a b) (Two c d) = (a,b) == (c,d)
-- 3
data StringOrInt =
    TisAnInt   Int
  | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt a) (TisAnInt b) = a==b
  (==) (TisAString c) (TisAString d) = c == d
  (==) _ _ = False
-- 4
data Pair a =
  Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x==x' && y==y'
-- 5
data Tuple a b =
  Tuple a b
instance (Eq a,Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = (x,y) == (x',y')
-- 6
data Which a =
    ThisOne a
  | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne y) = x==y
  (==) (ThatOne x') (ThatOne y') = x'==y'
  (==) _ _ = False
-- 7
data EitherOr a b =
    Hello a
  | Goodbye b
instance (Eq a,Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello y) = x==y
  (==) (Goodbye x') (Goodbye y') = x'==y'
  (==) _ _ = False
