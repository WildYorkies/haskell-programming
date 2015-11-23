-- givenType.hs
module GivenType where

--6. Only one version that will typecheck.
let co::(b->c)->(a->b)->(a->c)
co bToC aToB = aToC 
  where aToC x = bToC (aToB x)

--7. One version will typecheck.
let a::(a->c)->a->a
a aToC x = x

--8. One version will typecheck.
let a'::(a->b)->a->b
a' aToB x = aToB x
