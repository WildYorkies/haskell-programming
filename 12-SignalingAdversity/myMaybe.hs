-- myMaybe.hs
module MyMaybe where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing  = True

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee acc f Nothing  = acc
mayybee acc f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe x (Just y) = y

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe l  = Just (head l)

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes []     = []
catMaybes (x:xs) = case x of Just y  -> y : catMaybes xs
                             Nothing -> catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = undefined

-- EITHER TIME 

lefts' :: [Either a b] -> [a]
lefts' l = foldr (\a b -> case a of Left x  -> x : b
                                    Right x -> b    )
                 []
                 l 

rights' :: [Either a b] -> [b]
rights' l = foldr (\a b -> case a of Left x  -> b
                                     Right x -> x : b )
                  []
                  l 

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' l = (left, right)
  where left  = lefts' l
        right = rights' l

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left x)  = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' leftF rightF (Left x)  = leftF x
either' leftF rightF (Right x) = rightF x






