--module Ch12 where

--import Data.List

----isJust :: Maybe a -> Bool
----isJust x = case x of
----  Nothing -> False
----  otherwise -> True

----isNothing :: Maybe a -> Bool
----isNothing x = case x of
----  Nothing -> True
----  otherwise -> False

----mayybee :: b -> (a->b) -> Maybe a -> b
----mayybee fallback f val = case val of 
----  Nothing -> fallback
----  Just x -> f val

----fromMaybe :: a -> Maybe a -> a
----fromMaybe fallback val = mayybee fallback id val

----listToMaybe :: [a] -> Maybe a
----listToMaybe xs = case val of 
----  [] -> Nothing
----  (x:xs) -> x

----maybeToList :: [a] -> Maybe a
----maybeToList x = mayybee [] (\x -> [x]) val

----catMaybes :: [Maybe a] -> [a]
----catMaybes [] = []
----catMaybes xs = case xs of
----  Nothing:xs -> catMaybes xs
----  (Just a):xs -> a:(catMaybes xs)

----lefts' :: [Either a b] -> [a]
----lefts' xs = case xs of 
----  (Left x):xs -> x:(lefts' xs)
----  (Right x):xs -> lefts' xs

----rights' :: [Either a b] -> [b]
----rights' xs = case xs of 
----  (Left x):xs -> rights' xs
----  (Right x):xs -> x:(rights' xs)

----partitionEithers' :: [Either a b] -> ([a], [b])
----partitionEithers' xs = (lefts' xs, rights' xs)

----eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
----eitherMaybe' f e = case xs of
----  Left x -> Nothing
----  Right x -> f x

----either' :: (a -> c) -> (b -> c) -> Either a b -> c
----either' f g e case e of
----  Left x -> f x
----  Right x -> g x

----eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
----eitherMaybe'' g e = case e of 
----  Left x -> Nothing 
----  Right x -> either' g e

----myIterate :: (a -> a) -> a -> [a] 
----myIterate f x = x:(myIterate f $ f x)

--myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
--myUnfoldr f x = case g of
--  Nothing -> []
--  Just (a, b)  -> a:(myUnfoldr f b)
--  where g = f x

----betterIterate :: (a -> a) -> a -> [a] 
----betterIterate f b = myUnfoldr (\b -> Just (b, f b)) b 

--data BinaryTree a = 
--  Leaf
--  | Node (BinaryTree a) a (BinaryTree a) 
--  deriving (Eq, Ord, Show)

--unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b 
--unfold f x = case f x of 
--  Nothing -> Leaf
--  Just (a, b, a') -> Node (unfold f a) b (unfold f a')

--treeBuild :: Integer -> BinaryTree Integer
--treeBuild n = unfold f 0
--  where f :: (Integer -> Maybe (Integer, Integer, Integer))
--        f x = if x == n + 1
--          then Nothing
--          else Just (x + 1, x, x + 1)


--module Ch12 where

--import Data.List

--data Optional a = Nada | Only a deriving (Eq, Show)

--instance Monoid a => Monoid (Optional a) where
--  mempty = Nada
--  mappend a b = case (a,b) of 
--    (Nada, Nada) -> mempty
--    (Only x, Nada) -> Only x
--    (Nada, Only y) ->  Only y
--    (Only x, Only y) -> Only (mappend x y)


--import Data.Monoid
--import Test.QuickCheck
--import Data.Semigroup
--import Test.QuickCheck

--data Trivial = Trivial deriving (Eq, Show)

--instance Semigroup Trivial where
--  _ <> _ = Trivial

--instance Arbitrary Trivial where
--  arbitrary = return Trivial

--semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
--semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

--type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

--main :: IO () 
--main = quickCheck (semigroupAssoc :: TrivAssoc)

--import Data.Semigroup
--import Test.QuickCheck

--data Identity a = Identity a deriving (Eq, Show)

--instance Semigroup Identity a where
--  Identity x <> Identity y = Identity (x <> y)

--instance Arbitrary Identity a where
--  arbitrary = return Identity a

--semigroupAssoc :: (Eq m, Semigroup m) =>m->m->m->Bool
--semigroupAssoc a b c = (a<>(b<>c))==((a<>b)<>c)

--type IdentAssoc = Identity a -> Identity b -> Identity c -> Bool

--main :: IO () 
--main = quickCheck (semigroupAssoc :: IdentAssoc)

---- a = (+1) $ read "[1]" :: [Int]
--fmap (+1) $ read "[1]" :: [Int]

---- b = (++ "lol") (Just ["Hi,", "Hello"])
--(fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

---- c=(*2)(\x->x-2)
-- ((* 2) . (\x -> x - 2)) 1

---- e :: IO Integer
---- e = let ioi = readIO "1" :: IO Integer
----         changed = read ("123"++) show ioi 
----     in (*3) changed

--let ioi = readIO "1" :: IO Integer
--          changed = fmap ("123"++) (fmap show ioi)
--    in fmap (*3) changed

{-# LANGUAGE ViewPatterns #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => 
                    f a 
                    -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => 
                  (a -> b) 
                  -> (b -> c) 
                  -> f a 
                  -> Bool
functorCompose f g x = 
  (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

type IntToInt = Fun Int Int


instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  quickCheck $ \x -> functorIdentity (x :: Identity Int)
  quickCheck $ \x -> functorIdentity (x :: Arbitrary Int Int)

