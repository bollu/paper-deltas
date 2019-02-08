{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tensor where
import Generics.Eot
import Control.Applicative
import Data.Semigroup

-- Tensors over a Semigroup / Monoid

-- contains the `n` in the 2^n dimension size
newtype Dim = Dim Int deriving(Show, Eq)

-- n tensor products of (|0>, |1>), ie, tensors in the space
-- (|0>, |1>)^{(x) n}
data T2 a = T2 Dim [a] deriving(Show, Eq)

ispot :: Int -> Bool
ispot 1 = True
ispot n = if n < 0 then False
          else if n `mod` 2 == 1 
          then False 
          else ispot (n `div` 2)


mkT2 :: [a] -> T2 a 
mkT2 as = if ispot (length as)
             then T2 (Dim (floor . logBase 2 . fromIntegral $ length as)) as
             else error "invalid length, must be power of 2"

dimCheck :: Dim -> Dim -> a -> a
dimCheck (Dim d1) (Dim d2) a = if d1 == d2 then a else error "mismatched Dim"

mulDim :: Dim -> Dim -> Dim
mulDim (Dim d1) (Dim d2) = Dim (d1 + d2)

divDim :: Dim -> Dim -> Dim
divDim (Dim d1) (Dim d2) = Dim (d1 - d2)

modDim ::  Int -> Dim -> Int
modDim i (Dim d1)  = i `mod` (2^d1)

dimToInt :: Dim -> Int
dimToInt (Dim d) = 2^d


divByDimension ::  Int -> Dim -> Int
divByDimension i (Dim d1)  = i `div` (2^d1)

-- a + b
addt2 :: Semigroup a =>  T2 a -> T2 a -> T2 a 
addt2 (T2 d1 as) (T2 d2 as') = dimCheck d1 d2 $ T2 d1 (zipWith (<>) as as')

-- a (x) b
mult2 :: Semigroup a => T2 a -> T2 a -> T2 a
mult2 (T2 d1 as) (T2 d2 as') = T2 ds' (liftA2 (<>) as as') where
  ds' = mulDim d1 d2

-- Try to find a / b. eg. factorize "a" "ab" == "b", because monoid for 
-- strings is concatenation
type FactorizerFnT a = a -> a -> Maybe a

-- sample [a] by an offset offset, every dim.
-- for example,
-- 3 ["1", "2", "3", "4", "5", "6", "7", "8"] sampled at =:
--  - offset 0, Dim 0 = 2^0 = 1  will return every element
--  - offset 0, Dim 1= 2^1 =  2 will return every 2nd element
--  - offset 0, Dim 2= 2^2 =  4 will return every 4nd element
sampleEveryDim :: Dim -> [a] -> Int -> [a]
sampleEveryDim (Dim pot) xs offset = 
    (map snd) . (filter (\(i, x) -> i `mod` (2 ^ pot) == 0)) $  zip [0..] (drop offset xs)


-- factorize the 'a' at index 'Int' with the tensor 'T2 a' using the 
-- factorization function 'factorize'
-- note that [1, 2] * [a, b] = [1a, 2a, 1b, 2b]
-- so, to factorize at ix 'i', we need to know that we are factorizing with (i `divByDimension` d)
factorIx :: (a -> a -> Maybe a ) -> T2 a -> (Int, a) -> Maybe a
factorIx factorize (T2 fdim fas) (ix, a) = factorize a (fas !! (ix `divByDimension` fdim))

-- Check if all elements of a list are equal, and return that element if true
alleq :: Eq x => [x] -> Maybe x
alleq [] = Nothing
alleq (x:xs) = if all (== x) xs then (Just x) else Nothing


-- given a list of the form as * dim, reduce to as, and return Nothing otherwise
-- eg: removeDuplicateChunks (Dim 1) [a, b, a, b] === Just [a, b]
-- eg: removeDuplicateChunks (Dim 1) [a, x, b, x] === Nothing
removeDuplicateChunks :: Eq a => Dim -> [a] -> Maybe [a]
removeDuplicateChunks dim as = let chunks = map (sampleEveryDim dim as) [0..dimToInt dim-1]
                                in traverse alleq chunks
-- Try to perform a / b
-- TODO: use ReductiveMonioid for this purpose
-- http://hackage.haskell.org/package/monoid-subclasses-0.4.6.1/docs/Data-Monoid-Cancellative.html#t:GCDMonoid
divt2 :: Eq a => (a -> a -> Maybe a) -> T2 a -> T2 a -> Maybe (T2 a)
divt2 factorize (T2 d2 as') (factor@(T2 d1 _))  = T2 dimout <$> (masout >>= removeDuplicateChunks d1) where
  dimout :: Dim
  dimout = divDim d2 d1
  
 -- masout :: Maybe [a]
  masout = traverse (factorIx factorize factor) (zip [0..] as')


