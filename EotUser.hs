{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module EotUser where
import Generics.Eot

class Diff a where
  type family Patch a :: *
  type Patch a = GPatch (Eot a)

  diff :: a -> a -> Patch a
  default diff :: (HasEot a, GDiff (Eot a), Patch a ~ GPatch (Eot a)) => a -> a -> Patch a
  diff a a' = (gdiff (toEot a) (toEot a'))

class GDiff a where
  type family GPatch a :: *
  gdiff :: a -> a -> GPatch a

instance GDiff () where
  type  GPatch () = ()
  gdiff () () = ()

instance GDiff Void where
  type  GPatch Void = Void
  gdiff v = absurd v

instance (Diff a, GDiff b) => GDiff (a, b)  where
  type GPatch (a, b) = (Patch a, GPatch b)
  gdiff (a, b) (a', b') = (diff a a', gdiff b b')

instance (GDiff a, GDiff b) => GDiff (Either a b)  where
  type GPatch (Either a b) = (Either (Either a b) (Either (GPatch a) (GPatch b)))
  gdiff (Left a) (Left a') = Right (Left (gdiff a a'))
  gdiff (Right a) (Right a') = Right (Right (gdiff a a'))


instance Diff ()  where
instance Diff Void where

data DeltaNum a = DeltaNum a deriving(Show)
instance Diff Int where
  type Patch Int = DeltaNum Int
  diff a a' = DeltaNum $ a' - a
instance (Diff a, Diff b) => Diff (a, b) where
instance (Diff a, Diff b) => Diff (Either a b) where

instance Diff Bool where

-- This fucks up, unfortunately. Need to teach GHC how to deal.
-- instance Diff [Int] where


ii :: (Int, Int)
ii = (5, 4)
jj :: (Int, Int)
jj = (3, 2)

diijj = diff ii jj

bb :: (Bool, Bool)
bb = (True, True)
bb' :: (Bool, Bool)
bb' = (False, True)

dbb = diff bb bb'

xs :: [Int]
xs = [3, 4]

xs' :: [Int]
xs' = [4, 5]

-- dxs = diff xs xs'


main :: IO  ()
main = undefined
