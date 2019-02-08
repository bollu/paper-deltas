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
{-# LANGUAGE Rank2Types #-}
module WorkingGenerics where
import GHC.Generics
import GHC.Exts (Constraint)

-- type family DiffT (p :: * -> *) :: * -> *

data Void  deriving(Generic)
absurd :: Void -> a
absurd v = case v of

data Dict :: Constraint -> * where
  Dict :: a => Dict a

instance Show (Dict a) where
  show = const "dict"



-- type instance DiffT ((:*:) f g) = (DiffT f) :*: (DiffT g)


-- D(xy) = xDy + yDx
-- Borrow diffgeo lingo?
-- type Derivation a b da db = Either (a, db)  (b, da)

class Diff a  where
  type family Patch a :: *
  type Patch a = GPatch (Rep a) a

  diff :: a -> a -> Patch a
  default diff :: (Generic a, GDiff (Rep a), Patch a ~ (GPatch (Rep a)) a) => a -> a -> Patch a
  diff a a' = gdiff (from a) (from a')

-- type family DT (a :: * -> *) :: * -> *
-- type instance DT V1 = V1
-- type instance DT U1 = U1
-- type instance DT (f :*: g) = (DT f :*: DT g)
-- type instance DT (f :+: g) = DT f :+: DT g :+: f :+: g
-- type instance DT (M1 i t f) = (M1 i t (DT f))

class GDiff (gen :: * -> *)  where
  type family GPatch gen :: * -> *
  gdiff :: gen a -> gen a -> (GPatch gen) a

instance GDiff V1 where
  type GPatch V1 = V1 
  gdiff v1 _ = undefined

instance GDiff U1 where
  type GPatch U1 = U1
  gdiff u1 u1' = u1

-- products
instance (GDiff f, GDiff g) => GDiff (f :*: g) where
  type GPatch (f :*: g) = (GPatch f :*: GPatch g) 
  gdiff (f :*: g) (f' :*: g') = (gdiff f f') :*: (gdiff g g')


-- sums
-- instance (GDiff f fp, GDiff g gp) => GDiff (f :+: g) ((fp :+: gp) :+: (f :+: g)) where
-- instance (GDiff f fp, GDiff g gp) => GDiff (f :+: g) (fp :+: fp :+: gp) where

instance (GDiff f, GDiff g) => GDiff (f :+: g)  where
  type GPatch (f :+: g) = (f :+: g :+: GPatch f :+: GPatch g)
  gdiff (L1 f) (L1 f') = R1 (R1 (L1 (gdiff f f')))
  gdiff (R1 g) (R1 g') = R1 (R1 (R1 (gdiff g g')))
  gdiff (L1 f) (R1 g) = (L1 f)
  gdiff (R1 g) (L1 f) = R1 (L1 g)

-- meta info, we simply tunnel through
instance (GDiff f) => GDiff (M1 i t f)  where
  type GPatch (M1 i t f) =  M1 i t (GPatch f)
  gdiff (M1 x) (M1 x') = M1 $ gdiff x x'


-- instance {-# OVERLAPS #-} (GDiff f fp) => GDiff (M1 i t f) fp where
--   gdiff (M1 x) (M1 x') =  gdiff x x'
-- 
-- 
-- instance {-# OVERLAPS #-} (GDiff f fp) => GDiff f (M1 i t fp) where
--   gdiff x x' = M1 $ gdiff x x'

-- recursion
instance (Diff f) => GDiff (K1 i f) where
   type GPatch (K1 i f) = (K1 i (Patch f))
   gdiff (K1 x) (K1 x') = K1 $ diff x x'

instance Diff () 
instance Diff Void 
instance (Diff a, Diff b) => Diff (a, b) 
-- instance (Diff a pa, Diff b pb) => Diff (Either a b) (Either (Either pa pb) (Either a b))

-- type instance DT Int = (NumDelta Int)
data NumDelta a = NumDelta a deriving(Show)
--  instance Diff Int (NumDelta Int) where
--    diff a b =  NumDelta $ b - a


instance Diff Int  where
  type Patch Int = (NumDelta Int)
  diff a b =  NumDelta $ b - a

  
data Ctor a = Ctor a deriving(Generic, Show)
data Ctor2 a = Ctor2 a deriving(Generic, Show)

instance (Diff a) => Diff (Ctor a) 

data And = And () () deriving(Generic, Show)
data And2 = And2 () () deriving(Generic, Show)
instance Diff And

data Or = One | Two deriving (Generic, Show)
data Or2 = One2 | Two2 | Three2 deriving(Generic, Show)
data OrDiff = OneOne | OneTwo | TwoOne | TwoTwo deriving(Generic, Show)
instance Diff Or 

data Stream a = a :< (Stream a) deriving(Generic, Show)

-- TODO: allow this derivation to work automatically
-- instead of getting stuck
-- instance Diff (Stream a)
instance Diff a => Diff (Stream a) where
  type Patch (Stream a) = Stream (Patch a)
  diff (x:<xs) (x':<xs') = (diff x x'):<(diff xs xs')


instance Diff a => Diff [a] where
  type Patch [a] = [Patch a]
  diff (x:xs) (x':xs') = (diff x x'):(diff xs xs')


-- instance Diff [a] 

-- instance (Diff a) => Diff (Stream a) 


x :: Int
x = 3

y :: Int
y = 4

dxy :: NumDelta Int
dxy = diff x y


-- dxxyy :: (NumDelta Int, NumDelta Int)
dxxyy = diff (x, x) (y, y)

data Four a b c d = LL a | LR b | RL c | RR d

l :: Either Int Int
l = Left 4

r :: Either Int Int
r = Right 10

-- dlr :: Four Int Int (NumDelta Int) (NumDelta Int)
-- dlr = diff l r


-- type instance DiffT V1  = V1
-- type instance DiffT U1 = U1
-- type instance DiffT (f :*: g) = (DiffT f) :*: (DiffT g)
-- type instance DiffT (f :+: g) = (DiffT f) :+: (DiffT g) :+: f :+: g
-- type instance DiffT 

main :: IO ()
main = print "foo"
