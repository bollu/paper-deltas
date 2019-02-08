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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
module WorkingGenerics where
import GHC.Generics
import GHC.Exts (Constraint)
import Control.Monad.State.Lazy
import Control.Monad.Identity

-- type family DiffT (p :: * -> *) :: * -> *

data Void  deriving(Generic)
absurd :: Void -> a
absurd v = case v of

data Dict :: Constraint -> * where
  Dict :: a => Dict a

instance Show (Dict a) where
  show = const "dict"

class Diff a  where
  type family Patch a :: *
  type Patch a = GPatch (Rep a) a

  diff :: a -> a -> Patch a
  default diff :: (Generic a, GDiff (Rep a), Patch a ~ (GPatch (Rep a)) a) => a -> a -> Patch a
  diff a a' = gdiff (from a) (from a')

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


-- recursion
instance (Diff f) => GDiff (K1 i f) where
   type GPatch (K1 i f) = (K1 i (Patch f))
   gdiff (K1 x) (K1 x') = K1 $ diff x x'

instance Diff () 
instance Diff Void 
instance (Diff a, Diff b) => Diff (a, b) 
instance (Diff a, Diff b) => Diff (Either a b)

-- type instance DT Int = (NumDelta Int)
data NumDelta a = NumDelta a deriving(Show)


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

dlr = diff l r


-- Step 2: We want nice ways to render the deltas.
showPatch :: Diff a => Patch a -> String
showPatch = undefined

-- -- Step 3:
-- Now that we have the diffing mechanism, we now want good incremental
-- patching. We will probably invoke free monnad/cofree comonad
-- to derive incremental interpreters from non incremental ones.

data Free f a = Leaf a | Branch (f (Free f a)) deriving(Functor)

-- f := free, g := functor 
bindFree :: Functor g => Free g a -> (a -> Free g b) -> Free g b
bindFree (Leaf a) a2fgb = (a2fgb a) 
bindFree (Branch fga) a2fgb = 
  Branch $ (`bindFree` a2fgb) <$> fga

-- Does this have a better instance?
apFree :: Functor g => Free g (a -> b) -> Free g a -> Free g b
apFree frga2b frga = 
  bindFree frga2b  (\a2b ->
    bindFree frga  (\a -> Leaf $ a2b a))

instance Functor f => Applicative (Free f) where
  pure = Leaf
  (<*>) = apFree

instance Functor f => Monad (Free f) where
 return = pure
 (>>=) = bindFree

-- cofree comonad
data Cofree g a = Cobranch a (g (Cofree g a)) deriving(Functor)

class Comonad w where
  extract :: w a -> a
  (=>>) :: w a -> (w a -> b) -> w b


instance Functor g => Comonad (Cofree g) where
  extract (Cobranch a _) = a
  (cur@(Cobranch a g_cofree_a)) =>> cofree_a_to_b = 
    let cur' = cofree_a_to_b cur 
        next = (fmap (=>> cofree_a_to_b) g_cofree_a)
    in Cobranch cur' next


coiter :: Functor f => (a -> f a) -> a -> Cofree f a
coiter psi a = Cobranch a (coiter psi <$> psi a)
 

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (Cobranch a  _ ) (Leaf x)  = p a x
  pair p (Cobranch _ fs) (Branch gs) = pair (pair p) fs gs

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = pair (flip p) g f


-- Example language
data DSLF k = Add Int (Bool -> k)
  | Clear k
  | Total (Int -> k) deriving(Functor)

type DSL a = Free DSLF a


liftF :: (Functor f) => f a -> Free f a
liftF fa = Branch $ Leaf <$> fa

add :: Int -> DSL Bool
add x = liftF $ Add x  id

clear :: DSL ()
clear = liftF $ Clear ()

total :: DSL Int
total = liftF $ Total id

data CoDSLF k = CoDSLF {
  addH :: Int -> (Bool, k),
  clearH :: k,
  totalH :: (Int, k)
} deriving(Functor)

type Limit = Int
type Count = Int
type CoDSL = Cofree CoDSLF
mkCoDSL :: Limit -> Count -> CoDSL (Limit, Count)
mkCoDSL limit count = coiter next start
  where
    next w = CoDSLF (coAdd w) (coClear w) (coTotal w)
    start = (limit, count)

coClear :: (Limit, Count) -> (Limit, Count)
coClear (limit, count) = (limit, 0)

coTotal :: (Limit, Count) -> (Int, (Limit, Count))
coTotal (limit, count) = (count, (limit, count))

coAdd :: (Limit, Count) -> Int -> (Bool, (Limit, Count))
coAdd (limit, count) x = (test, (limit, next))
  where
    count' = count + x                        -- 1
    test = count' <= limit                    -- 2
    next = if test then count' else count     -- 3


instance Pairing CoDSLF DSLF where
  pair f (CoDSLF a _ _) (Add x k) = pair f (a x) k
  pair f (CoDSLF _ c _) (Clear k) = f c k
  pair f (CoDSLF _ _ t) (Total k) = pair f t k


double :: DSL Int
-- or if we want to be more general:
--   double :: Monad m => AdderT m Int
double = do
  -- capture the old count
  t <- total
  _ <- add t
  total


-- run with a given interpreter
runLimit :: CoDSL a -> Int
runLimit w = pair (\_ b -> b)  w double

-- test with a given limit and start count
testLimit :: Limit -> Count -> Int
testLimit l c = runLimit  (mkCoDSL l c)



-- TODO: incrementalize a given interpreter.
incremental :: (Diff a, b ~ Patch a) => Cofree f a -> Cofree f b
incremental = undefined




main :: IO ()
main = print "foo"
