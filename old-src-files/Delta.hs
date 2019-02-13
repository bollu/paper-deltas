{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
-- What do these do precisely?
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Delta where
import Data.Semigroup
import Data.Void
import Data.Typeable
import Generics.Eot
import Data.Constraint
import Data.Group
-- DeltaEOT a


class PatchEOT a where
  -- type DeltaEOT a 
  data DeltaEOT a
  eotTakeDelta :: a -> a -> DeltaEOT a
  eotApplyDelta :: DeltaEOT a -> a -> a

(\$) :: PatchEOT a => DeltaEOT a -> a -> a
(\$) = eotApplyDelta

(\-) :: PatchEOT a => a -> a -> DeltaEOT a 
(\-) = eotTakeDelta

instance PatchEOT Void where
  data DeltaEOT Void = VoidDelta Void
  eotTakeDelta a _ = absurd a
  eotApplyDelta (VoidDelta v) _ = absurd v

instance PatchEOT () where
  data DeltaEOT () = UnitDelta
  eotTakeDelta _ _= UnitDelta
  eotApplyDelta _ _ = ()


instance (PatchEOT a, PatchEOT b) => PatchEOT (a, b) where
  data DeltaEOT (a, b) = TupleDelta (DeltaEOT a, DeltaEOT b)

  eotTakeDelta (a1, b1) (a2, b2) = 
    TupleDelta (eotTakeDelta a1 a2, eotTakeDelta b1 b2)
  eotApplyDelta (TupleDelta (da, db)) (a, b) = 
    (eotApplyDelta da a, eotApplyDelta db b)


instance (PatchEOT a, PatchEOT b) => PatchEOT (Either a b) where
-- This is interesting, I don't know this algebraic structure.
-- I need to understand what algebra this is!
  data DeltaEOT (Either a b) = 
    EitherDeltaRL a | 
    EitherDeltaLR b | 
    EitherDeltaLL (DeltaEOT a) | 
    EitherDeltaRR (DeltaEOT b)

  eotTakeDelta (Left a) (Left b) = EitherDeltaLL (eotTakeDelta a b)
  eotTakeDelta (Right a) (Right b) = EitherDeltaRR (eotTakeDelta a b)
  eotTakeDelta (Left a) (Right b) = EitherDeltaLR b
  eotTakeDelta (Right b) (Left a) = EitherDeltaRL a

  eotApplyDelta (EitherDeltaLL dl) (Left l) = Left (eotApplyDelta dl l)
  eotApplyDelta (EitherDeltaRL a) _ = Left a
  eotApplyDelta (EitherDeltaRR dr) (Right r) = Right (eotApplyDelta dr r)
  eotApplyDelta (EitherDeltaLR a) _ = Right a


-- 1. #+> is a group action of (Delta a) on a 
-- 2. (a #-# b) #+> b = a
-- 3. (a #-# b) `gop` d  = (d #+> a) #-# b
class Patch a where
  data (Delta a)
  -- type (Delta a) = r | r -> a
  -- use Dict to witness the fact that Delta is a group

  takeDelta :: a -> a -> Delta a
  applyDelta ::  Delta a -> a -> a

  (#+>) :: Patch a => Delta a -> a -> a
  (#+>) = applyDelta

  (#-#) :: Patch a => a -> a -> Delta a
  (#-#) = takeDelta




instance Patch Bool where
  data (Delta Bool) = DBTrue | DBFalse | DBUnit
  takeDelta a b = boolToDeltaBool a
  applyDelta da a = case da of
                        DBUnit -> a
                        DBTrue -> True
                        DBFalse -> False


instance Semigroup (Delta Bool) where
  x <> DBUnit = x
  DBUnit <> x = x
  x <> y = y

boolToDeltaBool :: Bool -> (Delta Bool)
boolToDeltaBool True = DBTrue
boolToDeltaBool False = DBFalse

instance Monoid (Delta Bool)  where
  mempty = DBUnit
  mappend = (<>)

instance Group (Delta Bool) where
  invert DBUnit = DBUnit
  invert DBTrue = DBFalse
  invert DBFalse = DBTrue

-- Pointwise lifts
-- instance  (Patch b) => Patch (a -> b)  where
--   type Delta (a -> b) = Delta b
-- 
--   takeDelta :: (a -> b) -> (a -> b) -> (a -> Delta b)
--   takeDelta fa fa' = \a -> takeDelta (fa a) (fa' a)
-- 
--   applyDelta :: (a -> d) -> (a -> b) -> (a -> Delta b)
--   applyDelta fda fa = \a -> applyDelta (fda a) (fa a)
-- 

-- Int delta is just int
instance Patch Int where
    data Delta Int = DeltaInt (Sum Int)

    takeDelta :: Int -> Int -> Delta Int
    takeDelta a a' = DeltaInt $ Sum $ a - a'

    applyDelta :: Delta Int -> Int -> Int
    applyDelta (DeltaInt d) a = getSum d + a

{-
newtype GeneralizedCont (o :: * -> *) (c :: * -> Constraint) (a :: *)=
  GeneralizedCont { 
    runGeneralizedCont :: forall r. c r => (a -> r) -> o r 
  }

-- f $$ x is the analogous f $ x
($$) :: c r => (a -> r) -> GeneralizedCont o c a -> o r
($$) auser ka = runGeneralizedCont ka auser


reify :: (c' :- c) -> (c => a) -> (c' => a)
reify cons f = case cons of Sub d -> case d of Dict -> f


reify2 :: (forall r1. c' r1 :- c r1) -> (forall r2. c r2 => a) -> (forall r3. c' r3 => a)
reify2 cons f = reify cons f

($\\) :: (forall r. c r :- c' r) -> GeneralizedCont o c a -> GeneralizedCont o c' a
($\\) weakener ka = 
    GeneralizedCont $ \auser -> auser $ reify weakener (runGeneralizedCont ka)  

-- weaken constraint, and allow fmap
class ConstraintFunctor (f :: (* -> Constraint) -> * -> *) where
  cfmap :: (d' b :- d a) -> (a -> b) -> f d a -> f d' b

class ConstraintMonad (f :: (* -> Constraint) -> * -> *) where
  cpure :: a -> f d a
  cbind ::  (d a :- d' a) -> f d a -> (a -> f d' b) -> f d' b

instance Functor o => ConstraintFunctor (GeneralizedCont o) where
  cfmap :: (d a :- d' b) -> (a -> b) -> GeneralizedCont o d a -> GeneralizedCont o d' b
  cfmap entails f c = 
    GeneralizedCont $ \buser -> ((\a -> buser (f a)) $$ c)
-}

-- === CONTINUATION BASED, MOVE TO CONT.hs ===
-- All of these are subtypes of GeneralizedCont
newtype PatchCont a  =
  PatchCont { runPatchCont :: forall r. Patch r =>  (a -> r) -> r }



newtype EitherPatchCont  a b =
  EitherPatchCont { 
    runEitherPatchCont :: forall r. Patch r => (a -> r, b -> r) -> r
  }

  
instance Patch (PatchCont a)  where
 data Delta (PatchCont a) = DeltaCont {     
   runDeltaCont :: forall r. Patch r => (a -> r) -> Delta r 
 }
 takeDelta :: PatchCont a -> PatchCont a -> Delta (PatchCont a)
 takeDelta ka ka' =
   DeltaCont $ \auser ->
                 takeDelta (runPatchCont ka auser) (runPatchCont ka' auser) 

 applyDelta :: Delta (PatchCont a) -> PatchCont a -> PatchCont a
 applyDelta kda ka =
  PatchCont $ \auser ->
                 applyDelta (runDeltaCont kda auser) (runPatchCont ka auser)


instance Patch (EitherPatchCont a b) where
  data Delta (EitherPatchCont a b) = 
    EitherDeltaCont { 
      runEitherDeltaCont :: forall r. Patch r => (a -> r, b -> r) -> Delta r 
    }

  
  takeDelta :: EitherPatchCont a b -> EitherPatchCont a b -> Delta (EitherPatchCont a b)
  takeDelta ka ka' = EitherDeltaCont $ \(auser, buser) ->
    takeDelta (runEitherPatchCont ka (auser, buser)) (runEitherPatchCont ka' (auser, buser))

  applyDelta :: Delta (EitherPatchCont a b) -> EitherPatchCont a b -> EitherPatchCont a b
  applyDelta kda ka = EitherPatchCont $ \(auser, buser) ->
    let r = (runEitherPatchCont ka (auser, buser))
        dr = (runEitherDeltaCont kda (auser, buser))
    in applyDelta dr r

tupleToPatchCont :: (Patch a, Patch b) => (a, b) -> PatchCont (a, b) 
tupleToPatchCont ab = PatchCont $ \abuser -> abuser ab

eitherToPatchCont :: (Patch a, Patch b) => Either a b -> EitherPatchCont a b
eitherToPatchCont (Left a) = EitherPatchCont $ \(auser, _) -> auser a
eitherToPatchCont (Right b) = EitherPatchCont $ \(_, buser) -> buser b


{-
IRC log of discussion with carter
but, the point is, I don't recognize the algebraic structure that is coming out of `Delta(a :+: b)`
23:41 <bollu>  I mean, what operation _is_ `Delta`?
23:41 <bollu>  it's something I cooked up and makes sense
23:41 <bollu>  but in the vast realm of algebra / category theory, I've never seen anything like it, which is disturbing to me
23:41 <carter>  hrmm
23:41 <carter>  lets pause for a moment
23:41 <bollu>  so either there's some new structure here, or I'm not squinting at it right
23:41 <bollu>  sure
23:41 <carter>  and ask: is there a duality happening
23:41 <bollu>  between products and sums?
23:41 <carter>  well sure,
23:42 <carter>  also, i think the math here would be easier to understand if you augment wiht the invertible structure
23:42 <carter>  then you just weaken / generalize by making it uni direction
23:42 <bollu>  OK, sure, let's ask it to have a group structure, I don't mind
23:42 <carter>  at least for discussion purposes
23:42 <carter>  soo,
23:43 <carter>  whts the dual of sum?
23:43 <carter>  not not (a + b)
23:43 <carter>  not (not a  choose not b) ?
23:44 <carter>  so  roughly
23:44 <bollu>  why would the dual involve a not at all? :) 
23:44 <carter>  i'm using what i know
23:44 <carter>  a + b ===  (not a) & (not b)
23:44 <bollu>  OK
23:44 <bollu>  ah, but that's in a boolean algebra, but let's go on
23:44 <carter>  linear logic duality
23:44 <bollu>  not (a + b) === not a & not b
23:44 <carter>  sir
23:44 <carter>  :)
23:44 <bollu>  :) 
23:44 <bollu>  sure
23:44 <bollu>  go on
23:45 <carter>  a + b = not (not a & not b) is i guess the constructive one
23:45 <carter>  ish
23:45 <bollu>  sure
23:46 <carter>  a + b =  forall c .   (a & b -> c) - > c
23:46 <carter>  aka CPS
23:46 <bollu>  mmhm
23:46 <carter>  we in fp land / classical lands usually write a + b =   forall c .((a,b)-> c )-> c
23:47 <carter>  though that doesn't quite work here
23:47 <carter>  or at least its weird here
23:47 <carter>  bollu:  can you define a delta on the CPS's form?
23:48 <carter>  err
23:48 <carter>  hrmm
23:48 <bollu>  carter I'm not convinced that's the quite correct type, actually a + b == forall c. (a -> c & b -> c) -> c
23:49 <carter>  yeah
23:49 <carter>  i think your right
23:49 <carter>  it'd be (a -> c , b -> c) -> c
23:49 <bollu>  yeah, because as an Either, I need to have the choice of feeding you an "a" or a "b"
23:49 <bollu>  indeed
23:49 <carter>  you got me there
23:49 <carter>  ok, NOW can you :)
23:50 <carter>  this seems to possibly break your function instance?
23:50 <carter>  or do you have a seperate instance for quantified stuff?
23:50 <bollu>  my function instance is rank 1, not rank 2
23:50 <carter>  ok
23:50 <bollu>  yeah, I haven't thought about rank 2
23:50 <carter>  does that deserve a newtype for the rank 2?
23:50 <bollu>  essentially, you're asking me to differentiate Cont r a
23:51 <carter>  perhaps
23:51 <carter>  or saying: these two are related :)
23:51 <carter>  at least under some CBV == CBN assumption

-}
