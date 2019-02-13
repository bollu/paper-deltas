{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
module Incremental where
import Delta
import Data.Profunctor
import Control.Arrow
import qualified Control.Category as C

fullToIncremental :: (a -> b) -> (Delta a -> Delta b)
fullToIncremental = undefined


newtype CheckpointToken a = CheckpointToken Int
unsafeTransmuteCheckpointToken :: CheckpointToken a -> CheckpointToken b
unsafeTransmuteCheckpointToken (CheckpointToken i) = CheckpointToken i


-- finger tree? should be interesting!
data SpeculateA a b where
    -- Is it okay to lie about the source? I'm not sure. 
    -- TODO: think about this
    LiftA :: a -> SpeculateA k a
    -- LiftA :: a -> SpeculateA () a
    Arr :: (a -> b) -> SpeculateA a b
    Checkpoint :: SpeculateA a (CheckpointToken a, a)
    Unroll :: CheckpointToken a -> SpeculateA b a
    Seq :: SpeculateA a b -> SpeculateA b c -> SpeculateA a c
    Par :: SpeculateA i o -> SpeculateA i' o' -> SpeculateA (i, i') (o, o')
    Choice :: SpeculateA i o -> SpeculateA i' o' -> SpeculateA (Either i i') (Either o o')

instance C.Category SpeculateA where
    (.) = flip Seq
    id = Arr id
instance Arrow SpeculateA where
    arr = Arr
    (***) = Par

instance ArrowChoice SpeculateA where
    (+++) = Choice

-- Profunctor definition for the arrow based on 
-- Is this always possible? I guess so. What does this buy me? nothing, AFAICT
instance Profunctor SpeculateA where
    dimap i o spec = i ^>> spec >>^ o

-- Entry point to the runner of the speculation engine.
compileSpeculateA :: SpeculateA a b -> a -> b
compileSpeculateA (Arr f) a = f a
compileSpeculateA (LiftA b) _ = b
compileSpeculateA (Seq f g) a = 
    let x = (compileSpeculateA f a) in
        compileSpeculateA g x
compileSpeculateA (Par f g) tup = 
    let l = compileSpeculateA f (fst tup) 
        r = compileSpeculateA g (snd tup) 
        in (l, r)
compileSpeculateA (Choice f g) eab = 
    case eab of
        Left a -> Left (compileSpeculateA f a)
        Right b -> Right (compileSpeculateA g b)

-- Now that we have an Arrow and Profunctor instances, now what? I assume
-- we get started and build pipelines of computations that can be unrolled?

data IR 
instcombine :: SpeculateA IR IR
instcombine = undefined

loopinline :: SpeculateA IR IR
loopinline = undefined

vectorize :: SpeculateA IR IR
vectorize = undefined

goodVectorize :: IR -> Bool
goodVectorize = undefined

optimize :: SpeculateA IR  IR
optimize = 
    proc x -> do
    y <- instcombine <<< instcombine -< x
    (chkpt_loop_inline, li) <- Checkpoint <<< loopinline -< y
    (chkpt_vectorize, vec) <- Checkpoint <<< vectorize -< li
    if goodVectorize vec 
    then 
        returnA <<< vectorize -< vec
    else 
        returnA <<< loopinline -< li