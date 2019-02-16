{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE EmptyCase  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE Rank2Types  #-}
{-# LANGUAGE ApplicativeDo  #-}
module PatchDSL where
import Prelude hiding ((.))
import Control.Applicative
import Data.Monoid
-- import Generics.Eot
import Control.Category
import Control.Arrow
import GHC.Generics
import GHC.Exts (Constraint)
import Data.String.Utils
import qualified Language.Haskell.TH as TH
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Util as P
import qualified Data.Text.Prettyprint.Doc.Render.String as P
import qualified WorkingGenerics as WG
import qualified Data.FingerTree as FT
import qualified Data.LCA.Online as LCA
import qualified Data.LCA.View as LCA
-- Test harness
import Test.Tasty
import qualified Test.QuickCheck as QC
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Test.Tasty.HUnit
import Test.QuickCheck.Function
-- Profunctor
import Data.Profunctor
-- Profunctor
import Data.Profunctor

type family Patch (s :: * -> *) a = r 

class Diff a p where
  diff :: a -> a -> p
  patch :: p -> a -> a

(<->) = diff
(+$) = patch

newtype PatchFun s a b = PatchFun { runPatchFun :: a -> (b, Patch s a -> s b) }
newtype PatchFun' s a b = PatchFun' { runPatchFun' :: a -> s (b, Patch s a -> Patch s b) }

($$) :: PatchFun s a b -> a -> (b, Patch s a -> s b)
($$) = runPatchFun 


($:$) = runPatchFun'

-- | Lift a raw arrow into the domain of patches
patcharr :: (Diff a (Patch s a), Applicative s) => (a -> b) -> PatchFun s a b
patcharr f = PatchFun $ \a -> (f a, \pa ->  pure $ f (pa +$ a))

-- lifting into arrow
patcharr' :: (Diff b (Patch s b), Diff a (Patch s a), Applicative s) => (a -> b) -> PatchFun' s a b
patcharr' f = PatchFun' $ \a -> pure $ (f a , \pa -> f (pa +$ a) <-> f a )

patchcompose' :: (Monad s, Diff a (Patch s a), Diff b (Patch s b)) => 
  PatchFun' s a b
  -> PatchFun' s b c 
  -> PatchFun' s a c
patchcompose' fab fbc = PatchFun' $ \a -> do
      (b, pab) <- fab $:$ a
      (c, pbc) <- fbc $:$ b
      pure (c, pbc . pab)

-- composition
($.$) = patchcompose'



-- | compose two patches
-- | loses the ability to compose patches :( 
-- | that is, do not use: (Patch s a -> s b) -> (Patch s b -> sc) -> Patch s a -> s c
patchcompose :: (Applicative s, Diff a (Patch s a)) => PatchFun s a b 
    -> PatchFun s b c 
    -> PatchFun s a c
patchcompose fab fbc = 
  let fac =  fst . (runPatchFun fbc) . fst . (runPatchFun fab) in
    PatchFun $ \a -> (fac a, \pa -> pure $ fac (pa +$ a))



instance Category (PatchFun s) where



