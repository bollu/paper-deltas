{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module TemplateGenerics where
import Control.Monad
import Language.Haskell.TH


class Diff a where
  type family Patch a :: *
  diff :: a -> a -> Patch a

data PolyCons a = PolyCons a 
data List a = Nil | Cons a (List a)
data Stream a = SCons a (Stream a)
