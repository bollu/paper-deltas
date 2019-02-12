{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
module TemplateGenerics where
import Control.Monad
import Language.Haskell.TH


class Diff a where
  data family Patch a
  diff :: a -> a -> Patch a
  apply :: a -> Patch a -> Maybe a
  trivial :: Patch a

-- x ::Dec
-- x = InstanceD None
