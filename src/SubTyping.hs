{-# LANGUAGE TypeInType, TypeFamilies, GADTs, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes, ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes, TypeApplications #-}
module SubTyping where

import Data.Type.Equality (type (==))
import Data.Proxy
-- import Generics.SOP
import Generics.SOP.BasicFunctors
import Generics.SOP.Constraint
import Generics.SOP.GGP
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Universe
import Generics.SOP.Sing
import Generics.SOP.Type.Metadata
import qualified GHC.Generics as GHC
import GHC.Types
import Unsafe.Coerce

import Record

{-
data MyRec1 = MyRec1 { rint :: Int, rbool :: Bool }
  deriving (GHC.Generic, Show)

data MyRec2 = MyRec2 { rbool :: Bool, rchar :: Char, rint :: Int }
  deriving (GHC.Generic, Show)

instance Generic MyRec1
instance HasDatatypeInfo MyRec1

instance Generic MyRec2
instance HasDatatypeInfo MyRec2
-}

-- | Cast one record type to another if there is a subtype relationship
-- between them. Currently, only width subtyping is considered, which means
-- that we can forget and reorder fields.
--
cast :: (IsRecord a siga, IsRecord b sigb, IsSubTypeOf siga sigb) => a -> b
cast = fromRecord . castRecord . toRecord

class IsSubTypeOf (r1 :: RecordCode) (r2 :: RecordCode) where
  castRecord :: Record r1 -> Record r2

instance IsSubTypeOf r1 '[] where
  castRecord _ = Nil

instance (IsSubTypeOf r1 rs2, IsElemOf s2 a2 r1) => IsSubTypeOf r1 ( '(s2, a2) : rs2 ) where
  castRecord r = P (get @s2 r) :* castRecord r

class IsElemOf (s :: Symbol) (a :: Type) (r :: RecordCode) where
  get :: Record r -> a

class IsElemOf' (b :: Bool) (s :: Symbol) (a :: Type) (r :: RecordCode) where
  get' :: Record r -> a

instance
  IsElemOf' (s1 == s2) s2 a2 ( '(s1, a1) : rs ) =>
  IsElemOf s2 a2 ( '(s1, a1) : rs ) where
  get = get' @(s1 == s2) @s2

-- | Helper class. Isn't strictly needed, but allows us to avoid
-- overlapping instances.
--
instance (a1 ~ a2) => IsElemOf' True s a2 ( '(s, a1) : rs ) where
  get' (P a :* _) = a

instance IsElemOf s2 a2 rs => IsElemOf' False s2 a2 ( '(s1, a1) : rs ) where
  get' (_ :* r) = get @s2 r

