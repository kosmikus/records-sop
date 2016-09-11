{-# LANGUAGE TypeInType, TypeFamilies, GADTs, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes, ConstraintKinds #-}
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

data MyRec1 = MyRec1 { rint :: Int, rbool :: Bool }
  deriving (GHC.Generic, Show)

data MyRec2 = MyRec2 { rbool :: Bool, rchar :: Char, rint :: Int }
  deriving (GHC.Generic, Show)

instance Generic MyRec1
instance HasDatatypeInfo MyRec1

instance Generic MyRec2
instance HasDatatypeInfo MyRec2

cast :: (IsRecord a siga, IsRecord b sigb, IsSubTypeOf siga sigb) => a -> b
cast = fromRecord . castRecord . toRecord

class IsSubTypeOf (r1 :: RecordCode) (r2 :: RecordCode) where
  castRecord :: Record r1 -> Record r2

instance IsSubTypeOf r1 '[] where
  castRecord _ = Nil

instance (IsSubTypeOf r1 rs2, Contains r1 s2 a2) => IsSubTypeOf r1 ( '(s2, a2) : rs2 ) where
  castRecord r = P (get (Proxy :: Proxy s2) r) :* castRecord r

class Contains (r :: RecordCode) (s :: Symbol) (a :: Type) where
  get :: Proxy s -> Record r -> a

class ContainsHelper (b :: Bool) (r :: RecordCode) (s :: Symbol) (a :: Type) where
  get' :: Proxy b -> Proxy s -> Record r -> a

instance
  ContainsHelper (s1 == s2) ( '(s1, a1) : rs ) s2 a2 =>
  Contains ( '(s1, a1) : rs ) s2 a2 where
  get = get' (Proxy :: Proxy (s1 == s2))

instance (a1 ~ a2) => ContainsHelper True ( '(s, a1) : rs ) s a2 where
  get' _ _ (P a :* _) = a

instance Contains rs s2 a2 => ContainsHelper False ( '(s1, a1) : rs ) s2 a2 where
  get' _ p (_ :* r) = get p r

