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

-- Idea for an actually convenient interface:
--
-- Have three levels of representation.
--
-- The "full" level contains all info.
--
-- The "medium" level contains only the names of constructors and fields.
--
-- SOP P2 xss = NS (NP P2 `Compose` P2) xss
--
-- Code:
--
-- MCode MyRec1 =
--   '[ '( "MyRec1", '[ '( "rint", Int ), '( "rbool", Bool ) ] ]

-- from :: Generic a => a -> Rep a

data MyRec1 = MyRec1 { rint :: Int, rbool :: Bool }
  deriving (GHC.Generic, Show)

data MyRec2 = MyRec2 { rbool :: Bool, rchar :: Char, rint :: Int }
  deriving (GHC.Generic, Show)

instance Generic MyRec1
instance HasDatatypeInfo MyRec1

instance Generic MyRec2
instance HasDatatypeInfo MyRec2

cast :: (IsRecord a siga, IsRecord b sigb, IsSubTypeOf siga sigb) => a -> b
cast = fromRecord . coerce . toRecord

{-
-- We claim that a Record is essentially the same as a Rep.
-- Can we generalize this?

coerce_NP :: forall k1 k2 f g xs ys . (Rel f g xs ys) => NP f xs -> NP g ys
coerce_NP Nil       = Nil
coerce_NP (x :* xs) = x :* coerce_NP xs

type family Rel (f :: k1 -> Type) (g :: k2 -> Type) (xs :: [k1]) (ys :: [k2]) :: Constraint where
  Rel f g '[]       ys = (ys ~ '[])
  Rel f g (x ': xs) ys = (ys ~ (Head ys : Tail ys), f x ~ g (Head ys), Rel f g xs (Tail ys))

class Related (f :: k1 -> Type) (g :: k2 -> Type) (xs :: [k1]) (ys :: [k2])

instance (ys ~ '[]) => Related f g '[] ys
instance (f x ~ g y, zs ~ (y ': ys), Related f g xs ys) => Related f g (x ': xs) zs 
-}

class IsSubTypeOf (r1 :: RecordCode) (r2 :: RecordCode) where
  coerce :: Record r1 -> Record r2

instance IsSubTypeOf r1 '[] where
  coerce _ = Nil

instance (IsSubTypeOf r1 rs2, Contains r1 s2 a2) => IsSubTypeOf r1 ( '(s2, a2) : rs2 ) where
  coerce r = P (get (Proxy :: Proxy s2) r) :* coerce r

-- | TODO. Can we reuse GHC.OverloadedLabels.IsLabel or similar?
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

{-
instance {-# OVERLAPPING #-} (a1 ~ a2) => Contains ( '(s, a1) : rs ) s a2 where
  get _ (P2 a :* _) = a

instance {-# OVERLAPPABLE #-} Contains rs s2 a2 => Contains ( '(s1, a1) : rs ) s2 a2 where
  get p (_ :* r) = get p r
-}

test1 :: Record '[ '( "name", String ), '( "age", Int ) ]
test1 = P "Andres" :* P 99 :* Nil

data DFoo x where MkDFoo :: x ~ (y : ys) => DFoo x

{-
instance
  IfThenElse (IsEqual s1 s2) (a1 ~ a2) (Contains rs s2 a2) =>
  Contains ( '(s1, a1) : rs ) s2 a2 where
  get p (P2 a :* r) =
    ifthenelse (Proxy :: Proxy '(IsEqual s1 s2, a1 ~ a2, Contains rs s2 a2))
      a
      (get p r)

class IfThenElse (b :: Bool) (t :: Constraint) (e :: Constraint) where
  ifthenelse :: Proxy '(b, t, e) -> (t => r) -> (e => r) -> r

instance t => IfThenElse True t e where
  ifthenelse _ t _ = t

instance e => IfThenElse False t e where
  ifthenelse _ _ e = e
-}

