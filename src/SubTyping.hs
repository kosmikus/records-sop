{-# LANGUAGE TypeInType, TypeFamilies, GADTs, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module SubTyping where

import Data.Proxy
import GHC.Types

type FieldLabel = Symbol
type RecordSig = [(FieldLabel, Type)]

data Record (r :: RecordSig) where
  RNil :: Record '[]
  (:-) :: a -> Record rs -> Record ( '(s, a) : rs )

type family AllShow (r :: RecordSig) :: Constraint where
  AllShow '[] = ()
  AllShow ( '(s, a) : rs ) = (Show a, AllShow rs)

deriving instance AllShow r => Show (Record r)

infixr 5 :-

class IsSubTypeOf (r1 :: RecordSig) (r2 :: RecordSig) where
  coerce :: Record r1 -> Record r2

instance IsSubTypeOf r1 '[] where
  coerce _ = RNil

instance (IsSubTypeOf r1 rs2, Contains r1 s2 a2) => IsSubTypeOf r1 ( '(s2, a2) : rs2 ) where
  coerce r = get (Proxy :: Proxy s2) r :- coerce r

-- | TODO. Can we reuse GHC.OverloadedLabels.IsLabel or similar?
class Contains (r :: RecordSig) (s :: Symbol) (a :: Type) where
  get :: Proxy s -> Record r -> a

instance {-# OVERLAPPING #-} (a1 ~ a2) => Contains ( '(s, a1) : rs ) s a2 where
  get _ (a :- _) = a

instance {-# OVERLAPPABLE #-} Contains rs s2 a2 => Contains ( '(s1, a1) : rs ) s2 a2 where
  get p (_ :- r) = get p r

test1 :: Record '[ '( "name", String ), '( "age", Int ) ]
test1 = "Andres" :- 99 :- RNil
