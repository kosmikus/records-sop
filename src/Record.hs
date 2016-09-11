{-# LANGUAGE TypeInType, TypeFamilies, GADTs, DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes, ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Record where

import Generics.SOP.BasicFunctors
import Generics.SOP.NP
import Generics.SOP.NS
import Generics.SOP.Universe
import Generics.SOP.Sing
import Generics.SOP.Type.Metadata
import GHC.TypeLits
import GHC.Types
import Unsafe.Coerce

-- * A suitable representation for single-constructor records.

-- | On the type-level, we represent fiel labels using symbols.
type FieldLabel = Symbol

-- | The record code deviates from the normal SOP code in two
-- ways:
--
-- - There is only one list, because we require that there is
--   only a single constructor.
--
-- - In addition to the types of the fields, we store the labels
--   of the fields.
--
type RecordCode = [(FieldLabel, Type)]

-- | The representation of a record is just a product indexed by
-- the record code, containing elements of the types indicated
-- by the code.
--
-- Note that the representation is deliberately chosen such that
-- it has the same run-time representation as the product part
-- of the normal SOP representation.
--
type Record (r :: RecordCode) = NP P r

type RecordRep (a :: Type) = Record (RecordCodeOf a)

-- * Computing the record code

-- | This type-level function takes the type-level metadata provided
-- by generics-sop as well as the normal generics-sop code, and transforms
-- them into the record code.
--
-- Arguably, the record code is more usable than the representation
-- directly on offer by generics-sop. So it's worth asking whether
-- this representation should be included in generics-sop ...
--
-- The function will only reduce if the argument type actually is a
-- record, meaning it must have exactly one constructor, and that
-- constructor must have field labels attached to it.
--
type RecordCodeOf a = ToRecordCode_Datatype (DatatypeInfoOf a) (Code a)

-- | Helper for 'RecordCodeOf', handling the datatype level. Both
-- datatypes and newtypes are acceptable. Newtypes are just handled
-- as one-constructor datatypes for this purpose.
--
type family ToRecordCode_Datatype (d :: DatatypeInfo) (c :: [[Type]]) :: RecordCode where
  ToRecordCode_Datatype (ADT _ _ cis)    c = ToRecordCode_Constructor cis c
  ToRecordCode_Datatype (Newtype _ _ ci) c = ToRecordCode_Constructor '[ ci ] c

-- | Helper for 'RecordCodeOf', handling the constructor level. Only
-- single-constructor types are acceptable, and the constructor must
-- contain field labels.
--
type family ToRecordCode_Constructor (cis :: [ConstructorInfo]) (c :: [[Type]]) :: RecordCode where
  ToRecordCode_Constructor '[ 'Record _ fis ] '[ ts ] = ToRecordCode_Field fis ts

-- | Helper for 'RecordCodeOf', handling the field level. At this point,
-- we simply zip the list of field names and the list of types.
--
type family ToRecordCode_Field (fis :: [FieldInfo]) (c :: [Type]) :: RecordCode where
  ToRecordCode_Field '[]                    '[]        = '[]
  ToRecordCode_Field ( 'FieldInfo l : fis ) ( t : ts ) = '(l, t) : ToRecordCode_Field fis ts

-- * Compatibility of the record code and the original code.
--
-- If things are correct, the generics-sop code and the record code are strongly
-- related. Stripping all the field names from the record code should give us the
-- original code.

type family ExtractTypesFromRecordCode (r :: RecordCode) :: [Type] where
  ExtractTypesFromRecordCode '[]               = '[]
  ExtractTypesFromRecordCode ( '(_, a) : sig ) = a : ExtractTypesFromRecordCode sig

type family ExtractLabelsFromRecordCode (r :: RecordCode) :: [FieldLabel] where
  ExtractLabelsFromRecordCode '[]               = '[]
  ExtractLabelsFromRecordCode ( '(l, _) : sig ) = l : ExtractLabelsFromRecordCode sig

type family RecombineRecordCode (ls :: [FieldLabel]) (ts :: [Type]) :: RecordCode where
  RecombineRecordCode _ '[]       = '[]
  RecombineRecordCode ls (t : ts) = '(Head ls, t) : RecombineRecordCode (Tail ls) ts

type ValidRecordCode sig xs =
  ( ExtractTypesFromRecordCode sig ~ xs
  , RecombineRecordCode (ExtractLabelsFromRecordCode sig) xs ~ sig
  )

type IsRecord a sig = IsRecord' a sig (GetSingleton (Code a))

type IsRecord' a sig xs =
  ( Generic a, Code a ~ '[ xs ]
  , RecordCodeOf a ~ sig, ValidRecordCode sig xs
  )

toRecord :: (IsRecord a sig) => a -> RecordRep a
toRecord = unsafeToRecord_NP . unZ . unSOP . from

toRecord_NP :: (ValidRecordCode sig xs) => NP I xs -> Record sig
toRecord_NP Nil         = Nil
toRecord_NP (I x :* xs) = P x :* toRecord_NP xs

unsafeToRecord_NP :: (ValidRecordCode sig xs) => NP I xs -> Record sig
unsafeToRecord_NP = unsafeCoerce

fromRecord :: (IsRecord a sig) => RecordRep a -> a
fromRecord = to . SOP . Z . unsafeFromRecord_NP

fromRecord_NP :: forall sig xs . (ValidRecordCode sig xs, SListI xs) => Record sig -> NP I xs
fromRecord_NP = case sList :: SList xs of
  SNil  -> const Nil
  SCons -> \ r -> case r of
    P x :* xs -> I x :* fromRecord_NP xs

unsafeFromRecord_NP :: forall sig xs . (ValidRecordCode sig xs, SListI xs) => Record sig -> NP I xs
unsafeFromRecord_NP = unsafeCoerce

-- * Utilities
 
-- | Projection of the second component of a type-level pair,
-- wrapped in a newtype.
--
newtype P (p :: (a, Type)) = P (Snd p)

-- | Type-level variant of 'snd'.
type family Snd (p :: (a, b)) :: b where
  Snd '(a, b) = b

-- | Type-level variant of 'head'.
type family Head (xs :: [k]) :: k where
  Head (x : xs) = x

-- | Type-level variant of 'tail'.
type family Tail (xs :: [k]) :: [k] where
  Tail (x : xs) = xs

-- | Partial type-level function that extracts the only element
-- from a singleton type-level list.
--
type family GetSingleton (xs :: [k]) :: k where
  GetSingleton '[ x ] = x
