{-# LANGUAGE
    CPP
  , DataKinds
  , DeriveGeneric
  , DuplicateRecordFields
  , TypeApplications
#-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module Main where

import Control.DeepSeq
import qualified GHC.Generics as GHC
import Test.Hspec
import Test.ShouldNotTypecheck

import Generics.SOP
import Generics.SOP.Record.SubTyping

data X = MkX {}
  deriving (Eq, Show, GHC.Generic)

instance Generic X
instance HasDatatypeInfo X
instance NFData X

data A = MkA { anInt :: Int, aBool :: Bool }
  deriving (Eq, Show, GHC.Generic)

instance Generic A
instance HasDatatypeInfo A
instance NFData A

data B = MkB { anInt :: Int, aBool :: Bool, aChar :: Char }
  deriving (Eq, Show, GHC.Generic)

instance Generic B
instance HasDatatypeInfo B
instance NFData B

-- Permutation.
data C = MkC { aBool :: Bool, aChar :: Char, anInt :: Int }
  deriving (Eq, Show, GHC.Generic)

instance Generic C
instance HasDatatypeInfo C
instance NFData C

#if __GLASGOW_HASKELL__ < 902
-- Duplicate label within a single record (works prior to ghc-9.2).
data D = MkD { anInt :: Int, anInt :: Int }
  deriving (Eq, Show, GHC.Generic)

instance Generic D
instance HasDatatypeInfo D
instance NFData D
#endif

-- Wrong type.
data E = MkE { anInt :: Int, aBool :: Bool, aChar :: () }
  deriving (Eq, Show, GHC.Generic)

instance Generic E
instance HasDatatypeInfo E
instance NFData E

-- No field labels.
data F = MkF Int Bool Char
  deriving (Eq, Show, GHC.Generic)

instance Generic F
instance HasDatatypeInfo F
instance NFData F

-- Two constructors.
data G = MkG { anInt :: Int, aBool :: Bool, aChar :: Char }
       | OtherG
  deriving (Eq, Show, GHC.Generic)

instance Generic G
instance HasDatatypeInfo G
instance NFData G

a :: A
a = MkA 3 True

b :: B
b = MkB 3 True 'x'

c :: C
c = MkC True 'x' 3

#if __GLASGOW_HASKELL__ < 902
d :: D
d = MkD 3 3

d' :: D
d' = MkD 3 4
#endif

e :: E
e = MkE 3 True ()

f :: F
f = MkF 3 True 'x'

g :: G
g = MkG 3 True 'x'

x :: X
x = MkX {}

main :: IO ()
main = hspec $
  describe "cast" $ do
    it "successfully casts X to X" $
      (cast x :: X) `shouldBe` x
    it "successfully casts A to A" $
      (cast a :: A) `shouldBe` a
    it "successfully casts A to X" $
      (cast a :: X) `shouldBe` x
    it "successfully casts B to B" $
      (cast b :: B) `shouldBe` b
    it "successfully casts B to X" $
      (cast b :: X) `shouldBe` x
    it "successfully casts B to A" $
      (cast b :: A) `shouldBe` a
    it "successfully casts B to C" $
      (cast b :: C) `shouldBe` c
#if __GLASGOW_HASKELL__ < 902
    it "successfully casts A to D" $
      (cast a :: D) `shouldBe` d
    it "successfully casts B to D" $
      (cast b :: D) `shouldBe` d
    it "successfully casts C to D" $
      (cast c :: D) `shouldBe` d
    it "successfully casts D to D" $
      (cast d :: D) `shouldBe` d
    it "prefers the first element when casting D to D" $
      (cast d' :: D) `shouldBe` d
    it "successfully casts E to D" $
      (cast e :: D) `shouldBe` d
    it "successfully casts D to X" $
      (cast d :: X) `shouldBe` x
#endif
    it "successfully casts C to X" $
      (cast c :: X) `shouldBe` x
    it "successfully casts C to A" $
      (cast c :: A) `shouldBe` a
    it "successfully casts C to B" $
      (cast c :: B) `shouldBe` b
    it "successfully casts E to E" $
      (cast e :: E) `shouldBe` e
    it "successfully casts E to X" $
      (cast e :: X) `shouldBe` x
    it "successfully casts E to A" $
      (cast e :: A) `shouldBe` a
    it "correctly fails to cast A to B" $
      shouldNotTypecheck (cast a :: B)
    it "correctly fails to cast A to C" $
      shouldNotTypecheck (cast a :: C)
#if __GLASGOW_HASKELL__ < 902
    it "correctly fails to cast D to A" $
      shouldNotTypecheck (cast d :: A)
    it "correctly fails to cast D to B" $
      shouldNotTypecheck (cast d :: B)
    it "correctly fails to cast D to C" $
      shouldNotTypecheck (cast d :: C)
#endif
    it "correctly fails to cast E to B" $
      shouldNotTypecheck (cast e :: B)
    it "correctly fails to cast E to C" $
      shouldNotTypecheck (cast e :: C)
    -- The following two produce type errors as expected.
    -- Unfortunately, user-defined type errors in combination
    -- with shouldNotTypeCheck seems to trigger an internal
    -- error ... (see GHC bug #12104) [fixed in 8.2]
#if __GLASGOW_HASKELL__ >= 802
    it "fails to cast F to anything (even X)" $
      shouldNotTypecheck (cast f :: X)
    it "fails to cast G to anything (even X)" $
      shouldNotTypecheck (cast g :: X)
#endif
    it "successfully extracts anInt from A" $
      getField @"anInt" a `shouldBe` 3
    it "successfully extracts aBool from A" $
      getField @"aBool" a `shouldBe` True
    it "correctly fails to extract aChar from A" $
      shouldNotTypecheck (getField @"aChar" a)
    it "successfully extracts anInt from B" $
      getField @"anInt" b `shouldBe` 3
    it "successfully extracts aBool from B" $
      getField @"aBool" b `shouldBe` True
    it "successfully extracts aChar from B" $
      getField @"aChar" b `shouldBe` 'x'
    it "successfully extracts anInt from C" $
      getField @"anInt" c `shouldBe` 3
    it "successfully extracts aBool from C" $
      getField @"aBool" c `shouldBe` True
    it "successfully extracts aChar from C" $
      getField @"aChar" c `shouldBe` 'x'
#if __GLASGOW_HASKELL__ < 902
    it "successfully extracts the first anInt from D" $
      getField @"anInt" d' `shouldBe` 3
#endif
