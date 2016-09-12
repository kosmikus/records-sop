{-# LANGUAGE
    DeriveGeneric
  , DuplicateRecordFields
#-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Main where

import Control.DeepSeq
import qualified GHC.Generics as GHC
import Test.Hspec
import Test.ShouldNotTypecheck

import Generics.SOP
import Generics.SOP.Record
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

-- Duplicate label within a single record (works!).
data D = MkD { anInt :: Int, anInt :: Int }
  deriving (Eq, Show, GHC.Generic)

instance Generic D
instance HasDatatypeInfo D
instance NFData D

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

d :: D
d = MkD 3 3

d' :: D
d' = MkD 3 4

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
    it "successfully casts A to D" $
      (cast a :: D) `shouldBe` d
    it "successfully casts B to B" $
      (cast b :: B) `shouldBe` b
    it "successfully casts B to X" $
      (cast b :: X) `shouldBe` x
    it "successfully casts B to A" $
      (cast b :: A) `shouldBe` a
    it "successfully casts B to C" $
      (cast b :: C) `shouldBe` c
    it "successfully casts B to D" $
      (cast b :: D) `shouldBe` d
    it "successfully casts C to X" $
      (cast c :: X) `shouldBe` x
    it "successfully casts C to A" $
      (cast c :: A) `shouldBe` a
    it "successfully casts C to B" $
      (cast c :: B) `shouldBe` b
    it "successfully casts C to D" $
      (cast c :: D) `shouldBe` d
    it "successfully casts D to D" $
      (cast d :: D) `shouldBe` d
    it "prefers the first element when casting D to D" $
      (cast d' :: D) `shouldBe` d
    it "successfully casts D to X" $
      (cast d :: X) `shouldBe` x
    it "successfully casts E to E" $
      (cast e :: E) `shouldBe` e
    it "successfully casts E to X" $
      (cast e :: X) `shouldBe` x
    it "successfully casts E to A" $
      (cast e :: A) `shouldBe` a
    it "successfully casts E to D" $
      (cast e :: D) `shouldBe` d
    it "fails to cast A to B" $
      shouldNotTypecheck (cast a :: B)
    it "fails to cast A to C" $
      shouldNotTypecheck (cast a :: C)
    it "fails to cast D to A" $
      shouldNotTypecheck (cast d :: A)
    it "fails to cast D to B" $
      shouldNotTypecheck (cast d :: B)
    it "fails to cast D to C" $
      shouldNotTypecheck (cast d :: C)
    it "fails to cast E to B" $
      shouldNotTypecheck (cast e :: B)
    it "fails to cast E to C" $
      shouldNotTypecheck (cast e :: C)
    -- The following two produce type errors as expected.
    -- Unfortunately, user-defined type errors in combination
    -- with shouldNotTypeCheck seems to trigger an internal
    -- error ...
    {-
    it "fails to cast F to anything (even X)" $
      shouldNotTypeCheck (cast f :: X)
    it "fails to cast G to anything (even X)" $
      shouldNotTypeCheck (cast g :: X)
    -}
