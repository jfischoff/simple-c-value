{-# LANGUAGE DeriveGeneric, KindSignatures, TemplateHaskell, 
    QuasiQuotes, FlexibleInstances, TypeOperators, TypeSynonymInstances,
    MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances,
    ScopedTypeVariables, EmptyDataDecls, DefaultSignatures, ViewPatterns,
    UndecidableInstances, FlexibleContexts, StandaloneDeriving, IncoherentInstances,
    DeriveDataTypeable, RankNTypes #-}
module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Debug.Trace.Helpers
import Debug.Trace
import Test.QuickCheck.Checkers
import Data.List
--import Language.C.Simple.CType.Build
--import Language.C.Simple.CType
import Language.C.Simple.CValue
import Foreign.C.Types
import GHC.Generics
import Data.DeriveTH  
import Control.Applicative
  
--main = quickCheck propertyToCValue_3
  
main = defaultMainWithArgs tests ["-a 100", "-o 5"]    

tests = [
        testGroup "ToCValue" [
            testProperty "propertyToCValue_0" propertyToCValue_0,
            testProperty "propertyToCValue_1" propertyToCValue_1,
            testProperty "propertyToCValue_2" propertyToCValue_2,
            testProperty "propertyToCValue_3" propertyToCValue_3,
            testProperty "propertyToCValue_4" propertyToCValue_4,
            testProperty "propertyToCValue_5" propertyToCValue_5,
            testProperty "propertyToCValue_6" propertyToCValue_6,
            testProperty "propertyToCValue_7" propertyToCValue_7,
            testProperty "propertyToCValue_8" propertyToCValue_8,
            testProperty "propertyToCValue_9" propertyToCValue_9,
            testProperty "propertyToCValue_10" propertyToCValue_10,
            testProperty "propertyToCValue_11" propertyToCValue_11,
            testProperty "propertyToCValue_12" propertyToCValue_12,
            testProperty "propertyToCValue_13" propertyToCValue_13,
            testProperty "propertyToCValue_14" propertyToCValue_14, 
            testCase "testFromCValuePrimitive_0" testFromCValuePrimitive_0,
            testCase "testFromCValuePrimitive_1" testFromCValuePrimitive_1,
            testCase "testFromCValueTest0" testFromCValueTest0,
            testCase "testFromCValueTest1" testFromCValueTest1,
            testCase "testFromCValueTest2" testFromCValueTest2,  
            testCase "testFromCValueTest3" testFromCValueTest3,
            testCase "testFromCValueTest4" testFromCValueTest4 
        ]
    ]

roundTrip :: (ToCValue a, FromCValue a, Eq a) => a -> Bool
roundTrip x = case (fromCValue $ toCValue x) of
                Right y -> x == y
                Left y -> error y

propertyToCValue_0 :: Test0 -> Bool
propertyToCValue_0 = roundTrip

propertyToCValue_1 :: Test1 -> Bool
propertyToCValue_1 = roundTrip

propertyToCValue_2 :: Test2 -> Bool
propertyToCValue_2 = roundTrip

propertyToCValue_3 :: Test3 -> Bool
propertyToCValue_3 = roundTrip

propertyToCValue_4 :: Test4 -> Bool
propertyToCValue_4 = roundTrip

propertyToCValue_5 :: Test5 -> Bool
propertyToCValue_5 = roundTrip

propertyToCValue_6 :: Test6 -> Bool
propertyToCValue_6 = roundTrip

propertyToCValue_7 :: Test7 -> Bool
propertyToCValue_7 = roundTrip

propertyToCValue_8 :: Test8 -> Bool
propertyToCValue_8 = roundTrip

propertyToCValue_9 :: Test9 -> Bool
propertyToCValue_9 = roundTrip

propertyToCValue_10 :: Test10 -> Bool
propertyToCValue_10 = roundTrip

propertyToCValue_11 :: Test11 -> Bool
propertyToCValue_11 = roundTrip

propertyToCValue_12 :: Test12 -> Bool
propertyToCValue_12 = roundTrip

propertyToCValue_13 :: Test13 -> Bool
propertyToCValue_13 = roundTrip

propertyToCValue_14 :: Test14 -> Bool
propertyToCValue_14 = roundTrip

testFromCValuePrimitive_0 = actual @?= Right expected where
    actual   = fromCValue initial
    expected = PChar $ fromIntegral 1
    initial  = VPrimitive $ expected

testFromCValuePrimitive_1 = actual @?= Right expected where
    actual   = fromCValue initial
    expected = PInt $ fromIntegral 1
    initial  = VPrimitive $ expected
    
testFromCValueTest0 = actual @?= Right expected where
    actual   = fromCValue initial
    expected = Test0 $ fromIntegral 1
    initial  = VUnion [Lft] (VPrimitive (PInt (1)))

testFromCValueTest1 = actual @?= Right expected where
    actual   = fromCValue initial
    expected = Test1 (fromIntegral 1) (fromIntegral 2)
    initial  = VUnion [Lft] $ VStruct [
        VMember $ VPrimitive $ PInt 1, 
        VMember $ VPrimitive $ PInt 2]
        
testFromCValueTest2 = actual @?= Right expected where
    actual   = fromCValue initial
    expected = Option0 $ Test0 $ fromIntegral 1
    initial  = VUnion [Lft] $ VUnion [Lft] $ VPrimitive $ PInt $ 1

testFromCValueTest3 = actual @?= Right expected where
    actual   = fromCValue initial
    expected = Option1 $ Test1 (fromIntegral 1) (fromIntegral 2)
    initial  = VUnion [Rght] $ VUnion [Lft] $ VStruct [
        VMember $ VPrimitive $ PInt 1, 
        VMember $ VPrimitive $ PInt 2]
    
testFromCValueTest4 = actual @?= Right expected where
    actual   = fromCValue initial
    expected = OptionB (Test1 0 (-1))
    initial  = VUnion [Lft, Rght] (VUnion [Lft] (VStruct [VMember (VPrimitive (PInt 0)),
        VMember (VPrimitive (PInt (-1)))]))


data Test0 = Test0 CInt
    deriving(Eq, Show, Generic)
    
instance (ToCValue a, ToCValue b) => ToCValue (a, b)
instance (FromCValue a, FromCValue b) => FromCValue (a, b)

instance ToCValue Test0
instance FromCValue Test0

instance Arbitrary CInt where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Int)

instance Arbitrary Test0 where
    arbitrary = Test0 <$> arbitrary

data Test1 = Test1 CInt CInt
    deriving(Eq, Show, Generic)

instance ToCValue Test1
instance FromCValue Test1

instance Arbitrary Test1 where
    arbitrary = Test1 <$> arbitrary <*> arbitrary

data Test2 = Test2 (CInt, CInt)
    deriving(Eq, Show, Generic)

instance ToCValue Test2
instance FromCValue Test2

instance Arbitrary Test2 where
    arbitrary = Test2 <$> arbitrary
    
data Test3 = Option0 Test0
           | Option1 Test1
           deriving(Eq, Show, Generic)    
           
instance ToCValue Test3
instance FromCValue Test3

instance Arbitrary Test3 where
   arbitrary = do
       test <- arbitrary 
       if test
           then Option0 <$> arbitrary
           else Option1 <$> arbitrary
           
           
data Test4 = OptionA Test0
          | OptionB Test1
          | OptionC Test2
          | OptionD Test3
          deriving(Eq, Show, Generic)    

instance ToCValue Test4
instance FromCValue Test4

instance Arbitrary Test4 where
  arbitrary = do
      l <- choose(0, 3 :: Int)
      case l of
          0 -> OptionA <$> arbitrary
          1 -> OptionB <$> arbitrary
          2 -> OptionC <$> arbitrary
          3 -> OptionD <$> arbitrary


data Test5 = Test5 [CInt]
    deriving(Eq, Show, Generic)

instance ToCValue Test5
instance FromCValue Test5

instance Arbitrary Test5 where
  arbitrary = Test5 <$> arbitrary

newtype Test6 = Test6 Test5
    deriving(Eq, Show, Generic)

instance ToCValue Test6
instance FromCValue Test6

instance Arbitrary Test6 where
  arbitrary = Test6 <$> arbitrary
  
  
data Test7 = Test7 
    {
        test0 :: Test0,
        test1 :: Test1,
        test2 :: Test2,
        test3 :: Test3,
        test4 :: Test4,
        test5 :: Test5,
        test6 :: Test6
    }
    deriving(Eq, Show, Generic)

instance ToCValue Test7
instance FromCValue Test7

instance Arbitrary Test7 where
  arbitrary = Test7 <$> arbitrary <*> arbitrary <*> arbitrary
                                  <*> arbitrary <*> arbitrary 
                                  <*> arbitrary <*> arbitrary 


data Test8 = Test8 {run :: CInt}
    deriving(Eq, Show, Generic)

instance ToCValue Test8
instance FromCValue Test8

instance Arbitrary Test8 where
  arbitrary = Test8 <$> arbitrary
  
  
data Test9 = Test9 
    {
        testA :: Test0,
        testB :: Test1,
        testC :: Test2
    }
    deriving(Eq, Show, Generic)

instance ToCValue Test9
instance FromCValue Test9

instance Arbitrary Test9 where
  arbitrary = Test9 <$> arbitrary <*> arbitrary <*> arbitrary
                                  
data Test10 = Test10
  {
      testA1 :: Test0
  }
  deriving(Eq, Show, Generic)

instance ToCValue Test10
instance FromCValue Test10

instance Arbitrary Test10 where
 arbitrary = Test10 <$> arbitrary 

data Test11 = Test11
    {
       testA1A :: Test0,
       testA1B :: Test1
    }
 deriving(Eq, Show, Generic)

instance ToCValue Test11
instance FromCValue Test11

instance Arbitrary Test11 where
  arbitrary = Test11 <$> arbitrary <*> arbitrary
  
data Test12 = Test12
    {
        testAA :: Test2
    }
    deriving(Eq, Show, Generic)

instance ToCValue Test12
instance FromCValue Test12

instance Arbitrary Test12 where
    arbitrary = Test12 <$> arbitrary  


data Test13 = Test13
    {
        testAAA :: Test1,
        testAAB :: Test2
    }
    deriving(Eq, Show, Generic)

instance ToCValue Test13
instance FromCValue Test13

instance Arbitrary Test13 where
    arbitrary = Test13 <$> arbitrary <*> arbitrary

data Test14 = Test14 CInt CInt CInt
  deriving(Eq, Show, Generic)

instance ToCValue Test14
instance FromCValue Test14

instance Arbitrary Test14 where
  arbitrary = Test14 <$> arbitrary <*> arbitrary <*> arbitrary

data Test15 = Thing0
            | Thing1
            | Thing2
          deriving(Eq, Show, Generic)    
          
          
data Test16 = Blah0
            | Blah1
            | Blah2
            | Blah3
            | Blah4
            | Blah5
        deriving(Eq, Show, Generic)



