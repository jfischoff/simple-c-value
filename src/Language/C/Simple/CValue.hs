{-# LANGUAGE DeriveGeneric, KindSignatures, 
    FlexibleInstances, TypeOperators, TypeSynonymInstances,
    MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances,
    ScopedTypeVariables, EmptyDataDecls, DefaultSignatures,
    UndecidableInstances, FlexibleContexts, StandaloneDeriving, IncoherentInstances,
    DeriveDataTypeable #-}
module Language.C.Simple.CValue (
 CValue(..),
 ToCValue(..),
 FromCValue(..), 
 Side(..),
 UnionPath(..),
 PrimitiveValue(..)
 )
 where
import Data.Word
import Data.Data
import GHC.Generics
import Foreign.C.Types
import Control.Applicative
import Data.List
import Data.DList (DList, toList)
import Data.Monoid (mappend)
import Debug.Trace
import Debug.Trace.Helpers
import Data.Tuple.Select
import Control.Monad ((<=<))
import Data.Binary

debug = False

traceDebug msg x = if debug 
    then trace msg x
    else x

-- | A step in union path
data Side = Lft
          | Rght
          deriving(Show, Eq, Read, Ord, Data, Typeable, Generic)
-- | This is used for the conversion from a CValue back to Haskell type. Ideally it should be
--   and index, but unforunately this does not work with the way Generics creates its :+: binary tree.
--   I'm leaving it here for now, but I might find a more elegant way to handle this.
type UnionPath = [Side]

deriving instance Data CChar
deriving instance Data CSChar 
deriving instance Data CUChar 
deriving instance Data CShort 
deriving instance Data CUShort
deriving instance Data CInt  
deriving instance Data CUInt 
deriving instance Data CLong 
deriving instance Data CULong
deriving instance Data CPtrdiff
deriving instance Data CSize
deriving instance Data CWchar
deriving instance Data CSigAtomic
deriving instance Data CLLong
deriving instance Data CULLong
deriving instance Data CIntPtr
deriving instance Data CUIntPtr
deriving instance Data CIntMax
deriving instance Data CUIntMax
deriving instance Data CClock
deriving instance Data CTime
deriving instance Data CUSeconds
deriving instance Data CSUSeconds
deriving instance Data CFloat
deriving instance Data CDouble

-- | Primitive C values
data PrimitiveValue = PChar      CChar  
                    | PSChar     CSChar 
                    | PUChar     CUChar 
                    | PShort     CShort 
                    | PUShort    CUShort
                    | PInt       CInt  
                    | PUInt      CUInt 
                    | PLong      CLong 
                    | PULong     CULong
                    | PPtrdiff   CPtrdiff
                    | PSize      CSize
                    | PWchar     CWchar
                    | PSigAtomic CSigAtomic
                    | PLLong     CLLong
                    | PULLong    CULLong
                    | PIntPtr    CIntPtr
                    | PUIntPtr   CUIntPtr
                    | PIntMax    CIntMax
                    | PUIntMax   CUIntMax
                    | PClock     CClock
                    | PTime      CTime
                    | PUSeconds  CUSeconds
                    | PSUSeconds CSUSeconds
                    | PFloat     CFloat
                    | PDouble    CDouble
                    deriving(Eq, Show, Read, Ord, Data, Typeable, Generic)
                    
          
 
-- | A generic C value
data CValue = VStruct [CValue]
            | VUnion UnionPath CValue 
            | VPrimitive PrimitiveValue
            | VArray [CValue]
            | VMember CValue
            | Void
            deriving(Show, Eq, Read, Ord, Data, Typeable, Generic)
            
------------------------------------------------------------------------------------
-- | ToCValue Class
------------------------------------------------------------------------------------
class ToCValue a where
    toCValue :: a -> CValue
    default toCValue :: (Generic a, GToCValue (Rep a)) => a -> CValue
    toCValue a = gToCValue (from a)    
    
------------------------------------------------------------------------------------
-- Primitive Instances
------------------------------------------------------------------------------------
instance ToCValue CFloat where
    toCValue = VPrimitive . PFloat 
    
instance ToCValue CDouble where
    toCValue = VPrimitive . PDouble 

instance ToCValue CChar where
    toCValue = VPrimitive . PChar
    
instance ToCValue CSChar where
    toCValue = VPrimitive . PSChar
    
instance ToCValue CUChar where
    toCValue = VPrimitive . PUChar
    
instance ToCValue CShort where
    toCValue = VPrimitive . PShort
    
instance ToCValue CUShort where
    toCValue = VPrimitive . PUShort
    
instance ToCValue CInt where
    toCValue = VPrimitive . PInt
    
instance ToCValue CUInt where
    toCValue = VPrimitive . PUInt
    
instance ToCValue CLong where
    toCValue = VPrimitive . PLong
    
instance ToCValue CULong where
    toCValue = VPrimitive . PULong
    
instance ToCValue CPtrdiff where
    toCValue = VPrimitive . PPtrdiff
    
instance ToCValue CSize where
    toCValue = VPrimitive . PSize
    
instance ToCValue CWchar where
    toCValue = VPrimitive . PWchar
    
instance ToCValue CSigAtomic where
    toCValue = VPrimitive . PSigAtomic
    
instance ToCValue CLLong where
    toCValue = VPrimitive . PLLong
    
instance ToCValue CULLong where
    toCValue = VPrimitive . PULLong
    
instance ToCValue CIntPtr where
    toCValue = VPrimitive . PIntPtr
    
instance ToCValue CUIntPtr where
    toCValue = VPrimitive . PUIntPtr
    
instance ToCValue CIntMax where
    toCValue = VPrimitive . PIntMax
    
instance ToCValue CUIntMax where
    toCValue = VPrimitive . PUIntMax
    
instance ToCValue CClock where
    toCValue = VPrimitive . PClock
    
instance ToCValue CTime where
    toCValue = VPrimitive . PTime
    
instance ToCValue CUSeconds where
    toCValue = VPrimitive . PUSeconds
    
instance ToCValue CSUSeconds where
    toCValue = VPrimitive . PSUSeconds
-------------------------------------- Other Instances -----------------------------
instance ToCValue a => ToCValue [a] where
    toCValue = VArray . map toCValue

------------------------------------------------------------------------------------
-- Derive from this to convert a Haskell Type to a CValue
------------------------------------------------------------------------------------

--convert
class GToCValue f where
  gToCValue :: f a -> CValue

instance GToCValue U1 where
  gToCValue U1 = Void

instance (Datatype d, DispatchConstructor a, GToCValueList a) => GToCValue (D1 d a) where
  gToCValue = dispatchCon . unM1 . traceDebug "GToCValue (D1 d a)"

instance (Constructor c, GToCValue a) => GToCValue (C1 c a) where
    gToCValue = gToCValue . unM1 . traceDebug "GToCValue (C1 c a)"
    
instance (Selector s, GToCValue a) => GToCValue (S1 s a) where
    gToCValue = gToCValue . unM1 . traceDebug "GToCValue (S1 s a)"

instance (ToCValue a) => GToCValue (K1 i a) where
  gToCValue = toCValue . unK1 . traceDebug "GToCValue (K1 i a)"

instance (GToCValue a, GToCValue b, GConArgToLit a, GConArgToLit b) => GToCValue (a :*: b) where
    gToCValue =  VStruct . toList . gConArgToLit . traceDebug "GToCValue (a :*: b)"
-- Ripped from Aeson
--
class GConArgToLit f where
  gConArgToLit :: f a -> DList CValue 

instance (GConArgToLit a, GConArgToLit b) => GConArgToLit (a :*: b) where
  gConArgToLit (a :*: b) = gConArgToLit a `mappend` (gConArgToLit $ traceDebug "GConArgToLit (a :*: b)" b)

instance (Selector s, GToCValue a) => GConArgToLit (S1 s a) where
  gConArgToLit a = pure (VMember $ gToCValue $ traceDebug "GConArgToLit a" a)
  

--------------------------------------------------------------------------------
class DispatchConstructor    f where dispatchCon  :: f a -> CValue
class DispatchConstructor' b f where dispatchCon' :: Tagged b (f a -> CValue)

instance (IsSum f b, DispatchConstructor' b f, GToCValueList f) => DispatchConstructor f where
    dispatchCon = unTagged (dispatchCon' :: Tagged b (f a -> CValue))

instance (GToCValueList a) => DispatchConstructor' True a where
    dispatchCon' = Tagged (toSum . traceDebug "DispatchConstructor' True a")

toSum :: (GToCValueList f) => f a -> CValue 
toSum x = result where
    result = VUnion index ctype 
    (index, ctype) = toUnionPathValue $ gToCValueList $ traceDebug "GToCValue (a :+: b)" x

instance (GToCValue f) => DispatchConstructor' False f where
    dispatchCon' = Tagged (VUnion [Lft] . gToCValue . traceDebug "DispatchConstructor' False f")
--------------------------------------------------------------------------------
data True
data False

newtype Tagged s b = Tagged {unTagged :: b}

data CValueList = LeftSucc CValueList
                | RightSucc CValueList
                | Nil CValue

--http://www.haskell.org/haskellwiki/GHC/AdvancedOverlap
--To pick an instance based on the context
class GToCValueList    f where gToCValueList  :: f a -> CValueList
class GToCValueList' b f where gToCValueList' :: Tagged b (f a -> CValueList)

instance (IsSum f b, GToCValueList' b f) => GToCValueList f where
    gToCValueList = unTagged (gToCValueList' :: Tagged b (f a -> CValueList))
    
instance (GToCValue f) => GToCValueList' False f where
    gToCValueList' = Tagged (Nil . gToCValue)
    
instance (GToCValueList a, GToCValueList b) => GToCValueList' True (a :+: b) where
    gToCValueList' = Tagged $ \x -> case x of 
                                            R1 x -> RightSucc (gToCValueList $ traceDebug "NilR" x)
                                            L1 x -> LeftSucc (gToCValueList $ traceDebug "Succ" x)

    
toUnionPath :: CValueList -> UnionPath
toUnionPath (LeftSucc x)  = Lft:(toUnionPath $ traceDebug "LeftSucc" x)
toUnionPath (RightSucc x) = Rght:(toUnionPath $ traceDebug "RightSucc" x)    
toUnionPath (Nil _)       = []


retrieveValue :: CValueList -> CValue
retrieveValue (RightSucc x) = retrieveValue x
retrieveValue (LeftSucc x) = retrieveValue x
retrieveValue (Nil x) = x

toUnionPathValue :: CValueList -> (UnionPath, CValue)
toUnionPathValue x = (toUnionPath x, retrieveValue x)


---------------------------------------------------------------------------------
-- | Derive from this to convert from a CValue to Haskell type
---------------------------------------------------------------------------------
class FromCValue a where
    fromCValue :: CValue -> Either String a
    default fromCValue :: (Generic a, GFromCValue (Rep a)) => CValue -> Either String a
    fromCValue = fmap to . gFromCValue . traceDebug "fromCValue"
------------------------------------Instances-------------------------------------

instance FromCValue PrimitiveValue where
    fromCValue (VPrimitive x) = Right $ traceDebug "FromCValue PrimitiveValue" x
    fromCValue x = Left $ show x ++  " is not a VPrimitive"
    
instance FromCValue CSChar where
    fromCValue x = fromPSChar <$> fromCValue x
    
instance FromCValue CUChar where
    fromCValue x = fromPUChar <$> fromCValue x
    
instance FromCValue CShort where
    fromCValue x = fromPShort <$> fromCValue x
    
instance FromCValue CUShort where
    fromCValue x = fromPUShort <$> fromCValue x
    
instance FromCValue CInt where
    fromCValue x = fromPInt <$> fromCValue x
    
instance FromCValue CUInt where
    fromCValue x = fromPUInt <$> fromCValue x
    
instance FromCValue CLong where
    fromCValue x = fromPLong <$> fromCValue x
    
instance FromCValue CULong where
    fromCValue x = fromPULong <$> fromCValue x
    
instance FromCValue CPtrdiff where
    fromCValue x = fromPPtrdiff <$> fromCValue x
    
instance FromCValue CSize where
    fromCValue x = fromPSize <$> fromCValue x
    
instance FromCValue CWchar where
    fromCValue x = fromPWchar <$> fromCValue x
    
instance FromCValue CSigAtomic where
    fromCValue x = fromPSigAtomic <$> fromCValue x
    
instance FromCValue CLLong where
    fromCValue x = fromPLLong <$> fromCValue x
    
instance FromCValue CULLong where
    fromCValue x = fromPULLong <$> fromCValue x

instance FromCValue CIntPtr where
    fromCValue x = fromPIntPtr <$> fromCValue x
    
instance FromCValue CUIntPtr where
    fromCValue x = fromPUIntPtr <$> fromCValue x
    
instance FromCValue CIntMax where
    fromCValue x = fromPIntMax <$> fromCValue x
    
instance FromCValue CUIntMax where
    fromCValue x = fromPUIntMax <$> fromCValue x
    
instance FromCValue CClock where
    fromCValue x = fromPClock <$> fromCValue x
    
instance FromCValue CTime where
    fromCValue x = fromPTime <$> fromCValue x
    
instance FromCValue CUSeconds where
    fromCValue x = fromPUSeconds <$> fromCValue x
    
instance FromCValue CSUSeconds where
    fromCValue x = fromPSUSeconds <$> fromCValue x
    
instance FromCValue CFloat where
    fromCValue x = fromPFloat <$> fromCValue x

instance FromCValue CDouble where
    fromCValue x = fromPDouble <$> fromCValue x
    
-------------------------------------- Other Instances ---------------------------------
instance FromCValue a => FromCValue [a] where
    fromCValue (VArray xs) = mapM fromCValue xs

------------------------------------------------------------------------------------
-- Generic FromCValue Class
------------------------------------------------------------------------------------
{-
    This should start with the D1
    and basically follow the format above

-}
class GFromCValue f where
  gFromCValue :: CValue -> Either String (f a)

instance GFromCValue U1 where
  gFromCValue Void = Right U1
  gFromCValue x    = Left $ "could not convert from " ++ show x ++ " to the unit type"

instance (Datatype d, ConsFromCValue a, GFromSum a) => GFromCValue (D1 d a) where
  gFromCValue = fmap (\x -> traceDebug ("GFromCValue (D1 d a)" ++ datatypeName x) x) . fmap M1 . consParseCValue

instance (Constructor c, GFromCValue a) => GFromCValue (C1 c a) where
  gFromCValue = fmap (\x -> traceDebug ("GFromCValue (C1 d a)" ++ conName x) x) . fmap M1 . gFromCValue

instance (Selector s, GFromCValue a) => GFromCValue (S1 s a) where
  gFromCValue = fmap (traceDebug "GFromCValue (S1 d a)") . fmap M1 . gFromCValue

instance (FromCValue a) => GFromCValue (K1 i a) where
  gFromCValue = fmap K1 . fromCValue . traceDebug "GFromCValue (K1 i a)"

instance (GFromCValue a, GFromCValue b, GFromProduct a, GFromProduct b) => GFromCValue (a :*: b) where
    gFromCValue (VStruct xs) = gParseProduct $ traceDebug "GFromCValue (a :*: b)" xs 
    gFromCValue x = Left $ "could not convert from " ++ show x ++ " to a product type"

--------------------------------------------------------------------------------

class ConsFromCValue    f where consParseCValue  ::           CValue -> Either String (f a)
class ConsFromCValue' b f where consParseCValue' :: Tagged b (CValue -> Either String (f a))

instance (IsSum f b, ConsFromCValue' b f, GFromSum f) => ConsFromCValue f where
    consParseCValue = unTagged (consParseCValue' :: Tagged b (CValue -> Either String (f a))    )

instance (GFromSum f) => ConsFromCValue' True f where
    consParseCValue' = Tagged parseSum

parseSum :: (GFromSum f) => CValue -> Either String (f a )
parseSum (VUnion index x) = gParseSum index $ traceDebug "parseSum" x
parseSum x = Left $ "a sum type must be a union not" ++ show x

instance (GFromCValue f) => ConsFromCValue' False f where
    consParseCValue' = Tagged (gFromCValue <=< (fromUnionE . traceDebug "ConsFromCValue' False f") )

fromUnionE :: CValue -> Either String CValue
fromUnionE (VUnion _ x) = Right x
fromUnionE x            = Left $ "could not convert from " ++ show x ++ " to a union type"

----------------------------------------------------------------------------------------
class GFromProduct f where
    gParseProduct :: [CValue] -> Either String (f a)

instance (GFromProduct a, GFromProduct b) => GFromProduct (a :*: b) where
    gParseProduct []     = Left "parse product can't work with empty lists"
    gParseProduct xs = (:*:) <$> gParseProduct firstHalf
                        <*> (gParseProduct $ traceDebug "GFromProduct (a :*: b)" secondHalf) where
        firstHalf  = take (length xs `div` 2) xs                             
        secondHalf = drop (length xs `div` 2) xs 
 
instance (Selector s, GFromCValue a) => GFromProduct (S1 s a) where
    gParseProduct ((VMember x):[]) = gFromCValue $ traceDebug "GFromProduct (S1 s a)" x 
    gParseProduct x = Left $ "could not convert from " ++ show x ++ " to a selector or member"
----------------------------------------------------------------------------------------    
class GFromSum f where
    gParseSum :: UnionPath -> CValue -> Either String (f a)

instance (GFromSum a, GFromSum b) => GFromSum (a :+: b) where
    gParseSum (Lft:[]) x  = L1 <$> (gParseSum [] $ traceDebug "gParseSum L1 []" x)
    gParseSum (Rght:[]) x = R1 <$> (gParseSum [] $ traceDebug "gParseSum R1 []" x)        
    gParseSum (Lft:ys) x  = L1 <$> (gParseSum ys $ traceDebug "gParseSum L1"  x)
    gParseSum (Rght:ys) x = R1 <$> (gParseSum ys $ traceDebug "gParseSum R1"  x)
    
instance (GFromCValue f) => GFromSum f where
    gParseSum _ x = gFromCValue $ traceDebug "GFromSum (f)" x
                                          
------------------------------------------------------------------------------------------

class IsSum (f :: * -> *) b | f -> b

instance (IsSum f b) => IsSum (M1 S s f) b
instance (IsSum f b) => IsSum (M1 C c f) b
instance (IsSum f b) => IsSum (M1 D c f) b
instance IsSum (f :+: g) True
instance IsSum (f :*: g) False
instance IsSum (K1 i c) False
instance IsSum U1 False



fromPChar :: PrimitiveValue -> CChar
fromPChar (PChar x1) = x1
fromPChar _ = error "fromPChar failed, not a PChar"
fromPSChar :: PrimitiveValue -> CSChar
fromPSChar (PSChar x1) = x1
fromPSChar _ = error "fromPSChar failed, not a PSChar"
fromPUChar :: PrimitiveValue -> CUChar
fromPUChar (PUChar x1) = x1
fromPUChar _ = error "fromPUChar failed, not a PUChar"
fromPShort :: PrimitiveValue -> CShort
fromPShort (PShort x1) = x1
fromPShort _ = error "fromPShort failed, not a PShort"
fromPUShort :: PrimitiveValue -> CUShort
fromPUShort (PUShort x1) = x1
fromPUShort _ = error "fromPUShort failed, not a PUShort"
fromPInt :: PrimitiveValue -> CInt
fromPInt (PInt x1) = x1
fromPInt _ = error "fromPInt failed, not a PInt"
fromPUInt :: PrimitiveValue -> CUInt
fromPUInt (PUInt x1) = x1
fromPUInt _ = error "fromPUInt failed, not a PUInt"
fromPLong :: PrimitiveValue -> CLong
fromPLong (PLong x1) = x1
fromPLong _ = error "fromPLong failed, not a PLong"
fromPULong :: PrimitiveValue -> CULong
fromPULong (PULong x1) = x1
fromPULong _ = error "fromPULong failed, not a PULong"
fromPPtrdiff :: PrimitiveValue -> CPtrdiff
fromPPtrdiff (PPtrdiff x1) = x1
fromPPtrdiff _ = error "fromPPtrdiff failed, not a PPtrdiff"
fromPSize :: PrimitiveValue -> CSize
fromPSize (PSize x1) = x1
fromPSize _ = error "fromPSize failed, not a PSize"
fromPWchar :: PrimitiveValue -> CWchar
fromPWchar (PWchar x1) = x1
fromPWchar _ = error "fromPWchar failed, not a PWchar"
fromPSigAtomic :: PrimitiveValue -> CSigAtomic
fromPSigAtomic (PSigAtomic x1) = x1
fromPSigAtomic _ = error "fromPSigAtomic failed, not a PSigAtomic"
fromPLLong :: PrimitiveValue -> CLLong
fromPLLong (PLLong x1) = x1
fromPLLong _ = error "fromPLLong failed, not a PLLong"
fromPULLong :: PrimitiveValue -> CULLong
fromPULLong (PULLong x1) = x1
fromPULLong _ = error "fromPULLong failed, not a PULLong"
fromPIntPtr :: PrimitiveValue -> CIntPtr
fromPIntPtr (PIntPtr x1) = x1
fromPIntPtr _ = error "fromPIntPtr failed, not a PIntPtr"
fromPUIntPtr :: PrimitiveValue -> CUIntPtr
fromPUIntPtr (PUIntPtr x1) = x1
fromPUIntPtr _ = error "fromPUIntPtr failed, not a PUIntPtr"
fromPIntMax :: PrimitiveValue -> CIntMax
fromPIntMax (PIntMax x1) = x1
fromPIntMax _ = error "fromPIntMax failed, not a PIntMax"
fromPUIntMax :: PrimitiveValue -> CUIntMax
fromPUIntMax (PUIntMax x1) = x1
fromPUIntMax _ = error "fromPUIntMax failed, not a PUIntMax"
fromPClock :: PrimitiveValue -> CClock
fromPClock (PClock x1) = x1
fromPClock _ = error "fromPClock failed, not a PClock"
fromPTime :: PrimitiveValue -> CTime
fromPTime (PTime x1) = x1
fromPTime _ = error "fromPTime failed, not a PTime"
fromPUSeconds :: PrimitiveValue -> CUSeconds
fromPUSeconds (PUSeconds x1) = x1
fromPUSeconds _ = error "fromPUSeconds failed, not a PUSeconds"
fromPSUSeconds :: PrimitiveValue -> CSUSeconds
fromPSUSeconds (PSUSeconds x1) = x1
fromPSUSeconds _ = error "fromPSUSeconds failed, not a PSUSeconds"
fromPFloat :: PrimitiveValue -> CFloat
fromPFloat (PFloat x1) = x1
fromPFloat _ = error "fromPFloat failed, not a PFloat"
fromPDouble :: PrimitiveValue -> CDouble
fromPDouble (PDouble x1) = x1
fromPDouble _ = error "fromPDouble failed, not a PDouble"

fromVStruct :: CValue -> [CValue]
fromVStruct (VStruct x1) = x1
fromVStruct _ = error "fromVStruct failed, not a VStruct"
fromVUnion :: CValue -> ([Side], CValue)
fromVUnion (VUnion x1 x2) = (x1, x2)
fromVUnion _ = error "fromVUnion failed, not a VUnion"
fromVPrimitive :: CValue -> PrimitiveValue
fromVPrimitive (VPrimitive x1) = x1
fromVPrimitive _ = error "fromVPrimitive failed, not a VPrimitive"
fromVArray :: CValue -> [CValue]
fromVArray (VArray x1) = x1
fromVArray _ = error "fromVArray failed, not a VArray"
fromVMember :: CValue -> CValue
fromVMember (VMember x1) = x1
fromVMember _ = error "fromVMember failed, not a VMember"
fromVoid :: CValue -> ()
fromVoid Void = ()
fromVoid _ = error "fromVoid failed, not a Void"







