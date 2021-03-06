{-# language
    AllowAmbiguousTypes
  , TemplateHaskell
  , ScopedTypeVariables
  , DataKinds
  , KindSignatures
  , PolyKinds
  , GADTs
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , QuantifiedConstraints
  , FlexibleInstances
#-}

--{-# options_ghc -ddump-splices #-}

{-# options_ghc -Wtype-defaults #-}

module Test where

import Shwifty
import Data.Proxy
import Data.Kind (Type)
import Data.Void (Void)
import qualified Data.UUID.Types

data OmitFields0 = OmitFields0
  { field01 :: Int
  , field02 :: Bool
  }
$(getShwiftyWith defaultOptions{omitFields = const Discard} ''OmitFields0)

data OmitFields1 = OmitFields1
  { field11 :: Int
  , field12 :: Bool
  }
$(getShwiftyWith defaultOptions{omitFields = \s -> if s `elem` ["field11"] then Discard else Keep} ''OmitFields1)

data OmitCases0
  = OmitCases01
  | OmitCases02
  | OmitCases03
$(getShwiftyWith defaultOptions{omitCases = const Discard} ''OmitCases0)

data OmitCases1
  = OmitCases11
  | OmitCases12
  | OmitCases13
$(getShwiftyWith defaultOptions{omitCases = \s -> if s `elem` ["OmitCases11"] then Discard else Keep} ''OmitCases1)

data CodecTest a = CodecTest
  { codecTestOne :: a
  , codecTestTwo :: Int
  }
$( getShwiftyCodec
     (Codec @
       (   Drop 'Field "codecTest"
         & Implement 'Codable
         & DontGenerate ToSwift
         & OmitField "codecTestOne"
         & MakeBase 'Nothing '[]
       )
     )

     ''CodecTest
 )

data CodecSum a b
  = CodecSumL a
  | CodecSumR b
$( getShwiftyCodec
     (Codec @
        (   MakeBase ('Just 'Str) '[ Equatable, Hashable, Codable ]
          & Drop 'DataCon "CodecSum"
        )
     )
     ''CodecSum
 )

data Fun a b = MkFun
  { fun :: Int -> Char -> Bool -> String -> Either a b
  }
getShwifty ''Fun

newtype Lol = MkLol String
newtype Haha = MkHaha String
data Laughs = MkLaughs { lol :: Lol, haha :: Haha }
$(getShwiftyWithTags defaultOptions [ ''Lol, ''Haha ] ''Laughs)

newtype IsANewtype = MkIsANewtype { getIsANewtype :: String }
$(getShwiftyWith (defaultOptions { newtypeTag = True }) ''IsANewtype)

data ContainsANewtype = ContainsANewtype
  { fieldIsANewtype :: IsANewtype
  }
getShwifty ''ContainsANewtype

data HasRawValue = H1 | H2
$(getShwiftyWith (defaultOptions { dataRawValue = Just Str }) ''HasRawValue)

newtype AliasTest = AliastTest Int
$(getShwiftyWith (defaultOptions { typeAlias = True }) ''AliasTest)

-- newtype AliasTestArb a = AliasTestArb { getAliasTestArb :: Maybe a }
-- $(getShwiftyWith (defaultOptions { typeAlias = True }) ''AliasTestArb)

newtype AliasTestPoly a = AliasTestPoly Int
$(getShwiftyWith (defaultOptions { typeAlias = True }) ''AliasTestPoly)

newtype EnumTestTag = EnumTestTag Int

data EnumTest
  = Enum1
  | Enum2 { enumTestX :: Int }
$(getShwiftyWithTags defaultOptions ([ ''EnumTestTag ]) ''EnumTest)

class DataClass a where
  data family Key a

instance DataClass Int where
  newtype Key Int = IntKey { unIntKey :: Int }

data HasTags = HasTags { hasTagsX :: Int, hasTagsY :: Int }

$(getShwiftyWithTags defaultOptions ([ 'IntKey ]) ''HasTags)

type U = Data.UUID.Types.UUID

newtype TypeOneId = TypeOneId { getTypeOneId :: U }
newtype TypeTwoId = TypeTwoId { getTypeTwoId :: U }

data TypeOne = TypeOne
  { typeOneId :: TypeOneId
  , typeOneX :: Int
  }
$(getShwiftyWithTags defaultOptions ([ ''TypeOneId ]) ''TypeOne)

data TypeTwo = TypeTwo
  { typeTwoId :: TypeTwoId
  , typeTwoX :: Int
  }
$(getShwiftyWithTags defaultOptions ([ ''TypeTwoId ]) ''TypeTwo)

data TypeThree = TypeThree
  { typeThreeHasExternalTag :: TypeTwoId
  , typeThreeX :: Int
  }
getShwifty ''TypeThree

data CommonPrefixSum
  = CommonPrefixSum1
  | CommonPrefixSum2
$(getShwiftyWith (defaultOptions { constructorModifier = drop 12, dataProtocols = [Hashable], dataRawValue = Just Str }) ''CommonPrefixSum)

data CommonPrefix = CommonPrefix
  { commonPrefixA :: Int
  , commonPrefixB :: Int
  }
$(getShwiftyWith (defaultOptions { fieldLabelModifier = drop 12, dataProtocols = [Codable, Hashable, Equatable] }) ''CommonPrefix)

data TreeType a = TreeType
  { treeTypeField :: Either
      ( Either String
        ( Either String (Maybe a)
        )
      )
      ( Either String
        ( Either String (Maybe a)
        )
      )
  }
getShwifty ''TreeType

data Sum = Sum1 | Sum2 | Sum3 | Sum4
getShwifty ''Sum

newtype Endo a = Endo { appEndo :: a -> a }
getShwifty ''Endo

newtype N a = N { getN :: a }
getShwifty ''N

data K a = K { getK :: a, getInt :: Int }
getShwifty ''K

data M (a :: k) = MkM
getShwifty ''M

data OneTyVar a = OneTyVar
  { one :: Either (Maybe a) (Maybe a)
  , two :: Maybe (Maybe (Maybe (Maybe a)))
  }
getShwifty ''OneTyVar

data Z a b = Z { x :: Maybe a, b :: Maybe (Maybe b) }
getShwifty ''Z

data L a b = L
  { l0 :: Int
  , l1 :: (a,b,b)
  , l2 :: [a]
  , l3 :: [b]
  }
getShwifty ''L

data Foo a b (c :: k)
  = MkFoo1 Int a (Maybe b)
  | MkFoo2 b
  | MkFoo3 { intField1 :: Int, intField2 :: Int }
getShwifty ''Foo

data Contains a = Contains
  { m1 :: M Int
  , m2 :: M a
  , m3 :: Foo (a -> Int) a Int
  }
getShwifty ''Contains

test :: IO ()
test = do
  testPrint @(Contains X)
  testPrint @(Foo X X X)
  testPrint @(OneTyVar X)
  testPrint @(K X)
  testPrint @(Z X X)
  testPrint @(L X X)
  testPrint @(M X)
  testPrint @CommonPrefix
  testPrint @CommonPrefixSum
  testPrint @HasTags
  testPrint @TypeOne
  testPrint @TypeTwo
  testPrint @TypeThree
  testPrint @EnumTest
  testPrint @AliasTest
  testPrint @(AliasTestPoly X)
  testPrint @IsANewtype
  testPrint @ContainsANewtype
  testPrint @Laughs
  testPrint @(Fun X X)
  testPrint @(CodecTest X)
  testPrint @(CodecSum X X)
  testPrint @OmitFields0
  testPrint @OmitFields1
  testPrint @OmitCases0
  testPrint @OmitCases1
  --testPrint @(AliasTestArb X)

testPrint :: forall a. ToSwiftData a => IO ()
testPrint = putStrLn $ prettySwiftData $ toSwiftData (Proxy @a)

--data VoidTest
--getShwifty ''VoidTest

--data SingleConNonRecordTest
--  = SingleConNonRecordTest Int
--getShwifty ''SingleConNonRecordTest

--data InfixConTest = Int :+: Int
--getShwifty ''InfixConTest

--data KindVarRealisationTest (a :: Maybe k) = KindVarRealisationTest
--getShwifty ''KindVarRealisationTest

--data ExTypsTest = forall x y z. Ex x
--getShwifty ''ExTypsTest

