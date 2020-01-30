{-# language
    AllowAmbiguousTypes
  , TemplateHaskell
  , ScopedTypeVariables
  , DataKinds
  , KindSignatures
  , PolyKinds
  , GADTs
  , TypeApplications
  , DuplicateRecordFields
  , TypeFamilies
  , QuantifiedConstraints
  , FlexibleInstances
#-}

--{-# options_ghc -ddump-splices #-}

module Test where

import Shwifty
import Data.Proxy
import Data.Kind (Type)
import Data.Void (Void)
import qualified Data.UUID.Types

newtype EnumTestTag = EnumTestTag Int

data EnumTest
  = Enum1
  | Enum2 { x :: Int }
$(getShwiftyWithTags defaultOptions ([ ''EnumTestTag ]) ''EnumTest)

class DataClass a where
  data family Key a

instance DataClass Int where
  newtype Key Int = IntKey { unIntKey :: Int }

data HasTags = HasTags { x :: Int, y :: Int }

$(getShwiftyWithTags defaultOptions ([ 'IntKey ]) ''HasTags)

type U = Data.UUID.Types.UUID

newtype TypeOneId = TypeOneId { getTypeOneId :: U }
newtype TypeTwoId = TypeTwoId { getTypeTwoId :: U }

data TypeOne = TypeOne
  { id :: TypeOneId
  , x :: Int
  }
$(getShwiftyWithTags defaultOptions ([ ''TypeOneId ]) ''TypeOne)

data TypeTwo = TypeTwo
  { id :: TypeTwoId
  , x :: Int
  }
$(getShwiftyWithTags defaultOptions ([ ''TypeTwoId ]) ''TypeTwo)

data TypeThree = TypeThree
  { hasExternalTag :: TypeTwoId
  , x :: Int
  }
getShwifty ''TypeThree

data CommonPrefixSum
  = CommonPrefixSum1
  | CommonPrefixSum2
$(getShwiftyWith (defaultOptions { fieldLabelModifier = drop 12, dataProtocols = [Hashable], dataRawValue = Just Str }) ''CommonPrefixSum)

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

testPrint :: forall a. ToSwiftData a => IO ()
testPrint = putStrLn $ prettySwiftData $ Proxy @a

--data VoidTest
--getShwifty ''VoidTest

--data SingleConNonRecordTest
--  = SingleConNonRecordTest Int
--getShwifty ''SingleConNonRecordTest

--data InfixConTest = Int :+: Int
--getShwifty ''InfixConTest

--data KindVarRealisationTest (a :: Maybe k) = K
--getShwifty ''KindVarRealisationTest

--data ExTypsTest = forall x y z. Ex x
--getShwifty ''ExTypsTest

