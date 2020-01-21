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

{-# options_ghc -ddump-splices #-}

module Test where

import Shwifty
import Data.Proxy
import Data.Kind (Type)
import Data.Void (Void)

class DataClass a where
  data family Key a

instance DataClass Int where
  newtype Key Int = IntKey { unIntKey :: Int }

instance DataClass (Maybe a) where
  newtype Key (Maybe a) = MaybeKey { unMaybeKey :: Maybe a }

instance DataClass Bool where
  data Key Bool
    = BoolKey1 Bool
    | BoolKey2 Bool

--getShwifty 'BoolKey1
--getShwifty 'MaybeKey
getShwifty 'IntKey

data CommonPrefixSum
  = CommonPrefixSum1
  | CommonPrefixSum2
$(getShwiftyWith (defaultOptions { fieldLabelModifier = drop 12, dataProtocols = [Hashable], dataRawValue = Just String }) ''CommonPrefixSum)

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

testPrint :: forall a. ToSwiftData a => IO ()
testPrint = putStrLn $ prettySwiftData $ toSwiftData (Proxy @a)

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

