{-# language TemplateHaskell #-}
{-# language ScopedTypeVariables, DataKinds #-}
{-# language KindSignatures, PolyKinds, GADTs #-}
{-# language TypeApplications #-}
-- {-# language EmptyCase, GADTs, DataKinds, PolyKinds, KindSignatures,
--   ScopedTypeVariables, DuplicateRecordFields, TypeApplications #-}

{-# options_ghc -ddump-splices #-}

module Test where

import Shwifty
import Data.Proxy
import Data.Kind (Type)
import Data.Void (Void)

data M (a :: k) = MkM
getShwifty ''M

data Contains a = Contains
  { field :: M a
  }
getShwifty ''Contains

data OneTyVar a = OneTyVar
  { one :: Either (Maybe a) (Maybe a)
  , two :: Maybe (Maybe (Maybe (Maybe a)))
  }
getShwifty ''OneTyVar

data K a = K { getK :: a, getInt :: Int }
getShwifty ''K

data Z a b = Z { x :: Maybe a, b :: Maybe (Maybe b) }
getShwifty ''Z

data L a b = L
  { l0 :: Int
  , l1 :: (a,b)
  , l2 :: [a]
  , l3 :: [b]
  }
getShwifty ''L

data Foo a b (c :: k)
  = MkFoo1 Int a (Maybe b)
  | MkFoo2 b
  | MkFoo3 { intField1 :: Int, intField2 :: Int }
getShwifty ''Foo

test :: IO ()
test = do
  let testPrint :: ToSwiftData a => Proxy a -> IO ()
      testPrint = putStrLn . prettySwiftData . toSwiftData
  pure ()
  testPrint $ Proxy @(Foo X X X)
  testPrint $ Proxy @(OneTyVar X)
  testPrint $ Proxy @(K X)
  testPrint $ Proxy @(Z X X)
  testPrint $ Proxy @(L X X)

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

