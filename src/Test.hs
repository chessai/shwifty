{-# language TemplateHaskell #-}
{-# language EmptyCase, GADTs, DataKinds, PolyKinds, KindSignatures,
   DuplicateRecordFields #-}

-- {-# options_ghc -ddump-splices #-}

module Test where

import Shwifty
import Data.Proxy

data M (a :: k) = MkM
getShwifty ''M

test :: IO ()
test = putStrLn
  $ prettySwiftData
  $ toSwiftData (Proxy :: Proxy (M Int))

--data M m a = MkM (m a)

data Void
--getShwifty ''Void

-- we cannot have existentially quantified type variables
data Ex = forall x. Ex x
--getShwifty ''Ex

data Foo a b (c :: k)
  = MkFoo1 Int a (Maybe b)
  | MkFoo2 b
  | MkFoo3 { x :: Int, y :: Int }
getShwifty ''Foo

{-
collect tyVars
  - how to handle datakinds/polykinds?
    e.g. consider
      `data Bar (a :: k) = MkBar`:
    we must instantiate k to `Type` when generating
    swift code, since there is no real notion of kinds
    there. But what if we had
      `data Baz (a :: Maybe k) = MkBaz`?
    We must reject this. anything other than
    the most free of kinds cannot be accepted.

    Thus, we accept:

      KindedTV a StarT

      and

      KindedTV b (VarT k)

      but not

      KindedTV c (AppT (ConT Maybe) (VarT k)

      and not

      KindedTV d (ConT Bool)

    We must also have no constructorVars. e.g. the
    following would be rejected due to having
    existentially quantified type variables:

      data Ex = forall x. Ex x
      data Ex1 = forall x. Ex (x -> x)

    As far as I know, swift has no GADTs or
    even existential type variables, so we should
    reject these.

    We do not differentiate between newtypes and
    datatypes. e.g.:


    `data Foo = MkFoo Int` will generate code
    equivalent to `newtype Foo = MkFoo Int`

    and

    `data Foo = MkFoo { getFoo :: Int }` will
    generate code equivalent to
    `newtype Foo = MkFoo { getFoo :: Int }`.

    The biggest restriction on the translation
    to swift types is that single-constructor
    products must have named
    fields. this is not a requirement on haskell
    types. my inclination thus far is to only accept
    record constructors, and not allow Infix or Normal.

    We cannot accept higher-kinded tyvars. e.g. we
    cannot accept
      `data HasMonad m a = MkHasMonad (m a)`
    because `m` has kind `Type -> Type`.
-}

--1. constructor:

--collect type variables


{-
loop through constructors

cases
0 constructors: error (equiv. to Haskell Void)
1 constructor: struct
n constructors: sum of products (enum of structs)

examples:

data Void -- would fail with compile-time error

data Foo = MkFoo Int -- would fail because field isn't named

data Foo = MkFoo { getFoo :: Int }
=
struct Foo {
  let getFoo: Int
}

data Barcode
  = Upc Int Int Int Int
  | QrCode String
=
enum Barcode {
  case upc(Int, Int, Int, Int)
  case qrCode(String)
}

data HasTyVars a b c
  = FieldA { labelA :: a }
  | FieldB { labelB :: b }
  | FieldC c -- no label
=
enum HasTyVars<A, B, C> {
  case fieldA(_ labelA: A)
  case fieldB(_ labelB: B)
  case fieldC(C)
}

-}

{-
    DatatypeInfo {
        datatypeContext = []
      , datatypeName = Test.Foo
      , datatypeVars =
          [ KindedTV a StarT
          , KindedTV b StarT
          , KindedTV c (VarT k)
          ]
      , datatypeInstTypes =
          [ SigT (VarT a) StarT
          , SigT (VarT b) StarT
          , SigT (VarT c) (VarT k)
          ]
      , datatypeVariant = Datatype
      , datatypeCons =
          [ ConstructorInfo {
              constructorName = Test.MkFoo1
            , constructorVars = []
            , constructorContext = []
            , constructorFields =
                [ ConT GHC.Types.Int
                , VarT a
                , AppT (ConT Maybe) (VarT b)
                ]
            , constructorStrictness =
                [ FieldStrictness {
                      fieldUnpackedness = UnspecifiedUnpackedness
                    , fieldStrictness = UnspecifiedStrictness
                  }
                ]
            , constructorVariant = NormalConstructor
            }
          , ConstructorInfo {
              constructorName = Test.MkFoo2
            , constructorVars = []
            , constructorContext = []
            , constructorFields =
                [ VarT b
                ]
            , constructorStrictness =
                [ FieldStrictness {
                    fieldUnpackedness = UnspecifiedUnpackedness
                  , fieldStrictness = UnspecifiedStrictness
                  }
                ]
            , constructorVariant = NormalConstructor
            }
            , ConstructorInfo {
                constructorName = Test.MkFoo3
              , constructorVars = []
              , constructorContext = []
              , constructorFields =
                  [ ConT Int
                  , ConT Int
                  ]
              , constructorStrictness =
                  [ FieldStrictness {
                      fieldUnpackedness = UnspecifiedUnpackedness
                    , fieldStrictness = UnspecifiedStrictness}
                    , FieldStrictness {
                        fieldUnpackedness = UnspecifiedUnpackedness
                      , fieldStrictness = UnspecifiedStrictness
                    }
                  ]
              , constructorVariant = RecordConstructor
                  [ Test.x
                  , Test.y
                  ]
            }
        ]
    }
-}

