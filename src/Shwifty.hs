{-# language
    AllowAmbiguousTypes
  , BangPatterns
  , CPP
  , DataKinds
  , DerivingStrategies
  , DuplicateRecordFields
  , FlexibleContexts
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , NamedFieldPuns
  , OverloadedStrings
  , RecordWildCards
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , TypeFamilies
#-}

{-# options_ghc
  -Wall
  -fno-warn-unused-imports
  -fno-warn-unused-top-binds
#-}

module Shwifty
  ( getShwifty
  , SwiftData(..)
  , Struct(..)
  , Enum(..)
  , ToSwiftData(..)
  , SwiftTy(..)
  , prettyStruct
  , prettyEnum
  , prettySwiftData
  ) where

#include "MachDeps.h"

import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy (Proxy(..))
import Control.Monad (forM)
import Data.Maybe (mapMaybe,fromMaybe)
import Data.Functor.Identity (Identity(..))
import Control.Lens hiding (cons)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.Foldable
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Vector (Vector)
import Data.Vector.Mutable (MVector)
import Data.Word
import GHC.Generics hiding (datatypeName)
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype
import Prelude hiding (Enum(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Char as Char

data Ty
  = Unit
    -- ^ Unit (Unit/Void in swift). Empty struct type.
  | Character
    -- ^ Character
  | Str
    -- ^ String
  | Tuple2 Ty Ty
    -- ^ 2-tuple
  | Tuple3 Ty Ty Ty
    -- ^ 3-tuple
  | Optional Ty
    -- ^ Maybe type
  | Result Ty Ty
    -- ^ Either type
  | Dictionary Ty Ty
    -- ^ Dictionary type
  | Array Ty
    -- ^ array type
  -- numbers
  | I | I8 | I16 | I32 | I64 -- signed integers
  | U | U8 | U16 | U32 | U64 -- unsigned integers
  | F32 | F64 | Decimal -- floating point numbers
  | BigSInt32 | BigSInt64 -- big integers
  | Poly String -- polymorphic type variable

-- | Options that specify how to
--   encode your 'SwiftData' to a swift type.
--
--   Options can be set using record syntax on
--   'defaultOptions' with the fields below.
data Options = Options
  { fieldLabelModifier :: String -> String
    -- ^ Function applied to field labels.
    --   Handy for removing common record prefixes,
    --   for example. The default ('id') makes no
    --   changes.
  , optionalTruncate :: Bool
    -- ^ Whether or not to truncate Optional types.
    --   Normally, an Optional ('Maybe') is encoded as "Optional<A>",
    --   but in Swift it is valid to have "A?" (\'?\' appended to the
    --   type). The default ('False') is the verbose option.
  , indent :: Int
    -- ^ Number of spaces to indent field names
    --   and cases. The default is 2.
  }

class SwiftTy a where
  toSwiftTy :: Proxy a -> Ty

instance SwiftTy () where
  toSwiftTy = const Unit

instance forall a. SwiftTy a => SwiftTy (Maybe a) where
  toSwiftTy = const (Optional (toSwiftTy (Proxy @a)))

-- | N.B. we flip the ordering because in Swift they are flipped
--   Should we though?
instance forall a b. (SwiftTy a, SwiftTy b) => SwiftTy (Either a b) where
  toSwiftTy = const (Result (toSwiftTy (Proxy @b)) (toSwiftTy (Proxy @a)))

instance SwiftTy Integer where
  toSwiftTy = const
#if WORD_SIZE_IN_BITS == 32
    BigSInt32
#else
    BigSInt64
#endif

instance SwiftTy Int   where toSwiftTy = const I
instance SwiftTy Int8  where toSwiftTy = const I8
instance SwiftTy Int16 where toSwiftTy = const I16
instance SwiftTy Int32 where toSwiftTy = const I32
instance SwiftTy Int64 where toSwiftTy = const I64

instance SwiftTy Word   where toSwiftTy = const U
instance SwiftTy Word8  where toSwiftTy = const U8
instance SwiftTy Word16 where toSwiftTy = const U16
instance SwiftTy Word32 where toSwiftTy = const U32
instance SwiftTy Word64 where toSwiftTy = const U64

instance SwiftTy Float  where toSwiftTy = const F32
instance SwiftTy Double where toSwiftTy = const F64

instance SwiftTy Char where toSwiftTy = const Character

instance {-# overlappable #-} forall a. SwiftTy a => SwiftTy [a] where
  toSwiftTy = const (Array (toSwiftTy (Proxy @a)))

instance {-# overlapping #-} SwiftTy [Char] where toSwiftTy = const Str
instance SwiftTy TL.Text where toSwiftTy = const Str
instance SwiftTy TS.Text where toSwiftTy = const Str

instance forall a b. (SwiftTy a, SwiftTy b) => SwiftTy ((,) a b) where
  toSwiftTy = const (Tuple2 (toSwiftTy (Proxy @a)) (toSwiftTy (Proxy @b)))

instance forall a b c. (SwiftTy a, SwiftTy b, SwiftTy c) => SwiftTy ((,,) a b c) where
  toSwiftTy = const (Tuple3 (toSwiftTy (Proxy @a)) (toSwiftTy (Proxy @b)) (toSwiftTy (Proxy @c)))

data SwiftData = SwiftStruct Struct | SwiftEnum Enum

prettySwiftData :: SwiftData -> String
prettySwiftData = \case
  SwiftStruct s -> prettyStruct s
  SwiftEnum e -> prettyEnum e

data Struct = Struct
  { name :: String
  , tyVars :: [String]
  , fields :: [(String,Ty)]
  }

data Enum = Enum
  { name :: String
  , tyVars :: [String]
  , cases :: [(String, [(Maybe String, Ty)])]
  }

testEnums :: [Enum]
testEnums =
  [ Enum {
      name = "Barcode"
    , tyVars = []
    , cases =
        [ ("upc", [(Nothing, I),(Nothing, I),(Nothing, I),(Nothing, I)])
        , ("qrCode", [(Nothing, Str)])
        ]
    }
  , Enum {
      name = "HasLabels"
    , tyVars = []
    , cases =
        [ ("field", [(Just "fieldLabel", Str)])
        ]
    }
  , Enum {
      name = "HasTyVars"
    , tyVars = ["A", "B", "C"]
    , cases =
        [ ("fieldA", [(Just "labelA", Poly "A")])
        , ("fieldB", [(Just "labelB", Poly "B")])
        , ("fieldC", [(Just "labelC", Poly "C")])
        ]
    }
  ]

prettyEnum :: Enum -> String
prettyEnum Enum{name,tyVars,cases} = []
  ++ "enum " ++ prettyTypeHeader name tyVars ++ " {\n"
  ++ go cases
  ++ "}"
  where
    go [] = ""
    go ((caseName, cs):xs) = "    case " ++ caseName ++ "(" ++ (intercalate ", " (map (uncurry labelCase) cs)) ++ ")\n" ++ go xs

labelCase :: Maybe String -> Ty -> String
labelCase Nothing ty = prettyTy ty
labelCase (Just label) ty = "_ " ++ label ++ ": " ++ prettyTy ty

testStructs :: [Struct]
testStructs =
  [ Struct {
      name = "Crazy",
      tyVars = [],
      fields = [
        ("crazyField1", Tuple2 (Optional (Optional Str)) (Array I64)),
        ("crazyField2", Optional (Optional (Result Str (Result I U))))
      ]
  },
    Struct {
      name = "Person",
      tyVars = [],
      fields = [
        ("name", Str),
        ("age", I),
        ("numOfChildren", Optional I),
        ("moneyInBankAccount", Decimal)
      ]
  }
  ]

prettyTypeHeader :: String -> [String] -> String
prettyTypeHeader name [] = name
prettyTypeHeader name tyVars = name ++ "<" ++ intercalate ", " tyVars ++ ">"

prettyStruct :: Struct -> String
prettyStruct Struct{name,tyVars,fields} = []
  ++ "struct " ++ prettyTypeHeader name tyVars ++ " {\n"
  ++ go fields
  ++ "}"
  where
    go [] = ""
    go ((fieldName,ty):fs) = "    let " ++ fieldName ++ ": " ++ prettyTy ty ++ "\n" ++ go fs

prettyTy :: Ty -> String
prettyTy = \case
  Str -> "String"
  Unit -> "()"
  Character -> "Character"
  Tuple2 e1 e2 -> "(" ++ prettyTy e1 ++ ", " ++ prettyTy e2 ++ ")"
  Tuple3 e1 e2 e3 -> "(" ++ prettyTy e1 ++ ", " ++ prettyTy e2 ++ ", " ++ prettyTy e3 ++ ")"
  Optional e -> "Optional<" ++ prettyTy e ++ ">"
  Result e1 e2 -> "Result<" ++ prettyTy e1 ++ ", " ++ prettyTy e2 ++ ">"
  Dictionary e1 e2 -> "Dictionary<" ++ prettyTy e1 ++ ", " ++ prettyTy e2 ++ ">"
  Array e -> "Optional<" ++ prettyTy e ++ ">"
  I -> "Int"
  I8 -> "Int8"
  I16 -> "Int16"
  I32 -> "Int32"
  I64 -> "Int64"
  U -> "UInt"
  U8 -> "UInt8"
  U16 -> "UInt16"
  U32 -> "UInt32"
  U64 -> "UInt64"
  F32 -> "Float"
  F64 -> "Double"
  Decimal -> "Decimal"
  BigSInt32 -> "BigSInt32"
  BigSInt64 -> "BigSInt64"
  Poly ty -> ty

noConstructorVars :: ConstructorInfo -> Q ()
noConstructorVars ConstructorInfo{..} = do
  case constructorVars of
    [] -> pure ()
    _ -> fail "You cannot have existentially quantified type variables in a shwifty datatype."

isDatatypeOrNewtype :: DatatypeInfo -> Q ()
isDatatypeOrNewtype DatatypeInfo{..} = do
  case datatypeVariant of
    Datatype -> pure ()
    Newtype -> pure ()
    _ -> fail "Data Family instances are not supported"

noVoidTypes :: DatatypeInfo -> Q ()
noVoidTypes DatatypeInfo{..} = do
  case datatypeCons of
    [] -> fail "You cannot get shwifty with the void."
    _ -> pure ()

-- TODO: should we should only reject
-- type variables with bad kinds if they
-- are non-phantom?
checkKind :: Type -> Q ()
checkKind = \case
  SigT (VarT _a) (VarT _k) -> pure ()
  SigT (VarT _a) StarT -> pure ()
  _ -> fail "Found type variable with wrong kind."

type Shwifty
  = ReaderT DatatypeInfo (ExceptT ShwiftyError Q)

data ShwiftyError
  = WrongKind
  | VoidType

getShwifty :: Name -> Q [Dec]
getShwifty name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo {
      datatypeName = parentName
    , datatypeInstTypes = instTys
    , datatypeVariant = variant
    , datatypeCons = cons
    } -> do {
       -- preliminary checks
    -- ; isDatatypeOrNewtype info
    ; mapM_ noConstructorVars cons
    -- ; mapM_ checkKind instTys
    -- ; noVoidTypes info
    ; (instanceCtx, instanceType)
        <- buildTypeInstance parentName instTys variant
    ; (:[]) <$> instanceD
        (pure instanceCtx)
        (pure instanceType)
        (methodDecs parentName instTys cons)
    }
  where
    methodDecs :: Name -> [Type] -> [ConstructorInfo] -> [Q Dec]
    methodDecs parentName instTys cons = (:[])
      $ funD 'toSwiftData
          [ clause
              []
              (normalB $ consToSwiftData instTys cons)
              []
          ]

consToSwiftData :: ()
  => [Type]
  -> [ConstructorInfo]
  -> Q Exp
consToSwiftData _ [] = fail "Cannot get shwifty with The Void!"
consToSwiftData instTys cons = do
  value <- newName "value"
  --let tvMap = M.fromList $ zip instTys
  lamE [varP value] (caseE (varE value) matches)
  where
    matches :: [Q Match]
    matches = case cons of
      [con] -> [argsToValue False con instTys]
      _ -> []

unqualName :: Name -> Exp
unqualName = stringE . TS.unpack . last . TS.splitOn "." . TS.pack . show

prettyTyVar :: Name -> Exp
prettyTyVar = stringE . map Char.toUpper . TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show

getTyVars :: [Type] -> [Name]
getTyVars = mapMaybe getFreeTyVar
  where
    getFreeTyVar = \case
      SigT (VarT name) _kind -> Just name
      _ -> Nothing

argsToValue :: Bool -> ConstructorInfo -> [Type] -> Q Match
-- single constructor, no fields
argsToValue False (ConstructorInfo { constructorVariant = NormalConstructor, constructorFields = [], .. }) instTys = do
  let sd x = AppE (ConE 'SwiftStruct)
        (AppE (ConE 'Struct) x)
  let tyVars = ListE $ map prettyTyVar $ getTyVars instTys
  match (conP 'Proxy [])
        (normalB
          $ pure
          $ AppE (ConE 'SwiftStruct)
          $ RecConE 'Struct
          $ [ (mkName "name", unqualName constructorName)
            , (mkName "tyVars", tyVars)
            , (mkName "fields", ListE [])
            ]
        )
        [] --fail "TODO: empty structs"
-- single constructor, non-record
argsToValue False (ConstructorInfo { constructorVariant = NormalConstructor }) instTys = do
  fail "CANNOT GET SHWIFTY WITH SINGLE-CONSTRUCTOR NON-RECORDS!!!!!!"
-- single constructor, record
argsToValue False ConstructorInfo{..} instTys = do
  fail "TODO: records"
--argsToValue True ConstructorInfo{..} = do

  --argTys <- mapM resolveTypeSynonyms constructorFields
  --let len = length argTys
  --args <- newNameList "arg" len

  --ConstructorInfo {
  --  constructorName = conName
  --, constructorVariant = Norma
 -- }
--data Struct = Struct
--  { name :: String
--  , tyVars :: [String]
--  , fields :: [(String,Ty)]
--  }


{-
data ConstructorInfo
  { constructorName :: Name
      -- ^ Constructor name
  , constructorVars :: [TyVarBndr]
      -- ^ Constructor type parameters
  , constructorContext :: Cxt
      -- ^ Constructor constraints
  ,  constructorFields :: [Type]
      -- ^ Constructor fields
  , constructorStrictness :: [FieldStrictness]
      -- ^ Constructor fields' strictness (Invariant: has the same length as constructorFields)
  , constructorVariant :: ConstructorVariant
      -- ^ Extra information
-}

buildTypeInstance :: ()
  => Name
  -> [Type]
  -> DatatypeVariant
  -> Q (Cxt, Type)
buildTypeInstance tyConName varTysOrig variant = do
  -- Make sure to expand through type/kind synonyms!
  -- Otherwise, the eta-reduction check might get
  -- tripped up over type variables in a synonym
  -- that are actually dropped.
  -- (See GHC Trac #11416 for a scenario where this
  -- actually happened)
  varTysExp <- mapM resolveTypeSynonyms varTysOrig

  starKindStats :: [KindStatus] <- foldlM
    (\stats ty -> case canRealiseKindStar ty of
      NotKindStar -> do
        fail "encountered kind variable that cannot be realised to kind *"
      s -> pure (stats ++ [s])
    ) [] varTysExp

  let kindVarNames :: [Name]
      kindVarNames = flip mapMaybe starKindStats
        (\case
            IsKindVar n -> Just n
            _ -> Nothing
        )

  -- instantiate polykinded things to star
  let varTysExpSubst :: [Type]
      varTysExpSubst = map (substNamesWithKindStar kindVarNames) varTysExp

  let preds :: [Maybe Pred]
      kvNames :: [[Name]]
      kvNames' :: [Name]
      -- Derive instance constraints (and any
      -- kind variables which are specialised to
      -- * in those constraints)
      (preds, kvNames) = unzip $ map deriveConstraint varTysExpSubst
      kvNames' = concat kvNames

      --varTysExpSubst' :: [Type]
      --varTysExpSubst' = map (substNamesWithKindStar kvNames') varTysExpSubst

      -- We now sub all of the specialised-to-* kind
      -- variable names with *, but in the original types,
      -- not the synonym-expanded types. The reason we
      -- do this is superficial: we want the derived
      -- instance to resemble the datatype written in
      -- source code as closely as possible. For example,
      --
      --   data family Fam a
      --   newtype instance Fam String = Fam String
      --
      -- We'd want to generate the instance:
      --
      --   instance C (Fam String)
      --
      -- Not:
      --
      --   instance C (Fam [Char])
      varTysOrigSubst :: [Type]
      varTysOrigSubst =
        map (substNamesWithKindStar (kindVarNames `L.union` kvNames')) $ varTysOrig

      isDataFamily :: Bool
      isDataFamily = case variant of
        Datatype -> False
        Newtype -> False
        DataInstance -> True
        NewtypeInstance -> True

      varTysOrigSubst' :: [Type]
      varTysOrigSubst' = if isDataFamily
        then varTysOrigSubst
        else map unSigT varTysOrigSubst

      instanceCxt :: Cxt
      instanceCxt = catMaybes preds

      instanceType :: Type
      instanceType = AppT (ConT ''ToSwiftData)
        $ applyTyCon tyConName varTysOrigSubst'

  pure (instanceCxt, instanceType)

-- peel off a kind signature from a Type, if it has one
unSigT :: Type -> Type
unSigT = \case
  SigT t _ -> t
  t -> t

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (`substNameWithKind` starK) t ns
  where
    substNameWithKind :: Name -> Kind -> Type -> Type
    substNameWithKind n k = applySubstitution (M.singleton n k)

data KindStatus
  = KindStar
    -- ^ kind * (or some k which can be realised to *)
  | NotKindStar
    -- ^ any other kind
  | IsKindVar Name
    -- ^ is actually a kind variable

canRealiseKindStar :: Type -> KindStatus
canRealiseKindStar = \case
  VarT{} -> KindStar
  SigT _ StarT -> KindStar
  SigT _ (VarT k) -> IsKindVar k
  _ -> NotKindStar

hasKindStar :: Type -> Bool
hasKindStar = \case
  VarT {} -> True
  SigT _ StarT -> True
  _ -> False

-- TODO: type variables with no bearing on runtime rep
-- should not get constraints. note that
-- 'no bearing on runtime rep' is different than
-- 'phantom', since a user could give a stronger
-- RoleAnnotation to a type variable but it would
-- still have no bearing on runtime rep.
deriveConstraint :: Type -> (Maybe Pred, [Name])
deriveConstraint t
  | not (isTyVar t) = (Nothing, [])
  | hasKindStar t = (Just (applyCon ''SwiftTy tName), [])
  | otherwise = (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

-- is the given type a type variable?
isTyVar :: Type -> Bool
isTyVar = \case
  VarT _ -> True
  SigT t _ -> isTyVar t
  _ -> False

applyCon :: Name -> Name -> Pred
applyCon con t = AppT (ConT con) (VarT t)

-- fully applies a type constructor to its
-- type variables
applyTyCon :: Name -> [Type] -> Type
applyTyCon = foldl' AppT . ConT

varTToNameMaybe :: Type -> Maybe Name
varTToNameMaybe = \case
  VarT n -> Just n
  SigT t _ -> varTToNameMaybe t
  _ -> Nothing

varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!")
  . varTToNameMaybe

class ToSwiftData a where
  toSwiftData :: Proxy a -> SwiftData

stringE :: String -> Exp
stringE = LitE . StringL

removeQualifiers :: String -> String
removeQualifiers = TS.unpack . last . TS.splitOn "." . TS.pack

toSwiftTyE :: Type -> Exp
toSwiftTyE typ = AppE
  (VarE 'toSwiftTy)
  (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ))
