{-# language
    AllowAmbiguousTypes
  , BangPatterns
  , CPP
  , DataKinds
  , DeriveFoldable
  , DeriveFunctor
  , DeriveTraversable
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
  , PolyKinds
  , RecordWildCards
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeApplications
  , TypeFamilies
  , ViewPatterns
  , QuantifiedConstraints
#-}

{-# options_ghc
  -Wall
  -Werror
#-}

module Shwifty
  ( getShwifty
  , Ty(..)
  , Swift(..)
  , prettyTy
  , SingSymbol(..)
  , Options(..)
  , X
  ) where

#include "MachDeps.h"

import Control.Monad.Except
import Data.Foldable (foldlM,foldr',foldl')
import Data.Functor ((<&>))
import Data.Int (Int8,Int16,Int32,Int64)
import Data.List (intercalate)
import Data.List.NonEmpty ((<|), NonEmpty(..))
import Data.Maybe (catMaybes)
import Data.Maybe (mapMaybe,fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Word (Word8,Word16,Word32,Word64)
import Data.Void (Void)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype
import Prelude hiding (Enum(..))
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

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
  | App Ty Ty
    -- ^ function type
  | I
    -- ^ signed machine integer
  | I8
    -- ^ signed 8-bit integer
  | I16
    -- ^ signed 16-bit integer
  | I32
    -- ^ signed 32-bit integer
  | I64
    -- ^ signed 64-bit integer
  | U
    -- ^ unsigned machine integer
  | U8
    -- ^ unsigned 8-bit integer
  | U16
    -- ^ unsigned 16-bit integer
  | U32
    -- ^ unsigned 32-bit integer
  | U64
    -- ^ unsigned 64-bit integer
  | F32
    -- ^ 32-bit floating point
  | F64
    -- ^ 64-bit floating point
  | Decimal
    -- ^ Increased-precision floating point
  | BigSInt32
    -- ^ 32-bit big integer
  | BigSInt64
    -- ^ 64-bit big integer
  | Poly String
    -- ^ polymorphic type variable
  | Struct
      { name :: String
      , tyVars :: [String]
      , fields :: [(String, Ty)]
      }
  | Enum
      { name :: String
      , tyVars :: [String]
      , cases :: [(String, [(Maybe String, Ty)])]
      }

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

class Swift a where
  toSwift :: Proxy a -> Ty

data SingSymbol (x :: Symbol) = SingSymbol
instance KnownSymbol x => Swift (SingSymbol x) where
  toSwift _ = Poly (symbolVal (Proxy @x))

type X = Void

instance Swift () where
  toSwift = const Unit

instance forall a b. (Swift a, Swift b) => Swift (a -> b) where
  toSwift = const (App (toSwift (Proxy @a)) (toSwift (Proxy @b)))

instance forall a. Swift a => Swift (Maybe a) where
  toSwift = const (Optional (toSwift (Proxy @a)))

-- | N.B. we flip the ordering because in Swift they are flipped
--   Should we though?
instance forall a b. (Swift a, Swift b) => Swift (Either a b) where
  toSwift = const (Result (toSwift (Proxy @b)) (toSwift (Proxy @a)))

instance Swift Integer where
  toSwift = const
#if WORD_SIZE_IN_BITS == 32
    BigSInt32
#else
    BigSInt64
#endif

instance Swift Int   where toSwift = const I
instance Swift Int8  where toSwift = const I8
instance Swift Int16 where toSwift = const I16
instance Swift Int32 where toSwift = const I32
instance Swift Int64 where toSwift = const I64

instance Swift Word   where toSwift = const U
instance Swift Word8  where toSwift = const U8
instance Swift Word16 where toSwift = const U16
instance Swift Word32 where toSwift = const U32
instance Swift Word64 where toSwift = const U64

instance Swift Float  where toSwift = const F32
instance Swift Double where toSwift = const F64

instance Swift Char where toSwift = const Character

instance {-# overlappable #-} forall a. Swift a => Swift [a] where
  toSwift = const (Array (toSwift (Proxy @a)))

instance {-# overlapping #-} Swift [Char] where toSwift = const Str
instance Swift TL.Text where toSwift = const Str
instance Swift TS.Text where toSwift = const Str

instance forall a b. (Swift a, Swift b) => Swift ((,) a b) where
  toSwift = const (Tuple2 (toSwift (Proxy @a)) (toSwift (Proxy @b)))

instance forall a b c. (Swift a, Swift b, Swift c) => Swift ((,,) a b c) where
  toSwift = const (Tuple3 (toSwift (Proxy @a)) (toSwift (Proxy @b)) (toSwift (Proxy @c)))

labelCase :: Maybe String -> Ty -> String
labelCase Nothing ty = prettyTy ty
labelCase (Just label) ty = "_ " ++ label ++ ": " ++ prettyTy ty

prettyTypeHeader :: String -> [String] -> String
prettyTypeHeader name [] = name
prettyTypeHeader name tyVars = name ++ "<" ++ intercalate ", " tyVars ++ ">"

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
  Array e -> "Array<" ++ prettyTy e ++ ">"
  App e1 e2 -> "((" ++ prettyTy e1 ++ ") -> " ++ prettyTy e2 ++ ")"
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
  Enum {name,tyVars,cases} -> []
    ++ "enum " ++ prettyTypeHeader name tyVars ++ " {\n"
    ++ go cases
    ++ "}"
    where
      go [] = ""
      go ((caseNm, cs):xs) = "    case " ++ caseNm ++ "(" ++ (intercalate ", " (map (uncurry labelCase) cs)) ++ ")\n" ++ go xs
  Struct {name,tyVars,fields} -> []
    ++ "struct " ++ prettyTypeHeader name tyVars ++ " {\n"
    ++ go fields
    ++ "}"
    where
      go [] = ""
      go ((fieldName,ty):fs) = "    let " ++ fieldName ++ ": " ++ prettyTy ty ++ "\n" ++ go fs

noConstructorVars :: ConstructorInfo -> Q ()
noConstructorVars ConstructorInfo{..} = do
  case constructorVars of
    [] -> pure ()
    _ -> fail "You cannot have existentially quantified type variables in a shwifty datatype."

ensureEnabled :: Extension -> Q ()
ensureEnabled ext = isExtEnabled ext >>= \case
  False -> fail $ show ext ++ " is not enabled."
    ++ " It is needed by shwifty to work."
  True -> pure ()

getShwifty :: Name -> Q [Dec]
getShwifty name = do
  ensureEnabled ScopedTypeVariables
  ensureEnabled DataKinds
  reifyDatatype name >>= \case
    DatatypeInfo
      { datatypeName = parentName
      , datatypeVars = tyVarBndrs
      , datatypeInstTypes = instTys
      , datatypeVariant = variant
      , datatypeCons = cons
      } -> do
        mapM_ noConstructorVars cons
        instanceHead <- buildTypeInstance parentName instTys tyVarBndrs variant
        inst <- instanceD
          (pure [])
          (pure instanceHead)
          (methodDecs parentName instTys cons)
        pure [inst]
  where
    methodDecs :: Name -> [Type] -> [ConstructorInfo] -> [Q Dec]
    methodDecs parentName instTys cons =
      [ funD 'toSwift
          [ clause
              []
              (normalB $ consToSwift parentName instTys cons)
              []
          ]
      ]

data ShwiftyError
  = VoidType
      { _conName :: Name
      }
  | SingleConNonRecord
      { _conName :: Name
      }
  | KindVariableCannotBeRealised
      { _kind :: Kind
      }
  | ExtensionNotEnabled
      { _ext :: Extension
      }
  | ExistentialTypes
      { _types :: [Type]
      }

type ShwiftyM = ExceptT ShwiftyError Q

consToSwift' :: ()
  => Name
     -- ^ name of type (used for Enums only)
  -> [Type]
     -- ^ type variables
  -> [ConstructorInfo]
     -- ^ constructors
  -> ShwiftyM Exp
consToSwift' parentName instTys = \case
  [] -> do
    throwError $ VoidType parentName
  cons -> do
    -- TODO: use '_' instead of matching
    value <- lift $ newName "value"
    lift $ lamE [varP value] (caseE (varE value) matches)
    where
      matches :: [Q Match]
      matches = case cons of
        [con] -> [mkProd instTys con]
        _ -> []

consToSwift :: ()
  => Name
  -> [Type]
  -> [ConstructorInfo]
  -> Q Exp
consToSwift parentName instTys = \case
  [] -> do
    fail "Cannot get shwifty with The Void!"
  cons -> do
    -- TODO: use '_' instead of matching.
    value <- newName "value"
    lamE [varP value] (caseE (varE value) matches)
    where
      matches :: [Q Match]
      matches = case cons of
        [con] ->
          [ mkProd instTys con
          ]
        _ -> do
          let tyVars = prettyTyVars instTys
          pure $ match
            (conP 'Proxy [])
            (normalB
               $ pure
               $ RecConE 'Enum
               $ [ (mkName "name", unqualName parentName)
                 , (mkName "tyVars", tyVars)
                 , (mkName "cases", ListE [ mkCase con | con <- cons])
                 ]
            )
            []

mkCaseHelper :: Name -> [Exp] -> Exp
mkCaseHelper name es = TupE [ caseName name, ListE es ]

mkCase :: ConstructorInfo -> Exp
mkCase = \case
  -- no fields
  ConstructorInfo
    { constructorVariant = NormalConstructor
    , constructorFields = []
    , constructorName = name
    , ..
    } -> mkCaseHelper name []
  -- has fields, non-record
  ConstructorInfo
    { constructorVariant = NormalConstructor
    , constructorName = name
    , constructorFields = fields
    , ..
    } -> mkCaseHelper name $ fields <&>
        (\typ -> TupE
          [ ConE 'Nothing
          , toSwiftE typ
          ]
        )
  -- infix constructor
  -- TODO: we need to be running in ExceptT ShwiftyError Q, then make this
  -- function return an Either
  ConstructorInfo
    { constructorVariant = InfixConstructor
    , ..
    } -> error "Infix Constructors not supported"
  -- records
  -- we turn names into labels
  ConstructorInfo
    { constructorVariant = RecordConstructor fieldNames
    , constructorName = name
    , constructorFields = fields
    , ..
    } ->
       let fieldsMap = zip fieldNames fields
       in mkCaseHelper name $ map caseField fieldsMap

caseField :: (Name, Type) -> Exp
caseField (n,typ) = TupE
  [ mkLabel n
  , toSwiftE typ
  ]

mkLabel :: Name -> Exp
mkLabel = AppE (ConE 'Just)
  . stringE
  . onHead Char.toLower
  . TS.unpack
  . last
  . TS.splitOn "."
  . TS.pack
  . show

mkProd :: [Type] -> ConstructorInfo -> Q Match
mkProd instTys = \case
  -- single constructor, no fields
  ConstructorInfo
    { constructorVariant = NormalConstructor
    , constructorFields = []
    , ..
    } -> do
      let tyVars = prettyTyVars instTys
      match
        (conP 'Proxy [])
        (normalB
          $ pure
          $ RecConE 'Struct
          $ [ (mkName "name", unqualName constructorName)
            , (mkName "tyVars", tyVars)
            , (mkName "fields", ListE [])
            ]
        )
        []
  -- single constructor, non-record (Normal)
  ConstructorInfo
    { constructorVariant = NormalConstructor
    } -> do
      fail "Cannot get Shwifty with single-constructor non-records"
  -- single constructor, non-record (Infix)
  ConstructorInfo
    { constructorVariant = InfixConstructor
    } -> do
      fail "Cannot get Shwifty with single-constructor non-records"
  -- single constructor, record
  ConstructorInfo
    { constructorVariant = RecordConstructor fieldNames
    , ..
    } -> do
      let tyVars = prettyTyVars instTys
      let fieldsMap = zip fieldNames constructorFields
      let fields = ListE $ map prettyField $ fieldsMap
      match
        (conP 'Proxy [])
        (normalB
          $ pure
          $ RecConE 'Struct
          $ [ (mkName "name", unqualName constructorName)
            , (mkName "tyVars", tyVars)
            , (mkName "fields", fields)
            ]
        )
        []

caseName :: Name -> Exp
caseName = stringE . onHead Char.toLower . TS.unpack . last . TS.splitOn "." . TS.pack . show

onHead :: (Char -> Char) -> String -> String
onHead f = \case
  [] -> []
  (x:xs) -> f x : xs

unqualName :: Name -> Exp
unqualName = stringE . TS.unpack . last . TS.splitOn "." . TS.pack . show

prettyTyVar :: Name -> Exp
prettyTyVar = stringE . map Char.toUpper . TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show

getTyVars :: [Type] -> [Name]
getTyVars = mapMaybe getFreeTyVar

getFreeTyVar :: Type -> Maybe Name
getFreeTyVar = \case
  VarT name -> Just name
  SigT (VarT name) _kind -> Just name
  _ -> Nothing

prettyTyVars :: [Type] -> Exp
prettyTyVars = ListE . map prettyTyVar . getTyVars

prettyField :: (Name, Type) -> Exp
prettyField (name, ty) = TupE
  [ unqualName name
  , toSwiftE ty
  ]

buildTypeInstance :: ()
  => Name
  -> [Type]
  -> [TyVarBndr]
  -> DatatypeVariant
  -> Q Type
buildTypeInstance tyConName varTysOrig tyVarBndrs variant = do
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

      _instanceCxt :: Cxt
      _instanceCxt = catMaybes preds

      instanceType :: Type
      instanceType = AppT (ConT ''Swift)
        $ applyTyCon tyConName varTysOrigSubst'

-- [TyVarBndrQ] -> CxtQ -> TypeQ -> TypeQ
  forallT
    (map tyVarBndrNoSig tyVarBndrs)
    (pure []) --instanceCxt)
    (pure instanceType)

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
  | hasKindStar t = (Just (applyCon ''Swift tName), [])
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

tyVarBndrNoSig :: TyVarBndr -> TyVarBndr
tyVarBndrNoSig = \case
  PlainTV n -> PlainTV n
  KindedTV n _k -> PlainTV n

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

stringE :: String -> Exp
stringE = LitE . StringL

toSwiftE :: Type -> Exp
toSwiftE = \case
  VarT n
    -> AppE (ConE 'Poly) (prettyTyVar n)
  SigT (VarT n) _
    -> AppE (ConE 'Poly) (prettyTyVar n)
  typ ->
    let decompressed = decompress typ
        prettyName = map Char.toUpper . TS.unpack . head . TS.splitOn "_" . last . TS.splitOn "." . TS.pack . show
        filledInHoles = decompressed <&>
          (\case
            VarT name -> AppT
              (ConT ''Shwifty.SingSymbol)
              (LitT (StrTyLit (prettyName name)))
            SigT (VarT name) _ -> AppT
              (ConT ''Shwifty.SingSymbol)
              (LitT (StrTyLit (prettyName name)))
            t -> t
          )
        typ' = compress filledInHoles
     in AppE
      (VarE 'toSwift)
      (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ'))

decompress :: Type -> Rose Type
decompress typ = case unapplyTy typ of
  tyCon :| tyArgs -> Rose tyCon (decompress <$> tyArgs)

compress :: Rose Type -> Type
compress (Rose typ []) = typ
compress (Rose t ts) = foldl' AppT t (compress <$> ts)

unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go = \case
      AppT t1 t2 -> t2 <| go t1
      SigT t _ -> go t
      ForallT _ _ t -> go t
      t -> t :| []

-- | Types can be 'stretched' out into a Rose tree.
--   'decompress' will stretch a type out completely,
--   in such a way that it cannot be stretched out
--   further. 'compress' will reconstruct a type from
--   its stretched form.
--
--   Also note that this is equivalent to
--   @'Cofree' 'NonEmpty' 'Type'@.
--
--   Examples:
--
--   Maybe a
--   =>
--   AppT (ConT Maybe) (VarT a)
--
--
--   Either a b
--   =>
--   AppT (AppT (ConT Either) (VarT a)) (VarT b)
--   =>
--   Rose (ConT Either)
--     [ Rose (VarT a)
--         [
--         ]
--     , Rose (VarT b)
--         [
--         ]
--     ]
--
--
--   Either (Maybe a) (Maybe b)
--   =>
--   AppT (AppT (ConT Either) (AppT (ConT Maybe) (VarT a))) (AppT (ConT Maybe) (VarT b))
--   =>
--   Rose (ConT Either)
--     [ Rose (ConT Maybe)
--         [ Rose (VarT a)
--             [
--             ]
--         ]
--     , Rose (ConT Maybe)
--         [ Rose (VarT a)
--             [
--             ]
--         ]
--     ]
data Rose a = Rose a [Rose a]
  deriving stock (Eq, Show)
  deriving stock (Functor,Foldable,Traversable)
