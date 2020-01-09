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
  ) where

#include "MachDeps.h"

import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy (Proxy(..))
import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import Data.Functor.Identity (Identity(..))
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.Foldable (foldlM, foldr')
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

data Options = Options
  { optionalTruncate :: Bool
    -- ^ Whether or not to truncate Optional types.
    --   Normally, an Optional ('Maybe') is encoded as "Optional<A>",
    --   but in Swift it is valid to have "A?" (\'?\' appended to the
    --   type). The default ('False') is the verbose option.
  , indent :: Int
    -- ^ number of spaces to indent
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

{-
data StructTH = StructTH
  { name :: Name
  , tyVars :: [Name]
  , fields :: [(Name, Type)]
  }

data EnumTH = EnumTH
  { name :: Name
  , tyVars :: [Name]
  , cases :: [(Name, [(Maybe Name, Type)])]
  }
-}

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
      datatypeContext = ctx
    , datatypeName = parentName
    , datatypeInstTypes = instTys
    , datatypeVariant = variant
    , datatypeCons = cons
    } -> do {
       -- preliminary checks
    ; isDatatypeOrNewtype info
    ; mapM_ noConstructorVars cons
    ; mapM_ checkKind instTys
    ; noVoidTypes info

    ; pure []
    }

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

  pure undefined

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
    -- ^ actually a kind variable

canRealiseKindStar :: Type -> KindStatus
canRealiseKindStar = \case
  VarT{} -> KindStar
  SigT _ StarT -> KindStar
  SigT _ (VarT k) -> IsKindVar k
  _ -> NotKindStar

--data Struct = Struct
--  { name :: String
--  , tyVars :: [String]
--  , fields :: [(String,Ty)]
--  }


mkStruct :: ConstructorInfo -> [Name] -> Q [Dec]
mkStruct ConstructorInfo{..} names = do
  case constructorVariant of
    RecordConstructor fieldNames -> do
      let bndrs = zip fieldNames constructorFields
      pure []
    _ -> do
      fail "Single-constructor products must be records in order to be shwifty."

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

class ToSwiftData a where
  toSwiftData :: Proxy a -> SwiftData

{-
  case datatypeCons of
    [] -> fail $ "Cannot get schwifty with the void."
      ++ " You are trying to get schwifty with an empty type."
    [ConstructorInfo{..}] -> do
      case constructorVariant of
        NormalConstructor -> fail "NormalConstructor Not yet supported"
        RecordConstructor names -> do
          let fs = zip names constructorFields
          pure ()
    _ -> pure ()
-}

getTyVars :: DatatypeInfo -> [Name]
getTyVars DatatypeInfo{..} = id
  . mapMaybe getFreeTyVar
  $ datatypeInstTypes
  where
    getFreeTyVar = \case
      SigT (VarT name) _kind -> Just name
      _ -> Nothing

getFreeTyVars :: DatatypeInfo -> [String]
getFreeTyVars DatatypeInfo{..} = id
  . mapMaybe getFreeTyVar
  $ datatypeInstTypes
  where
    getFreeTyVar (SigT (VarT name) _kind) = Just (prepName name)
    getFreeTyVar _ = Nothing

prepName :: Name -> String
prepName = capFirstLetter . removeQualifiers . show
  where
    capFirstLetter [] = []
    capFirstLetter (c:cs) = Char.toUpper c : cs

getTyCon :: DatatypeInfo -> String
getTyCon DatatypeInfo{..} = id
  . removeQualifiers
  . show
  $ datatypeName

{-
let getFreeVarName (SigT (VarT name_) _kind) = Just name_
      getFreeVarName _ = Nothing
  let numTyVars = length datatypeVars
  let templateVars
        | numTyVars == 0 = []
        | numTyVars == 1 = [ConT ''T]
        | numTyVars > 10 = fail "More than 10 type variables not supported."
        | otherwise = take numTyVars $ [ConT ''T1, ConT ''T2, ConT ''T3, ConT ''T4, ConT ''T5, ConT ''T6, ConT ''T7, ConT ''T8, ConT ''T9, ConT ''T10]

  let subMap = M.fromList $ zip
        (catMaybes $ fmap getFreeVarName datatypeInstTypes)
        templateVars
  let fullyQualifiedDatatypeInfo = dti {
          datatypeInstTypes = templateVars
        , datatypeCons = fmap (applySubstitution subMap) datatypeCons
      }

  pure []
-}

stringE :: String -> Exp
stringE = LitE . StringL

removeQualifiers :: String -> String
removeQualifiers = TS.unpack . last . TS.splitOn "." . TS.pack

toSwiftTyE :: Type -> Exp
toSwiftTyE typ = AppE
  (VarE 'toSwiftTy)
  (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ))

{-
data Decl = DeclStruct Struct | DeclEnum Enum

data T   = T
data T1  = T1
data T2  = T2
data T3  = T3
data T4  = T4
data T5  = T5
data T6  = T6
data T7  = T7
data T8  = T8
data T9  = T9
data T10 = T10

getShwifty :: Name -> Q [Dec]
getShwifty name = do
  dti@DatatypeInfo{..} <- reifyDatatype name

  let getFreeVarName (SigT (VarT name_) _kind) = Just name_
      getFreeVarName _ = Nothing
  let numTyVars = length datatypeVars
  let templateVars
        | numTyVars == 0 = []
        | numTyVars == 1 = [ConT ''T]
        | numTyVars > 10 = fail "More than 10 type variables not supported."
        | otherwise = take numTyVars $ [ConT ''T1, ConT ''T2, ConT ''T3, ConT ''T4, ConT ''T5, ConT ''T6, ConT ''T7, ConT ''T8, ConT ''T9, ConT ''T10]

  let subMap = M.fromList $ zip
        (catMaybes $ fmap getFreeVarName datatypeInstTypes)
        templateVars
  let fullyQualifiedDatatypeInfo = dti {
          datatypeInstTypes = templateVars
        , datatypeCons = fmap (applySubstitution subMap) datatypeCons
      }

  pure []

getTypeExpression :: DatatypeInfo -> Q Exp
getTypeExpression DatatypeInfo{..} = case datatypeInstTypes of
  [] -> pure $ stringE $ getTypeName datatypeName
  vars -> do
    let baseName = stringE $ getTypeName datatypeName
    let typeNames = ListE [getTypeExp typ | typ <- vars]
    let headType = AppE (VarE 'head) typeNames
    let tailType = AppE (VarE 'tail) typeNames
    let comma = stringE ", "
    x <- newName "x"
    let tailsWithCommas = AppE (VarE 'mconcat) (CompE [BindS (VarP x) tailType, NoBindS (AppE (AppE (VarE 'mappend) comma) (VarE x))])
    let brackets = AppE (VarE 'mconcat) (ListE [stringE "<", headType, tailsWithCommas, stringE ">"])
    pure $ AppE (AppE (VarE 'mappend) baseName) brackets

getTypeName :: Name -> String
getTypeName = lastNameComponent . show

lastNameComponent :: String -> String
lastNameComponent = TS.unpack . last . TS.splitOn "." . TS.pack

allConstructorsNullary :: [ConstructorInfo] -> Bool
allConstructorsNullary = all isConstructorNullary

isConstructorNullary :: ConstructorInfo -> Bool
isConstructorNullary ConstructorInfo{..} =
     constructorVariant == NormalConstructor
  && constructorFields == []

getDatatypePredicate :: Type -> Pred
getDatatypePredicate typ = AppT (ConT ''Swift) typ

getTypeExp :: Type -> Exp
getTypeExp typ = AppE (VarE 'toSwiftTy) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) typ))

getTupleType :: [Type] -> Type
getTupleType [] = AppT ListT (ConT ''())
getTupleType (x:[]) = x
getTupleType conFields = apArgsT (ConT $ tupleTypeName $ length conFields) conFields

apArgsT :: Type -> [Type] -> Type
apArgsT con [] = con
apArgsT con (x:xs) = apArgsT (AppT con x) xs

apArgsE :: Exp -> [Exp] -> Exp
apArgsE f [] = f
apArgsE f (x:xs) = apArgsE (AppE f x) xs

stringE :: String -> Exp
stringE = LitE . StringL

mkInstance :: Cxt -> Type -> [Dec] -> Dec
mkInstance context typ decs = InstanceD Nothing context typ decs

assertExtensions :: DatatypeInfo -> Q ()
assertExtensions DatatypeInfo{..} = do
  unlessM (isExtEnabled ScopedTypeVariables) $ do
    fail "The ScopedTypeVariables extension is required to use Schwifty."
  unlessM (isExtEnabled KindSignatures) $ do
    fail "The KindSignatures extension is required to use Schwifty."

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb x = do { b <- mb; if b then pure () else x }
-}
