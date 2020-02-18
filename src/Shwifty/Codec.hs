{-# language
    AllowAmbiguousTypes
  , DataKinds
  , ExplicitNamespaces
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , PolyKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
  #-}

module Shwifty.Codec
  ( Codec(..)
  , ModifyOptions(..)
  , AsIs
  , type (&)
  , Label(..)
  , Drop
  , GenerateClass
  , DontGenerate
  , Implement
  , RawValue
  , CanBeRawValue
  , TypeAlias
  , NewtypeTag
  , DontLowercase
  , OmitField
  , OmitCase
  , MakeBase
  ) where

import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import GHC.TypeLits
  ( KnownSymbol, Symbol, symbolVal
  , TypeError, ErrorMessage(..)
  )
import Shwifty.Class
import Shwifty.Types

-- | Modify options.
class ModifyOptions tag where
  modifyOptions :: Options -> Options

-- | No modifications
type AsIs = ()

instance ModifyOptions AsIs where
  modifyOptions = id

-- | A carrier for modifiers.
data Codec tag = Codec

instance ModifyOptions tag => ModifyOptions (Codec tag) where
  modifyOptions = modifyOptions @tag

infixr 6 &
-- | Combine modifications.
data a & b

instance forall a b. (ModifyOptions a, ModifyOptions b) => ModifyOptions (a & b) where
  modifyOptions = modifyOptions @a . modifyOptions @b

-- | Label modifiers.
data Label
  = TyCon
    -- ^ Type constructor modifier
  | DataCon
    -- ^ Data constructor modifiers
  | Field
    -- ^ Field label modifiers

-- | Modify a label by dropping a string
data Drop (label :: Label) (string :: Symbol)

instance KnownSymbol string => ModifyOptions (Drop 'TyCon string) where
  modifyOptions options = options
    { typeConstructorModifier = drop (length (symbolVal (Proxy @string)))
    }

instance KnownSymbol string => ModifyOptions (Drop 'DataCon string) where
  modifyOptions options = options
    { constructorModifier = drop (length (symbolVal (Proxy @string)))
    }

instance KnownSymbol string => ModifyOptions (Drop 'Field string) where
  modifyOptions options = options
    { fieldLabelModifier = drop (length (symbolVal (Proxy @string)))
    }

-- | Don't generate a specific class.
data DontGenerate (cls :: * -> Constraint)

class GenerateClass (c :: * -> Constraint) where
  classModifier :: Options -> Options

instance GenerateClass ToSwiftData where
  classModifier options = options { generateToSwiftData = False }

instance GenerateClass ToSwift where
  classModifier options = options { generateToSwift = False }

instance GenerateClass c => ModifyOptions (DontGenerate c) where
  modifyOptions = classModifier @c

-- | Add protocols
data Implement (protocol :: Protocol)

class KnownProtocol (p :: Protocol) where protocolVal :: Protocol
instance KnownProtocol 'Equatable where protocolVal = Equatable
instance KnownProtocol 'Hashable where protocolVal = Hashable
instance KnownProtocol 'Codable where protocolVal = Codable

instance ModifyOptions (Implement 'Equatable) where
  modifyOptions options = options { dataProtocols = Equatable : dataProtocols options }

instance ModifyOptions (Implement 'Hashable) where
  modifyOptions options = options { dataProtocols = Hashable : dataProtocols options }

instance ModifyOptions (Implement 'Codable) where
  modifyOptions options = options { dataProtocols = Codable : dataProtocols options }

-- | Add a rawValue
data RawValue (ty :: Ty)

class CanBeRawValue (ty :: Ty) where
  getRawValue :: Ty

instance CanBeRawValue 'Str where getRawValue = Str
instance CanBeRawValue 'I where getRawValue = I
instance CanBeRawValue 'I8 where getRawValue = I8
instance CanBeRawValue 'I16 where getRawValue = I16
instance CanBeRawValue 'I32 where getRawValue = I32
instance CanBeRawValue 'I64 where getRawValue = I64
instance CanBeRawValue 'U where getRawValue = U
instance CanBeRawValue 'U8 where getRawValue = U8
instance CanBeRawValue 'U16 where getRawValue = U16
instance CanBeRawValue 'U32 where getRawValue = U32
instance CanBeRawValue 'U64 where getRawValue = U64

instance CanBeRawValue ty => ModifyOptions (RawValue ty) where
  modifyOptions options = options { dataRawValue = Just (getRawValue @ty) }

-- | Make it a type alias (only applies to newtypes)
data TypeAlias

instance ModifyOptions TypeAlias where
  modifyOptions options = options { typeAlias = True }

-- | Make it a newtype tag (only applies to newtype tags)
data NewtypeTag

instance ModifyOptions NewtypeTag where
  modifyOptions options = options { newtypeTag = True }

-- | Dont lower-case fields/cases
data DontLowercase (someKind :: Label)

instance TypeError ('Text "Cannot apply DontLowercase to TyCon") => ModifyOptions (DontLowercase 'TyCon) where
  modifyOptions _ = error "UNREACHABLE"

instance ModifyOptions (DontLowercase 'DataCon) where
  modifyOptions options = options { lowerFirstCase = False }

instance ModifyOptions (DontLowercase 'Field) where
  modifyOptions options = options { lowerFirstField = False }

-- | Omit a field
data OmitField (field :: Symbol)

instance KnownSymbol field => ModifyOptions (OmitField field) where
  modifyOptions options = options { omitFields = symbolVal (Proxy @field) : omitFields options }

-- | Omit a case
data OmitCase (cas :: Symbol)

instance KnownSymbol cas => ModifyOptions (OmitCase cas) where
  modifyOptions options = options { omitCases = symbolVal (Proxy @cas) : omitCases options }

-- | Make a base type
data MakeBase (rawValue :: Maybe Ty) (protocols :: [Protocol])

instance forall ty protocols. (CanBeRawValue ty, ProtocolList protocols) => ModifyOptions (MakeBase ('Just ty) protocols) where
  modifyOptions options = options
    { makeBase = (,,) True (Just (getRawValue @ty)) (protocolList @protocols)
    }

instance forall protocols. (ProtocolList protocols) => ModifyOptions (MakeBase 'Nothing protocols) where
  modifyOptions options = options
    { makeBase = (,,) True Nothing (protocolList @protocols)
    }

data SomeProtocol where
  SomeProtocol :: KnownProtocol p => SomeProtocol

class ProtocolList (x :: [Protocol]) where
  protocolList :: [Protocol]

instance ProtocolList '[] where
  protocolList = []

instance forall p ps. (KnownProtocol p, ProtocolList ps) => ProtocolList (p ': ps) where
  protocolList = protocolVal @p : protocolList @ps
