{-# language
    LambdaCase
  , RecordWildCards
  #-}

module Shwifty.Pretty
  ( prettySwiftData
  , prettyTy
  ) where

import Data.List (intercalate)

import Shwifty.Types

prettySwiftData :: SwiftData -> String
prettySwiftData = prettySwiftDataWith 4

-- | Pretty-print a 'SwiftData'.
--   This function cares about indent.
prettySwiftDataWith :: ()
  => Int -- ^ indent
  -> SwiftData
  -> String
prettySwiftDataWith indent = \case

  SwiftEnum {..} -> []
    ++ "enum "
    ++ prettyTypeHeader enumName enumTyVars
    ++ prettyRawValueAndProtocols enumRawValue enumProtocols
    ++ " {"
    ++ newlineNonEmpty enumCases
    ++ go enumCases
    ++ prettyTags indents enumTags
    ++ newlineNonEmpty enumTags
    ++ "}"
    where
      go [] = ""
      go ((caseNm, []):xs) = []
        ++ indents
        ++ "case "
        ++ caseNm
        ++ "\n"
        ++ go xs
      go ((caseNm, cs):xs) = []
        ++ indents
        ++ "case "
        ++ caseNm
        ++ "("
        ++ (intercalate ", " (map (uncurry labelCase) cs))
        ++ ")\n"
        ++ go xs

  SwiftStruct {..} -> []
    ++ "struct "
    ++ prettyTypeHeader structName structTyVars
    ++ prettyProtocols structProtocols
    ++ " {"
    ++ newlineNonEmpty structFields
    ++ go structFields
    ++ prettyTags indents structTags
    ++ newlineNonEmpty structTags
    ++ "}"
    where
      go [] = ""
      go ((fieldName,ty):fs) = indents ++ "let " ++ fieldName ++ ": " ++ prettyTy ty ++ "\n" ++ go fs

  SwiftAlias{..} -> []
    ++ "typealias "
    ++ prettyTypeHeader aliasName aliasTyVars
    ++ " = "
    ++ prettyTy aliasTyp
  where
    indents = replicate indent ' '

    newlineNonEmpty [] = ""
    newlineNonEmpty _ = "\n"

prettyTypeHeader :: String -> [String] -> String
prettyTypeHeader name [] = name
prettyTypeHeader name tyVars = name ++ "<" ++ intercalate ", " tyVars ++ ">"

prettyRawValueAndProtocols :: Maybe Ty -> [Protocol] -> String
prettyRawValueAndProtocols Nothing ps = prettyProtocols ps
prettyRawValueAndProtocols (Just ty) [] = ": " ++ prettyTy ty
prettyRawValueAndProtocols (Just ty) ps = ": " ++ prettyTy ty ++ ", " ++ intercalate ", " (map show ps)

prettyProtocols :: [Protocol] -> String
prettyProtocols = \case
  [] -> ""
  ps -> ": " ++ intercalate ", " (map show ps)

prettyTags :: String -> [Ty] -> String
prettyTags indents = go where
  go [] = ""
  go (Tag{..}:ts) = []
    ++ "\n"
    ++ prettyTagDisambiguator tagDisambiguate indents tagName
    ++ indents
    ++ "typealias "
    ++ tagName
    ++ " = Tagged<"
    ++ (if tagDisambiguate then tagName ++ "Tag" else tagParent)
    ++ ", "
    ++ prettyTy tagTyp
    ++ ">"
    ++ go ts
  go _ = error "non-tag supplied to prettyTags"

prettyTagDisambiguator :: ()
  => Bool
     -- ^ disambiguate?
  -> String
     -- ^ indents
  -> String
     -- ^ parent type name
  -> String
prettyTagDisambiguator disambiguate indents parent
  = if disambiguate
      then []
        ++ indents
        ++ "enum "
        ++ parent
        ++ "Tag { }\n"
      else ""

labelCase :: Maybe String -> Ty -> String
labelCase Nothing ty = prettyTy ty
labelCase (Just label) ty = "_ " ++ label ++ ": " ++ prettyTy ty

-- | Pretty-print a 'Ty'.
prettyTy :: Ty -> String
prettyTy = \case
  Str -> "String"
  Unit -> "()"
  Bool -> "Bool"
  Character -> "Character"
  Tuple2 e1 e2 -> "(" ++ prettyTy e1 ++ ", " ++ prettyTy e2 ++ ")"
  Tuple3 e1 e2 e3 -> "(" ++ prettyTy e1 ++ ", " ++ prettyTy e2 ++ ", " ++ prettyTy e3 ++ ")"
  Optional e -> prettyTy e ++ "?"
  Result e1 e2 -> "Result<" ++ prettyTy e1 ++ ", " ++ prettyTy e2 ++ ">"
  Set e -> "Set<" ++ prettyTy e ++ ">"
  Dictionary e1 e2 -> "Dictionary<" ++ prettyTy e1 ++ ", " ++ prettyTy e2 ++ ">"
  Array e -> "[" ++ prettyTy e ++ "]"
  -- App is special, we recurse until we no longer
  -- any applications.
  App e1 e2 -> prettyApp e1 e2
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
  Concrete ty [] -> ty
  Concrete ty tys -> ty
    ++ "<"
    ++ intercalate ", " (map prettyTy tys)
    ++ ">"
  Tag {..} -> tagParent ++ "." ++ tagName

prettyApp :: Ty -> Ty -> String
prettyApp t1 t2 = "(("
  ++ intercalate ", " (map prettyTy as)
  ++ ") -> "
  ++ prettyTy r
  ++ ")"
  where
    (as, r) = go t1 t2
    go e1 (App e2 e3) = case go e2 e3 of
      (args, ret) -> (e1 : args, ret)
    go e1 e2 = ([e1], e2)
