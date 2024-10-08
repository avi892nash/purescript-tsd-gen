{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.CodeGen.Tsd.Identifier
  ( isIdentifierName
  , IncludeKeywords(..)
  , Ident
  , identToText
  , identToBuilder
  , IdentifierName
  , Identifier
  , identToJs
  , properToJs
  , anyNameToJs
  , mapToIdent
  , ensureNonKeyword
  , toIdentifierName
  ) where
import           Data.Char (isAlphaNum, isLetter)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Language.PureScript.CodeGen.JS.Common as JSC
import qualified Language.PureScript.Names as PS

-- $
-- Behaviors of JSC.identToJs, JSC.properToJs, and JSC.anyNameToJs:
-- >>> JSC.identToJs (PS.Ident "foo")
-- "foo"
-- >>> JSC.identToJs (PS.Ident "foo'")
-- "foo$prime"
-- >>> JSC.identToJs (PS.Ident "for")
-- "$$for"
-- >>> JSC.properToJs (PS.ProperName "Foo")
-- "Foo"
-- >>> JSC.properToJs (PS.ProperName "Foo'")
-- "Foo$prime"
-- >>> JSC.anyNameToJs "foo"
-- "foo"
-- >>> JSC.anyNameToJs "foo'"
-- "foo$prime"
-- >>> JSC.anyNameToJs "for"
-- "$$for"

isIdentifierStart, isIdentifierPart :: Char -> Bool
isIdentifierStart c = isLetter c || c == '$' || c == '_' -- TODO: Match with "ID_Start"
isIdentifierPart c = isAlphaNum c || c == '$' || c == '\'' || c == '_' -- TODO: Match with "ID_Continue"

-- |
-- prop> all isIdentifierName ["foo", "x86", "PureScript", "$foobar", "__proto__"]
-- prop> not (any isIdentifierName ["", "foo'", "42"])
isIdentifierName :: T.Text -> Bool
isIdentifierName name = case T.uncons name of
                          Just (x0, xs) -> isIdentifierStart x0 && T.all isIdentifierPart xs
                          _ -> False

data IncludeKeywords = IncludeKeywords
                     | ExcludeKeywords


newtype Ident (k :: IncludeKeywords) = Ident T.Text
  deriving (Eq,Ord,Show)

mapToIdent :: (T.Text -> T.Text) -> Ident a -> Ident a
mapToIdent fn (Ident x) = Ident $ fn x

identToText :: Ident k -> T.Text
identToText (Ident name) = JSC.anyNameToJs name

identToBuilder :: Ident k -> TB.Builder
identToBuilder (Ident name) = TB.fromText name

type IdentifierName = Ident 'IncludeKeywords
type Identifier = Ident 'ExcludeKeywords

identToJs :: PS.Ident -> Ident 'ExcludeKeywords
identToJs = Ident . JSC.identToJs

properToJs :: PS.ProperName a -> Ident 'ExcludeKeywords
properToJs = Ident . JSC.properToJs

anyNameToJs :: T.Text -> Ident 'ExcludeKeywords
anyNameToJs = Ident . T.replace "$" "_" . JSC.anyNameToJs

-- |
-- >>> ensureNonKeyword (Ident "foo")
-- Just (Ident "foo")
-- >>> ensureNonKeyword (Ident "for")
-- Nothing
ensureNonKeyword :: Ident 'IncludeKeywords -> Maybe (Ident 'ExcludeKeywords)
ensureNonKeyword (Ident name) | JSC.nameIsJsReserved name = Nothing
                              | otherwise = Just (Ident name)


toIdentifierName :: Ident k -> Ident 'IncludeKeywords
toIdentifierName (Ident name) = Ident name
