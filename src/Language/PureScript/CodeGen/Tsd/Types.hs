{-# LANGUAGE OverloadedStrings #-}
module Language.PureScript.CodeGen.Tsd.Types
  ( showTSType
  , showParenIf
  , showField
  , showFunctionParameters
  , objectPropertyToString
  , showTSTypePrec
  ) where
import           Data.List (intersperse)
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder.Int as TB
import           Language.PureScript.CodeGen.JS.Common
import qualified Language.PureScript.CodeGen.Tsd.Identifier as JS (identToBuilder)
import           Language.PureScript.Label
import           Language.PureScript.PSString
import           Language.PureScript.TsdGen.Types
import           Data.Char

intercalateTB :: TB.Builder -> [TB.Builder] -> TB.Builder
intercalateTB sep xs = mconcat (intersperse sep xs)

showTSType :: TSType -> TB.Builder
showTSType = showTSTypePrec 0

showParenIf :: Bool -> TB.Builder -> TB.Builder
showParenIf True s  = "(" <> s <> ")"
showParenIf False s = s

showField :: Field -> TB.Builder
showField field@Field{} = objectPropertyToString (runLabel (fieldLabel field)) <> optionalMarker <> ": " <> showTSType (fieldType field)
  where optionalMarker | fieldIsOptional field = "?"
                       | otherwise = mempty

showFunctionParameters :: [TSType] -> TB.Builder
showFunctionParameters [] = ""
showFunctionParameters [ty] = showTSType ty
showFunctionParameters types = intercalateTB ", " $ map showTSType types

-- |
-- >>> objectPropertyToString "hello"
-- "hello"
-- >>> objectPropertyToString "foo'"
-- "\"foo'\""
-- >>> objectPropertyToString "0"
-- "\"0\""
-- >>> objectPropertyToString "for"
-- "\"for\""
objectPropertyToString :: PSString -> TB.Builder
objectPropertyToString ps = case decodeString ps of
                              Just t | isValidJsIdentifier t -> TB.fromText $ T.singleton '"' <> t <> T.singleton '"'
                              _ -> TB.fromText (prettyPrintStringJS ps)

showTypeName :: TSTypeName -> TB.Builder
showTypeName (QualifiedTypeName moduleId name) = JS.identToBuilder moduleId <> "." <> JS.identToBuilder name
showTypeName (UnqualifiedTypeName name) = JS.identToBuilder name

showTSTypePrec :: Int -> TSType -> TB.Builder
showTSTypePrec prec ty = case ty of
  TSAny -> "any"
  TSUndefined -> "undefined"
  TSNull -> "null"
  TSNever -> "never"
  TSNumber -> "number"
  TSBoolean -> "boolean"
  TSString -> "string"
  RSOption elemTy -> "option< " <> showTSType elemTy <> ">"
  TSFunction [] params ret -> showParenIf (prec > 0) $ "(" <> showFunctionParameters params <> ") => " <> showTSType ret
  TSFunction tp params ret -> showParenIf (prec > 0) $ "<" <> intercalateTB ", " (map (TB.fromText . anyNameToJs) tp) <> ">(" <> showFunctionParameters params <> ") => " <> showTSType ret
  TSArray elemTy -> "array< " <> showTSType elemTy <> " >" -- TODO: Use ReadonlyArray?
  TSStrMap elemTy -> "Js.Dict.t <" <> showTSType elemTy <> ">"
  TSRecord [] -> "{}"
  TSRecord fields -> "{ " <> intercalateTB ", " (map showField fields) <> " }"
  TSUnknown desc -> "unknown /* " <> TB.fromText desc <> " */"
  TSStringLit s -> TB.fromText (prettyPrintStringJS s)
  TSUnion [] -> "never" -- uninhabitated type
  TSUnion members -> showParenIf (prec > 1) $ intercalateTB " | " (map (showTSTypePrec 1) members)
  TSIntersection [] -> "{}" -- universal type.  TODO: use 'unknown' type?
  TSIntersection members -> intercalateTB " & " (map (showTSTypePrec 2) members)
  TSTyVar name -> TB.fromText name
  TSNamed name tyArgs -> showTypeName name <> ta
    where ta = case tyArgs of
                 [] -> mempty
                      -- the space after '<' is needed to avoid parse error with types like Array<<a>(_: a) => a>
                 _ -> "< " <> intercalateTB ", " (map showTSType tyArgs) <> " >"
  TSCommented inner desc -> showTSTypePrec prec inner <> " /* " <> TB.fromText desc <> " */"
