{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Language.PureScript.TsdGen.Module
  ( module Language.PureScript.TsdGen.Module
  , ModuleProcessingError (..)
  , readExternsForModule
  ) where
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Bifunctor
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import           Data.Version (showVersion)
import qualified Language.PureScript.CodeGen.Tsd.Identifier as JS
import           Language.PureScript.CodeGen.Tsd.Types
import qualified Language.PureScript.Constants.Compat as C
import           Language.PureScript.Environment
import           Language.PureScript.Environment.Compat
import           Language.PureScript.Errors
import           Language.PureScript.Externs.Compat
import           Language.PureScript.Label
import           Language.PureScript.Names
import           Language.PureScript.Pretty.Compat (prettyPrintKind)
import           Language.PureScript.PSString
import           Language.PureScript.TsdGen.Hardwired
import           Language.PureScript.TsdGen.Module.ReadExterns (ModuleProcessingError (..),
                                                                readExternsForModule)
import           Language.PureScript.TsdGen.Types
import           Language.PureScript.Types
import           Paths_purescript_tsd_gen (version)
import           Prelude hiding (elem, lookup, notElem)
import           Data.Char (toLower)
import Language.PureScript.CodeGen.Tsd.Identifier (mapToIdent)
newtype ModuleImport = ModuleImport { moduleImportIdent :: Maybe JS.Identifier
                                    }

type ModuleImportMap = Map.Map ModuleName ModuleImport

type RenamedExportMap = Map.Map {- external name -} JS.IdentifierName
                               ({- internal name: manged by anyNameToJs -} JS.Identifier, {- comments -} [T.Text])

type ModuleWriter = RWST () TB.Builder (ModuleImportMap, RenamedExportMap) (ExceptT ModuleProcessingError IO)

recursivelyLoadExterns :: FilePath -> ModuleName -> StateT (Environment, Map.Map ModuleName (Maybe ExternsFile)) (ExceptT ModuleProcessingError IO) ()
recursivelyLoadExterns dir moduleName
  | moduleName == C.Prim = return () -- ~v0.11.7
  | moduleName `List.elem` C.primModules = return () -- v0.12.0~
  | otherwise = do
  ef <- lift (readExternsForModule dir moduleName)
  modify (second (Map.insert moduleName (Just ef)))
  let imports = efImports ef
  forM_ (map eiModule imports) $ \importModuleName -> do
    alreadyLoading <- gets (Map.member importModuleName . snd)
    unless alreadyLoading $ do
      modify (second (Map.insert importModuleName Nothing))
      recursivelyLoadExterns dir importModuleName
  modify (first (applyExternsFileToEnvironment ef))

emitComment :: Text -> ModuleWriter ()
emitComment t = tell ("// " <> TB.fromText t <> "\n")

-- emitInterface :: JS.Identifier -> [Text] -> [Field] -> ModuleWriter ()
-- emitInterface name tyParams fields = do
--   let tyParamsText | null tyParams = mempty
--                    | otherwise = "<" <> TB.fromText (T.intercalate ", " $ map ("'" <>) tyParams) <> ">"
--   tell $ "interface " <> JS.identToBuilder name <> tyParamsText <> " {\n" <> mconcat (map (\f -> "    " <> showField f <> ",\n") fields) <> "}\n"

data ExportName = NeedsRenaming { exportedName :: JS.IdentifierName, internalName :: JS.Identifier }
                | NoRenaming JS.Identifier
psNameToJSExportName :: Text -> ExportName
psNameToJSExportName psName
  = let internalName = JS.anyNameToJs psName
        identifierName = JS.toIdentifierName internalName
    in if JS.identToText internalName == JS.identToText identifierName
      then NoRenaming internalName
      else NeedsRenaming { exportedName = identifierName
                                        , internalName
                                        }

emitRenamedExport :: Maybe Text -> JS.IdentifierName -> JS.Identifier -> ModuleWriter ()
emitRenamedExport comment externalName internalName = do
  minfo <- gets (Map.lookup externalName . snd)
  case minfo of
    Just (internalName', comments)
      | internalName' == internalName -> modify $ second $ Map.insert externalName (internalName', maybeToList comment ++ comments)
      | otherwise -> fail "renamed export: internalName mismatch"
    Nothing -> modify $ second $ Map.insert externalName (internalName, maybeToList comment)

emitTypeDeclaration :: Maybe Text -> ExportName -> [Text] -> Maybe TSType -> ModuleWriter ()
emitTypeDeclaration comment ename tyParams mbty = do
  let commentPart = case comment of
                 Just commentText -> "/*" <> TB.fromText commentText <> "*/ "
                 Nothing          -> mempty
  let tyParamsText | null tyParams = mempty
                   | otherwise = "<" <> TB.fromText (T.intercalate ", " $ map ("'" <>) tyParams) <> ">"
  case mbty of
    Just ty -> 
      case ename of
        NoRenaming name -> do
          tell $ "type " <> commentPart <> JS.identToBuilder name <> tyParamsText <> " = " <> showTSType ty <> "\n"
        NeedsRenaming { exportedName, internalName } -> do
          tell $ "type " <> commentPart <> JS.identToBuilder internalName <> tyParamsText <> " = " <> showTSType ty <> "\n"
          emitRenamedExport (Just "type") exportedName internalName
    Nothing -> 
      case ename of
        NoRenaming name -> do
          tell $ "type " <> commentPart <> JS.identToBuilder name <> tyParamsText <> "\n"
        NeedsRenaming { exportedName, internalName } -> do
          tell $ "type " <> commentPart <> JS.identToBuilder internalName <> tyParamsText <> "\n"
          emitRenamedExport (Just "type") exportedName internalName

emitValueDeclaration :: Maybe Text -> ExportName -> TSType -> ModuleWriter ()
emitValueDeclaration comment vname ty = case vname of
  NeedsRenaming { exportedName, internalName } -> do
    tell $ "declare type " <> commentPart <> JS.identToBuilder internalName <> ": " <> showTSType ty <> "\n"
    emitRenamedExport (Just "value") exportedName internalName
  NoRenaming name -> do
    tell $ "@module(\"./index.js\") external " <> commentPart <> JS.identToBuilder (mapToIdent lowerFirstLetter name) <> ": " <> showTSType ty <> " = \"" <> JS.identToBuilder name <>"\"\n"
  where commentPart = case comment of
                        Just commentText -> "/*" <> TB.fromText commentText <> "*/ "
                        Nothing -> mempty

processLoadedModule :: Environment -> ExternsFile -> Bool -> ExceptT ModuleProcessingError IO TB.Builder
processLoadedModule env ef importAll = execWriterT $ do
  tell $ "// module " <> TB.fromText (runModuleName currentModuleName) <> ", generated by purescript-tsd-gen " <> TB.fromString (showVersion version) <> "\n"
  ((moduleImportMap, renamedExportMap), moduleBody) <-
    lift $ execRWST (mapM_ processDecl (efDeclarations ef)) -- action
                    () -- reader context
                    (Map.singleton currentModuleName (ModuleImport { moduleImportIdent = Nothing }), Map.empty) -- initial state

  tell moduleBody

  -- Renamed exports
  -- forM_ (Map.toList renamedExportMap) $ \(externalName, (internalName, comments)) -> do
  --   let commentPart = case comments of
  --                       [] -> mempty
  --                       _:_ -> "/*" <> mconcat (List.intersperse "+" $ map TB.fromText $ reverse comments) <> "*/ "
  --   tell $ "export " <> commentPart <> "{ " <> JS.identToBuilder internalName <> " as " <> JS.identToBuilder externalName <> " }\n"

  -- TODO: module re-exports: dig efExports / ReExportRef

  where
    currentModuleName :: ModuleName
    currentModuleName = efModuleName ef

    qualCurrentModule :: a -> Qualified a
#if MIN_VERSION_purescript(0, 15, 3)
    qualCurrentModule = Qualified (ByModuleName currentModuleName)
#else
    qualCurrentModule = Qualified (Just currentModuleName)
#endif

    -- Get the JS identifier for given module
    getModuleId :: ModuleName -> ModuleWriter (Maybe JS.Identifier)
    getModuleId C.Prim = return Nothing -- should not occur
    getModuleId moduleName = do
      mid <- gets (Map.lookup moduleName . fst)
      case mid of
        Nothing -> do -- not found
          let moduleId = Just $ JS.anyNameToJs $ T.replace "." "_" (runModuleName moduleName)
          -- TODO: Make sure moduleId is unique
          modify (first $ Map.insert moduleName (ModuleImport { moduleImportIdent = moduleId }))
          return moduleId
        Just ModuleImport{..} -> return moduleImportIdent

    makeContext :: [Text] -> TypeTranslationContext ModuleWriter
    makeContext typeVariables = TypeTranslationContext typeVariables [] Nothing getModuleId env currentModuleName

    pursTypeToTSTypeX :: Bool -> [Text] -> SourceType -> ModuleWriter TSType
    pursTypeToTSTypeX toLowerFirstChar ctx ty = do
      e <- runExceptT $ runReaderT (pursTypeToTSType toLowerFirstChar ty) (makeContext ctx)
      case e of
        Left err   -> throwError (PursTypeError currentModuleName err)
        Right tsty -> return tsty

    processDecl :: ExternsDeclaration -> ModuleWriter ()
    processDecl EDType{..} = do
      let name = edTypeName
          qTypeName = qualCurrentModule edTypeName
      if isSimpleKind edTypeKind
        then case edTypeDeclarationKind of
               -- newtype declaration:
               DataType _dataDeclType (stripRole -> params) [(ctorPName,[member])]
                 | Just (Newtype,_,_,_) <- Map.lookup (qualCurrentModule ctorPName) (dataConstructors env) -> do
                     case extractTypes edTypeKind params of
                       Just typeParameters -> do
                         member' <- pursTypeToTSTypeX False typeParameters member
                         emitTypeDeclaration (Just "newtype") (psNameToJSExportName (lowerFirstLetter $ runProperName name)) typeParameters $ Just member'
                       Nothing -> do
                         emitComment $ "newtype " <> runProperName name <> ": kind annotation was not available"

               -- data declaration:
               DataType _dataDeclType (stripRole -> params) ctors -> do
                 case extractTypes edTypeKind params of
                   Just typeParameters -> do
                     emitTypeDeclaration (Just "data type") (psNameToJSExportName (lowerFirstLetter $ runProperName name)) typeParameters Nothing
                   Nothing -> do
                     emitComment $ "data " <> runProperName name <> ": kind annotation was not available"

               -- type synonym:
               TypeSynonym
                 | Just (synonymArguments, synonymType) <- Map.lookup qTypeName (typeSynonyms env) -> do
                     case extractTypes edTypeKind synonymArguments of
                       Just typeParameters -> do
                         tsty <- pursTypeToTSTypeX True typeParameters synonymType
                         emitTypeDeclaration (Just "synonym") (psNameToJSExportName (lowerFirstLetter $ runProperName name)) typeParameters $ Just tsty
                       Nothing -> do
                         emitComment $ "type synonym " <> runProperName name <> ": kind annotation was not available"
                 | otherwise -> emitComment ("type (synonym) " <> runProperName name <> ": " <> prettyPrintKind edTypeKind)

               -- foreign import data:
               ExternData {}
                 | qTypeName == qnUnit -> do
                     -- Data.Unit
                     emitTypeDeclaration (Just "builtin") (psNameToJSExportName "Unit") [] (Just $ TSRecord [mkOptionalField "$$pursType" (TSStringLit "Data.Unit.Unit")])
                 | qTypeName `List.elem` builtins -> do
                     pst <- pursTypeToTSTypeX False typeParameters (foldl (TypeApp nullSourceAnn) (TypeConstructor nullSourceAnn qTypeName) (map (TypeVar nullSourceAnn) typeParameters))
                     emitTypeDeclaration (Just "builtin") (psNameToJSExportName (runProperName name)) typeParameters $ Just pst
                 | otherwise -> do
                     -- Foreign type: just use 'any' type.
                     -- External '.d.ts' file needs to be supplied for better typing.
                     emitTypeDeclaration (Just "foreign") (psNameToJSExportName (lowerFirstLetter $ runProperName name)) typeParameters Nothing
                 where builtins = [qnFn0,qnFn2,qnFn3,qnFn4,qnFn5,qnFn6,qnFn7,qnFn8,qnFn9,qnFn10
                                  ,qnEffect,qnEffectFn1,qnEffectFn2,qnEffectFn3,qnEffectFn4,qnEffectFn5,qnEffectFn6,qnEffectFn7,qnEffectFn8,qnEffectFn9,qnEffectFn10
                                  ,qnStrMap,qnForeignObject,qnNullable]
                       n = numberOfTypeParams edTypeKind
                       typeParameters = map (\i -> "a" <> T.pack (show i)) [0..n-1]

               -- others:
               LocalTypeVariable -> emitComment ("unexpected local type variable: " <> runProperName name <> " :: " <> prettyPrintKind edTypeKind)
               ScopedTypeVar -> emitComment ("unexpected scoped type variable: " <> runProperName name <> " :: " <> prettyPrintKind edTypeKind)

        else emitComment ("type " <> runProperName name <> " :: " <> T.strip (prettyPrintKind edTypeKind) <> " : unsupported kind")

    processDecl EDDataConstructor{..} = do
      let name = edDataCtorName
      case Map.lookup (qualCurrentModule edDataCtorTypeCtor) (types env) of
        Just (k, DataType _dataDeclType (stripRole -> typeParameters) constructors)
          | isSimpleKind k
          , Just fieldTypes <- List.lookup edDataCtorName constructors -> do
              tsty <- pursTypeToTSTypeX True [] edDataCtorType
              case edDataCtorOrigin of
                Data -> do
                  let ctorFieldName | null edDataCtorFields = "value"
                                    | otherwise = "create"
                      ctorType = TSRecord [ mkField ctorFieldName tsty
                                          ]
                  emitValueDeclaration (Just "data ctor") (psNameToJSExportName (runProperName name)) ctorType

                Newtype ->
                  emitComment $ "Not implementing this constructor : " <> runProperName name <> " \n"

        Nothing -> emitComment $ "the type of an exported data constructor must be exported: " <> runProperName name
        Just (k, DataType {}) -> emitComment $ "unrecognized data constructor: " <> runProperName name <> " kind: " <> prettyPrintKind k
        _ -> emitComment $ "unrecognized data constructor: " <> runProperName name

    processDecl EDValue{..} = do
      let name = edValueName
      tsty <- pursTypeToTSTypeX True [] edValueType
      emitValueDeclaration (Just "Declaration Value ") (psNameToJSExportName (runIdent name)) tsty

    processDecl EDInstance{..}
      | Just constraints <- edInstanceConstraints
#if MIN_VERSION_purescript(0, 15, 3)
      , Just typeClassDict <- Map.lookup (ByModuleName currentModuleName) (typeClassDictionaries env)
#else
      , Just typeClassDict <- Map.lookup (Just currentModuleName) (typeClassDictionaries env)
#endif
      , Just _ <- Map.lookup edInstanceClassName typeClassDict = do
          -- TODO: This code depends on the undocumented implementation-details...
          let {-synonymInstance = replaceAllTypeVars (zip (freeTypeVariables synonymType) edInstanceTypes) synonymType-}
              dictTy = foldl srcTypeApp (srcTypeConstructor qDictTypeName) edInstanceTypes
              desugaredInstanceType = quantify (foldr srcConstrainedType dictTy constraints)
          instanceTy <- pursTypeToTSTypeX False [] desugaredInstanceType
          emitValueDeclaration (Just "instance") (psNameToJSExportName (runIdent edInstanceName)) instanceTy
      | otherwise = emitComment ("invalid instance declaration '" <> runIdent edInstanceName <> "'")
      where -- name = identToJs edInstanceName :: JS.Identifier
            qDictTypeName = fmap (coerceProperName . dictTypeName) edInstanceClassName :: Qualified (ProperName 'TypeName)

    processDecl EDKind { edKindName = kindName } = do
      -- Do nothing for kind declarations: just put a comment.
      let name = runProperName kindName
      emitComment ("kind " <> name)

    processDecl EDTypeSynonym{} = do
      -- Ignored: should be handled in EDType case.
      return ()

    processDecl EDClass{} = do
      -- Ignored: should be handled in EDType case.
      return ()
