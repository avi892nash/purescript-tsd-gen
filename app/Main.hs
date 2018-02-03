{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where
import Prelude hiding (elem,notElem,lookup)
import qualified Data.List as List
import Language.PureScript.Externs
import Language.PureScript.Environment
import Language.PureScript.Types
import Language.PureScript.Names
import Language.PureScript.Label
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Text (Text)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer hiding ((<>))
import Control.Monad.RWS.Strict hiding ((<>))
import qualified Data.Map as Map
import Language.PureScript.CodeGen.JS.Common
import Language.PureScript.PSString
import Language.PureScript.Pretty.Kinds
import Language.PureScript.CodeGen.TS
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe

type ModuleWriter = RWST () TB.Builder (Map.Map ModuleName (Maybe Text)) IO

emitComment :: Text -> ModuleWriter ()
emitComment t = tell ("// " <> TB.fromText t <> "\n")

emitInterface :: Text -> [Text] -> [Field] -> ModuleWriter ()
emitInterface name tyParams fields = do
  let tyParamsText | null tyParams = mempty
                   | otherwise = "<" <> TB.fromText (T.intercalate ", " $ map properToJs tyParams) <> ">"
  tell $ "interface " <> TB.fromText name <> tyParamsText <> " {\n" <> TB.fromText (T.concat (map (\f -> "    " <> showField f <> ";\n") fields)) <> "}\n"

emitTypeDeclaration :: Maybe Text -> Text -> [Text] -> TSType -> ModuleWriter ()
emitTypeDeclaration comment name tyParams ty = do
  let commentPart = case comment of
                 Just commentText -> "/*" <> TB.fromText commentText <> "*/ "
                 Nothing -> mempty
  let tyParamsText | null tyParams = mempty
                   | otherwise = "<" <> TB.fromText (T.intercalate ", " $ map properToJs tyParams) <> ">"
  tell $ "export type " <> commentPart <> TB.fromText name <> tyParamsText <> " = " <> TB.fromText (showTSType ty) <> ";\n"

emitValueDeclaration :: Maybe Text -> Text -> TSType -> ModuleWriter ()
emitValueDeclaration comment name ty
  | nameIsJsReserved name = do
      let jsName = "$$" <> TB.fromText name
      tell $ "declare const " <> jsName <> ": " <> TB.fromText (showTSType ty) <> ";\nexport " <> commentPart <> "{ " <> jsName <> " as " <> TB.fromText name <> " };\n"
  | isIdentifierName name = do
      tell $ "export const " <> commentPart <> TB.fromText name <> ": " <> TB.fromText (showTSType ty) <> ";\n"
  | otherwise = do
      -- As of PureScript 0.11.7, the compiler emits symbols that contain prime symbol `'`;
      -- Such identifiers cannot be used in ES6 modules.
      tell $ "// The identifier \"" <> TB.fromText name <> "\" cannot be expressed in JavaScript:\n// export const " <> commentPart <> TB.fromText name <> ": " <> TB.fromText (showTSType ty) <> ";\n"
        where commentPart = case comment of
                              Just commentText -> "/*" <> TB.fromText commentText <> "*/ "
                              Nothing -> mempty

processLoadedModule :: Environment -> ExternsFile -> Bool -> IO TB.Builder
processLoadedModule env ef importAll = execWriterT $ do
  tell $ "// module " <> TB.fromText (runModuleName currentModuleName) <> "\n"
  (moduleImportMap, moduleBody) <- lift (execRWST (mapM_ processDecl (efDeclarations ef)) () (Map.singleton currentModuleName Nothing))
  if importAll
    then do
    -- Emit 'import' statements for all modules referenced, whether or not they are actually used in the type declarations.
    let explicitlyImported = List.nub (map eiModule (efImports ef))
        allImports = Map.keys moduleImportMap
    forM_ (explicitlyImported `List.union` allImports) $
      \moduleName ->
        case Map.lookup moduleName moduleImportMap of
          Just (Just ident) ->
            tell $ "import * as " <> TB.fromText ident <> " from \"../" <> TB.fromText (runModuleName moduleName) <> "\";\n"
          Nothing | moduleName /= ModuleName [ProperName "Prim"] ->
            tell $ "import \"../" <> TB.fromText (runModuleName moduleName) <> "\";\n"
          _ -> return ()
    else
    -- Only emit 'import' statements for modules that are actually used in the type declarations.
    forM_ (catMaybes $ sequence <$> Map.toList moduleImportMap) $
         \(moduleName,ident) ->
           tell $ "import * as " <> TB.fromText ident <> " from \"../" <> TB.fromText (runModuleName moduleName) <> "\";\n"
  tell moduleBody

  where
    currentModuleName :: ModuleName
    currentModuleName = efModuleName ef

    qualCurrentModule :: a -> Qualified a
    qualCurrentModule = Qualified (Just currentModuleName)

    -- Get the JS identifier for given module
    getModuleId :: ModuleName -> ModuleWriter (Maybe Text)
    getModuleId (ModuleName [ProperName "Prim"]) = return Nothing -- should not occur
    getModuleId moduleName@(ModuleName components) = do
      mid <- gets (Map.lookup moduleName)
      case mid of
        Nothing -> do -- not found
          let moduleId = Just $ T.intercalate "_" (runProperName <$> components)
          -- TODO: Make sure moduleId is unique
          modify (Map.insert moduleName moduleId)
          return moduleId
        Just mModuleId -> return mModuleId

    makeContext :: [Text] -> TypeTranslationContext ModuleWriter
    makeContext typeVariables = TypeTranslationContext typeVariables [] Nothing getModuleId env currentModuleName

    pursTypeToTSTypeX :: [Text] -> Type -> ExceptT Text ModuleWriter TSType
    pursTypeToTSTypeX ctx ty = withExceptT (T.pack . show) $ runReaderT (pursTypeToTSType ty) (makeContext ctx)

    pursTypeToTSTypeE :: [Text] -> Type -> ModuleWriter (Either Text TSType)
    pursTypeToTSTypeE ctx ty = runExceptT (pursTypeToTSTypeX ctx ty)

    processDecl :: ExternsDeclaration -> ModuleWriter ()
    processDecl EDType{..} = do
      let name = runProperName edTypeName
          qTypeName = qualCurrentModule edTypeName
      if isSimpleKind edTypeKind
        then case edTypeDeclarationKind of
               -- newtype declaration:
               DataType params [(ctorPName,[member])]
                 | Just (Newtype,_,_,_) <- Map.lookup (qualCurrentModule ctorPName) (dataConstructors env) -> do
                     case extractTypes edTypeKind params of
                       Just typeParameters -> do
                         member' <- pursTypeToTSTypeE typeParameters member
                         case member' of
                           Right member'' -> emitTypeDeclaration (Just "newtype") name typeParameters member''
                           Left err -> emitComment $ "newtype " <> ": error: " <> err
                       Nothing -> do
                         emitComment $ "newtype " <> name <> ": kind annotation was not available"

               -- data declaration:
               DataType params ctors -> do
                 case extractTypes edTypeKind params of
                   Just typeParameters -> do
                     let buildCtorType (ctorPName,members)
                         -- the data constructor is exported:
                         -- the data constructor should be defined somewhere in this module (see EDDataConstructor case),
                         -- so just reference it.
                           | qualCurrentModule ctorPName `Map.member` dataConstructors env
                           = let fv = typeParameters `List.intersect` concatMap freeTypeVariables members
                             in TSNamed Nothing (runProperName edTypeName <> "$$" <> runProperName ctorPName) (map TSTyVar fv)

                         -- the data constructor is not exportd (i.e. abstract):
                         -- the marker fields are non-optional, so that they cannot be implicitly casted from other types.
                           | otherwise
                           = TSRecord [ mkField "$$pursType" (TSStringLit $ mkString $ runModuleName currentModuleName <> "." <> runProperName edTypeName)
                                      , mkField "$$pursTag" (TSStringLit $ mkString $ runProperName ctorPName)
                                      , mkField "$$abstractMarker" TSNever
                                      ]
                     emitTypeDeclaration (Just "data") name typeParameters (TSUnion $ map buildCtorType ctors)
                   Nothing -> do
                     emitComment $ "data " <> name <> ": kind annotation was not available"

               -- type synonym:
               TypeSynonym
                 | Just (synonymArguments, synonymType) <- Map.lookup qTypeName (typeSynonyms env) -> do
                     case extractTypes edTypeKind synonymArguments of
                       Just typeParameters -> do
                         tsty <- pursTypeToTSTypeE typeParameters synonymType
                         case tsty of
                           Right tsty -> emitTypeDeclaration (Just "synonym") name typeParameters tsty
                           Left err -> do
                             liftIO $ putStrLn $ "type synonym: type checking error: " ++ T.unpack (prettyPrintKind edTypeKind) ++ " " ++ show synonymArguments ++ " " ++ show typeParameters ++ " " ++ T.unpack err
                             emitComment $ "type synonym " <> name <> ": error: " <> err
                       Nothing -> do
                         emitComment $ "type synonym " <> name <> ": kind annotation was not available"
                 | otherwise -> emitComment ("type (synonym) " <> name <> ": " <> prettyPrintKind edTypeKind)

               -- foreign import data:
               ExternData
                 | qTypeName == qnUnit -> do
                     -- Data.Unit
                     emitTypeDeclaration (Just "builtin") "Unit" [] (TSRecord [(mkOptionalField "$$pursType" (TSStringLit "Data.Unit.Unit"))])
                 | qTypeName `List.elem` builtins -> do
                     pst <- pursTypeToTSTypeE typeParameters (foldl TypeApp (TypeConstructor qTypeName) (map TypeVar typeParameters))
                     case pst of
                       Right pst -> emitTypeDeclaration (Just "builtin") name typeParameters pst
                       Left err -> emitComment $ "data " <> name <> ": error: " <> err
                 | otherwise -> do
                     -- Foreign type: just use 'any' type.
                     -- External '.d.ts' file needs to be supplied for better typing.
                     emitTypeDeclaration (Just "foreign") name typeParameters (TSUnknown "foreign")
                 where builtins = [qnFn0,qnFn2,qnFn3,qnFn4,qnFn5,qnFn6,qnFn7,qnFn8,qnFn9,qnFn10,qnStrMap]
                       n = numberOfTypeParams edTypeKind
                       typeParameters = map (\i -> "a" <> T.pack (show i)) [0..n-1]

               -- others:
               LocalTypeVariable -> emitComment ("unexpected local type variable: " <> name <> " :: " <> prettyPrintKind edTypeKind)
               ScopedTypeVar -> emitComment ("unexpected scoped type variable: " <> name <> " :: " <> prettyPrintKind edTypeKind)

        else emitComment ("type " <> name <> " :: " <> prettyPrintKind edTypeKind <> " : unsupported kind")

    processDecl EDDataConstructor{..} = do
      let name = runProperName edDataCtorName
      case Map.lookup (qualCurrentModule edDataCtorTypeCtor) (types env) of
        Just (k, DataType typeParameters constructors)
          | isSimpleKind k
          , Just fieldTypes <- List.lookup edDataCtorName constructors -> do
              tsty <- pursTypeToTSTypeE [] edDataCtorType
              case tsty of
                Left err -> emitComment $ "data ctor " <> name <> ": error: " <> err
                Right tsty -> case edDataCtorOrigin of
                  Data -> do
                    -- Data constructor for a 'data' declaration:
                    -- Emit an interface so that type refinement via 'instanceof' works.
                    let fieldTypeVars = map fst typeParameters `List.intersect` concatMap freeTypeVariables fieldTypes
                        dataCtorSubtypeName = runProperName edDataCtorTypeCtor <> "$$" <> runProperName edDataCtorName
                        dataCtorSubtype = TSNamed Nothing dataCtorSubtypeName (map TSTyVar fieldTypeVars)
                    fieldTypesTS <- runExceptT (mapM (pursTypeToTSTypeX fieldTypeVars) fieldTypes)
                    case fieldTypesTS of
                      Right fieldTypesTS -> do
                        let mkMarkerField | length constructors == 1 = mkOptionalField -- allow structural subtyping if there are only one constructor
                                          | otherwise = mkField -- nominal typing
                            makerFields = [ mkMarkerField "$$pursType" (TSStringLit (mkString $ runModuleName currentModuleName <> "." <> runProperName edDataCtorTypeCtor))
                                          , mkMarkerField "$$pursTag" (TSStringLit (mkString $ runProperName edDataCtorName))
                                          ]
                            dataFields = zipWith (\f ty -> mkField (Label $ mkString $ runIdent f) ty) edDataCtorFields fieldTypesTS
                        emitInterface dataCtorSubtypeName fieldTypeVars (makerFields <> dataFields)

                        -- The constructor function has a 'new' signature returning that interface.
                        let ctorFieldName | null edDataCtorFields = "value"
                                          | otherwise = "create"
                            ctorType = TSRecord [ mkField ctorFieldName tsty
                                                , NewSignature fieldTypeVars fieldTypesTS dataCtorSubtype
                                                ]
                        emitValueDeclaration (Just "data ctor") name ctorType
                      Left err -> do
                        emitComment $ "data ctor " <> name <> ": error: " <> err

                  Newtype ->
                    -- Data constructor for a 'newtype' declaration:
                    -- No 'new' signature: just define a function.
                    emitValueDeclaration (Just "newtype data ctor") name tsty

        Nothing -> emitComment $ "the type of an exported data constructor must be exported: " <> name
        Just (k, DataType _typeParameters _constructors) -> emitComment $ "unrecognized data constructor: " <> name <> " kind: " <> prettyPrintKind k
        _ -> emitComment $ "unrecognized data constructor: " <> name

    processDecl EDValue{..} = do
      let name = runIdent edValueName
      tsty <- pursTypeToTSTypeE [] edValueType
      case tsty of
        Right tsty -> emitValueDeclaration Nothing name tsty
        Left err -> emitComment $ "value " <> name <> ": error: " <> err

    processDecl EDInstance{..}
      | Just constraints <- edInstanceConstraints
      , Just (_synonymParams,_synonymType) <- Map.lookup qDictTypeName (typeSynonyms env) = do
          -- TODO: This code depends on the undocumented implementation-details...
          let {-synonymInstance = replaceAllTypeVars (zip (freeTypeVariables synonymType) edInstanceTypes) synonymType-}
              dictTy = foldl TypeApp (TypeConstructor qDictTypeName) edInstanceTypes
              desugaredInstanceType = quantify (foldr ConstrainedType dictTy constraints)
          instanceTy <- pursTypeToTSTypeE [] desugaredInstanceType
          case instanceTy of
            Right instanceTy -> emitValueDeclaration (Just "instance") name instanceTy
            Left err -> emitComment $ "instance " <> name <> ": error: " <> err
      | otherwise = emitComment ("invalid instance declaration '" <> name <> "'")
      where name = runIdent edInstanceName :: Text
            qDictTypeName = fmap coerceProperName edInstanceClassName :: Qualified (ProperName 'TypeName)

    processDecl EDKind{..} = do
      -- Do nothing for kind declarations: just put a comment.
      let name = runProperName edKindName
      emitComment ("kind " <> name)

    processDecl EDTypeSynonym{} = do
      -- Ignored: should be handled in EDType case.
      return ()

    processDecl EDClass{} = do
      -- Ignored: should be handled in EDType case.
      return ()

processModule :: FilePath -> FilePath -> String -> Bool -> IO ()
processModule inputDir outputDir modname importAll = do
  let entryModule = moduleNameFromString (T.pack modname)
  (env,m) <- execStateT (recursivelyLoadExterns inputDir entryModule) (initEnvironment, Map.empty)
  forM_ m $ \v -> case v of
              Just ef -> do
                let moduleName = runModuleName (efModuleName ef)
                    moduleDir = outputDir </> T.unpack moduleName
                createDirectoryIfMissing True moduleDir
                modTsd <- processLoadedModule env ef importAll
                TL.writeFile (moduleDir </> "index.d.ts") (TB.toLazyText modTsd)
              Nothing -> return ()

data TsdOutput = TsdOutputDirectory FilePath | StdOutput

data PursTsdGen = PursTsdGen
  { pursOutputDirectory :: FilePath
  , tsdOutputDirectory :: Maybe FilePath
  , mainModuleName :: String
  , importAll :: Bool
  }

pursTsdGen :: Parser PursTsdGen
pursTsdGen = PursTsdGen
  <$> strOption (long "purs-output-directory" <> metavar "<dir>" <> help "PureScript output directory")
  <*> optional (strOption (long "tsd-output-directory" <> metavar "<dir>" <> help "where to write .d.ts files; same as --purs-output-directory by default"))
  <*> strOption (long "main" <> short 'm' <> metavar "<string>" <> help "The application's main module")
  <*> switch (long "import-all" <> help "import dependent modules even if not referenced")

main :: IO ()
main = execParser opts >>= \PursTsdGen{..} -> do
  processModule pursOutputDirectory (fromMaybe pursOutputDirectory tsdOutputDirectory) mainModuleName importAll
  where
    opts = info (pursTsdGen <**> helper)
      (fullDesc <> progDesc "Generate .d.ts files for PureScript modules" <> header "purs-tsd-gen - .d.ts generator for PureScript")
