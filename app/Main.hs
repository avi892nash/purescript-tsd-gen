{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import           Data.Version (showVersion)
import qualified Language.PureScript (version)
import           Language.PureScript.Environment
import           Language.PureScript.Externs
import           Language.PureScript.Names
import           Language.PureScript.TsdGen.Module
import           Options.Applicative
import           Paths_purescript_tsd_gen (version)
import           Prelude hiding (elem, lookup, notElem)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   listDirectory)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, stderr)

processModules :: FilePath -> Maybe FilePath -> [String] -> Bool -> ExceptT ModuleProcessingError IO ()
processModules inputDir mOutputDir modules importAll = do
  let loadOneModule = recursivelyLoadExterns inputDir . moduleNameFromString . T.pack
  -- TODO: Check efVersion
  (env,m) <- execStateT (mapM_ loadOneModule modules) (initEnvironment, Map.empty)
  forM_ (catMaybes $ Map.elems m) $ \ef -> do
    let moduleName = runModuleName (efModuleName ef)
    modTsd <- processLoadedModule env ef importAll
    liftIO $ case mOutputDir of
      Just outputDir -> do
        let moduleDir = outputDir </> T.unpack moduleName
        createDirectoryIfMissing True moduleDir
        TL.writeFile (moduleDir </> T.unpack (T.replace (T.singleton '.') (T.singleton '_') moduleName) <> ".res") (TB.toLazyText modTsd)
      Nothing -> do -- write to stdout
        TL.putStr (TB.toLazyText modTsd)

data TsdOutput = TsdOutputDirectory FilePath | StdOutput | SameAsInput

data PursTsdGen = PursTsdGen { pursOutputDirectory :: FilePath
                             , tsdOutput           :: TsdOutput
                             , importAll           :: Bool
                             , moduleNames         :: [String]
                             }
                | ShowVersion

tsdOutputParser :: Parser TsdOutput
tsdOutputParser = (TsdOutputDirectory <$> strOption (long "tsd-directory" <> metavar "<dir>" <> help "Where to write .d.ts files; same as --directory by default"))
                  <|> flag' StdOutput (long "stdout" <> help "Write to stdout (for dry-run)")
                  <|> pure SameAsInput

pursTsdGen :: Parser PursTsdGen
pursTsdGen = PursTsdGen
  <$> strOption (long "directory" <> short 'd' <> metavar "<dir>" <> help "PureScript's output directory (typically ./output)")
  <*> tsdOutputParser
  <*> switch (long "import-all" <> help "Import dependent modules even if not referenced")
  <*> (many (strArgument (metavar "<modules>" <> help "List of modules to export (all if omitted). Glob-like patterns '*' and '**' are parsed.")))
  <|> flag' ShowVersion (long "version" <> short 'v' <> help "Show version")

-- |
-- >>> filter (testModuleGlob "Foo.*") ["FooBar","Foo.Bar","Foo.Bar.Baz"]
-- ["Foo.Bar"]
-- >>> filter (testModuleGlob "Foo*") ["Foo.Bar","FooBar"]
-- ["FooBar"]
-- >>> filter (testModuleGlob "Foo*Bar") ["FooBar","FooBazBar","FooBaz.Bar"]
-- ["FooBar","FooBazBar"]
-- >>> filter (testModuleGlob "Foo**Bar") ["FooBar","FooBazBar","FooBaz.Bar"]
-- ["FooBar","FooBazBar","FooBaz.Bar"]
testModuleGlob :: String -> String -> Bool
testModuleGlob [] [] = True
-- '**': wildcard, including '.'
testModuleGlob ('*':'*':xs) s = any (testModuleGlob xs) (List.tails s)
-- '*': wildcard, not including '.'
testModuleGlob ('*':xs) s = any (testModuleGlob xs) (tails' s)
  where
    tails' :: String -> [String]
    tails' [] = [[]]
    tails' t@(y:ys) | y == '.' = [t]
                    | otherwise = t : tails' ys
testModuleGlob (x:xs) (y:ys) | x == y = testModuleGlob xs ys
testModuleGlob _ _ = False

isGlobPattern :: String -> Bool
isGlobPattern = List.elem '*'

main :: IO ()
main = do
  p <- execParser opts
  case p of
    PursTsdGen{..} -> do
      dirEntries <- listDirectory pursOutputDirectory
      allModules <- filterM (\name -> doesDirectoryExist (pursOutputDirectory </> name)) dirEntries
      let selectedModules = case moduleNames of
                              [] -> allModules
                              _ -> let (patterns,literals) = List.partition isGlobPattern moduleNames
                                   in List.nub $ filter (\t -> any (flip testModuleGlob t) patterns) allModules ++ literals
      let tsdOutputDirectory = case tsdOutput of
            TsdOutputDirectory dir -> Just dir
            StdOutput              -> Nothing
            SameAsInput            -> Just pursOutputDirectory
      result <- runExceptT $ processModules pursOutputDirectory tsdOutputDirectory selectedModules importAll
      case result of
        Left err -> do hPutStrLn stderr (show err) -- TODO: Better error handling
                       exitFailure
        Right _  -> return ()
    ShowVersion -> do
      putStrLn $ "purs-tsd-gen " <> showVersion version
        <> " (works with purescript " <> showVersion Language.PureScript.version <> ")"
  where
    opts = info (pursTsdGen <**> helper)
      (fullDesc
        <> progDesc "Generate .d.ts files for PureScript modules"
        <> header ("purs-tsd-gen " <> showVersion version <> " - .d.ts generator for PureScript"))
