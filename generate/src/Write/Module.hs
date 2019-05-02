{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Write.Module
  ( Module(..)
  , ReexportedModule(..)
  , writeModules
  )where

import           Control.Applicative
import           Data.Char
import           Data.Functor
import           Data.List.Extra
import qualified Data.Map                      as Map
import qualified Data.MultiMap                 as MultiMap
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented

import           Documentation
import           Documentation.Haddock
import           Write.Element
import           Write.Util

data Module = Module
  { mName              :: Text
  , mWriteElements     :: [WriteElement]
  , mReexports         :: [Export]
    -- ^ Ignored, generates too much noise
  , mSeedReexports     :: [Guarded Export]
  , mReexportedModules :: [ReexportedModule]
  }

data ReexportedModule = ReexportedModule
  { rmName  :: Text
  , rmGuard :: Maybe Text
  }
  deriving (Show)

writeModules
  :: (Documentee -> Maybe Documentation)
  -- Find some documentation
  -> [Module]
  -> [(Doc (), Maybe (Doc ()))]
  -- ^ A list of modules and possible boot modules
writeModules findDoc ms =
  let moduleMapHN = findModuleHN ms
      moduleMap   = findModule ms
      getDoc :: Text -> Documentee -> Maybe Haddock
      getDoc = getModuleDoc findDoc moduleMap
  in  ms
        <&> (\m ->
              let stripConstructorExports we = we
                    { weProvides = fmap (WithoutConstructors . unExport)
                                     <$> weProvides we
                    }
                  bootModule =
                      case mapMaybe weBootElement (mWriteElements m) of
                        []  -> Nothing
                        wes -> Just m
                          { mWriteElements     = stripConstructorExports <$> wes
                          , mSeedReexports     = []
                          , mReexportedModules = []
                                      --- ^ boot modules don't reexport things
                          }
                  write = writeModule (getDoc (mName m)) moduleMapHN
              in  (write m, write <$> bootModule)
            )

getModuleDoc
  :: (Documentee -> Maybe Documentation)
  -- ^ find docs
  -> (Text -> Maybe Text)
  -- ^ Find the module exporting a name
  -> Text
  -- ^ The module name we are rendering
  -> (Documentee -> Maybe Haddock)
  -- ^ Get the rendered haddocks
getModuleDoc findDoc findModule' thisModule name = do
  doc <- findDoc name
  let getLoc :: Text -> DocumenteeLocation
      getLoc n = case findModule' n of
        Nothing -> Unknown
        Just m | m == thisModule -> ThisModule
               | otherwise       -> OtherModule m
  case documentationToHaddock getLoc doc of
    Left  _ -> Nothing -- TODO Improve
    Right h -> pure h

writeModule
  :: (Documentee -> Maybe Haddock)
  -> (HaskellName -> Maybe (Text, Guarded Export))
  -> Module
  -> Doc ()
writeModule getDoc getModule m@Module{..} = [qci|
  \{-# language Strict #-}
  \{-# language CPP #-}
  {vcat $ moduleExtensions m}

  module {mName}
    ( {indent (-2) $ separatedWithGuards "," (moduleExports m)}
    ) where

  {vcat $ moduleImports m}
  {vcat $ importReexportedModule <$> mReexportedModules}

  {vcat $ moduleInternalImports getModule m}

  {vcatPara $ (flip weDoc getDoc) <$> mWriteElements}
  |]

moduleExports :: Module -> [(Doc (), Maybe Text)]
moduleExports Module {..} =
  mapMaybe
      (exportHaskellName False)
      (  (weProvides =<< mWriteElements)
      ++ (weUndependableProvides =<< mWriteElements)
      ++ mSeedReexports
      )
    ++ [ ("module " <> pretty (rmName r), rmGuard r) | r <- mReexportedModules ]

importReexportedModule :: ReexportedModule -> Doc ()
importReexportedModule ReexportedModule {..} = case rmGuard of
  Nothing -> [qci|import {rmName}|]
  Just g  -> [qci|
      #if {g}
      import {rmName}
      #endif
    |]

exportHaskellName
  :: Bool
  -- ^ Is source import
  -> Guarded Export
  -> Maybe (Doc (), Maybe Text)
exportHaskellName isSourceImport e =
  let s = case unExport (unGuarded e) of
        TypeName n -> Just (pretty n)
        TermName n | isConstructor n -> Nothing
                   | otherwise       -> Just (pretty n)
        PatternName n -> Just ("pattern" <+> pretty n)
      doc = case unGuarded e of
        WithConstructors _ | not isSourceImport -> (<> "(..)") <$> s
        _                  -> s
      cppGuard = case e of
        Unguarded _ -> Nothing
        Guarded g _ -> Just $ guardCPPGuard g
  in  (, cppGuard) <$> doc

isConstructor :: Text -> Bool
isConstructor = \case
  T.Cons x _ | isUpper x -> True
  _                      -> False

moduleImports :: Module -> [Doc ()]
moduleImports Module {..} =
  let simplifiedImports = simplifyDependencies $ weImports =<< mWriteElements
      importMap :: Map.Map (Text, Bool, [Guard]) [Text]
      --- ^ (Module, isQualified, guard disjunction) [imports]
      importMap = sort <$> Map.fromListWith
        union
        [ ( (iModule import', isQualifiedImport import', guards)
          , iImports import'
          )
        | (import', guards) <- simplifiedImports
        ]
      makeImport :: ((Text, Bool, [Guard]), [Text]) -> Doc ()
      makeImport ((moduleName, qualified, guards), is) = guardedDisjunction (guardCPPGuard <$> guards) [qci|
         import{if qualified then " qualified " else (" " :: Text)}{pretty moduleName}
           ( {indent (-2) . vcat . intercalatePrepend "," $ pretty <$> is}
           )
       |]
  in makeImport <$> Map.assocs importMap

findModuleHN :: [Module] -> HaskellName -> Maybe (Text, Guarded Export)
findModuleHN ms =
  let nameMap = Map.fromList
        [ (unExport (unGuarded e), (mName m, e))
        | m  <- ms
        , we <- mWriteElements m
        , e  <- weProvides we
        ]
  in  (`Map.lookup` nameMap)

findModule :: [Module] -> Text -> Maybe Text
findModule ms =
  let nameMap = Map.fromList
        [ (e, mName m)
        | m  <- ms
        , we <- mWriteElements m
        , e  <- unHaskellName . unExport . unGuarded <$> weProvides we
        ]
  in  (`Map.lookup` nameMap)

moduleInternalImports
  :: (HaskellName -> Maybe (Text, Guarded Export))
  -- ^ which module is this name from
  -> Module
  -> [Doc ()]
moduleInternalImports nameModule Module {..} =
  let nonSourceDeps = simplifyDependencies (weDepends =<< mWriteElements)
      sourceDeps = simplifyDependencies (weSourceDepends =<< mWriteElements)
      reexportedDeps = simplifyDependencies (fmap unExport <$> mSeedReexports)
                       \\ nonSourceDeps
      -- A map between (ModuleName, Guards) and a list of exports
      depends :: [(HaskellName, [Guard])] -> Map.Map (Text, [Text]) [Guarded Export]
      depends deps = sort <$> Map.fromListWith
        (<>)
        [ ((m, (guardCPPGuard <$> gs)), [e])
        | (n, gs)           <- deps
        , Just (m, e) <- [nameModule n]
        , n `notElem` (unExport . unGuarded <$> (weProvides =<< mWriteElements))
        ]
      writeDeps :: Bool -> Doc () -> [(HaskellName, [Guard])] -> [Doc ()]
      writeDeps isSourceImport qualifier deps =
        Map.assocs (depends deps) <&> \((moduleName, guards), is) ->
          guardedDisjunction guards [qci|
            import {qualifier}{pretty moduleName}
              ( {indent (-2) . vcat . intercalatePrepend "," $ mapMaybe (fmap fst . exportHaskellName isSourceImport) is}
              )
          |]
  in concat [ writeDeps False "" nonSourceDeps
            , writeDeps True "{-# source #-} " sourceDeps
            , writeDeps False "" reexportedDeps
            ]

simplifyDependencies :: Ord a => [Guarded a] -> [(a, [Guard])]
simplifyDependencies deps =
  let unguarded = Set.fromList [ d | Unguarded d <- deps ]
      guarded'  = MultiMap.fromList
        [ (d, g) | Guarded g d <- deps, d `Set.notMember` unguarded ]
  in  ((, []) <$> Set.toList unguarded) ++ MultiMap.assocs guarded'

moduleExtensions :: Module -> [Doc ()]
moduleExtensions Module {..} =
  let extraExtensions =
        [ "PatternSynonyms"
        | PatternName _ <-
          unGuarded
            <$> (  (weDepends =<< mWriteElements)
                ++ (fmap unExport <$> mSeedReexports)
                )
        ]
      es = nubOrd $ (weExtensions =<< mWriteElements) ++ extraExtensions
  in  es <&> \e -> [qci|\{-# language {e} #-}|]
