{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Loader
  ( writeLoader
  ) where

import           Control.Arrow                            ((&&&))
import           Control.Monad
import           Data.Either.Validation
import           Data.List.Extra
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Text.Extra                          (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Platform
import           Spec.Savvy.Type                          hiding (TypeName)
import           Spec.Savvy.Type.Haskell
import           Write.Element
import           Write.Type.Enum
import           Write.Util

writeLoader
  :: (Text -> Maybe Text)
  -- ^ Enum name resolver
  -> [Platform]
  -- ^ Platform guard info
  -> [Command]
  -- ^ Commands in the spec
  -> Either [SpecError] WriteElement
writeLoader getEnumName platforms commands = do
  let
    platformGuardMap :: Text -> Maybe Text
    platformGuardMap =
      (`Map.lookup` (Map.fromList
                      ((Spec.Savvy.Platform.pName &&& pProtect) <$> platforms)
                    )
      )
  (weDoc, is, es) <- writeLoaderDoc platformGuardMap commands
  let
    exposedCommands = (filter (isJust . cCommandLevel) commands)
    weName          = "Dynamic Function Pointer Loaders"
    weExtensions    = ["CPP", "ForeignFunctionInterface", "MagicHash"] ++ es
    weImports =
      [ Import "Foreign.Ptr" ["FunPtr", "castPtrToFunPtr", "nullFunPtr"]
        , QualifiedImport "GHC.Ptr"     ["Ptr(..)"]
        ]
        ++ is
    cmdProvides :: Command -> [Guarded Export]
    cmdProvides Command {..} =
      (case platformGuardMap =<< cPlatform of
          Nothing -> Unguarded
          Just g  -> Guarded g
        )
        <$> (  [Term (dropVk cName)]
            ++ [Term (("has" <>) . T.upperCaseFirst . dropVk $ cName)]
            )
    weProvides =
      (   Unguarded
        <$> ([TypeConstructor, Term] <*> ["DeviceCmds", "InstanceCmds"])
        )
        ++ (Unguarded . Term <$> ["initDeviceCmds", "initInstanceCmds"])
        ++ (cmdProvides =<< exposedCommands)
    weReexports    = []
    weReexportable = []
    -- TODO: Write these like the imports and extensions, and move all that
    -- to some writer monad.
    weDepends =
      concat
          [ case platformGuardMap =<< cPlatform c of
              Nothing -> Unguarded <$> (commandDepends getEnumName $ c)
              Just g  -> Guarded g <$> (commandDepends getEnumName $ c)
          | c <- exposedCommands
          ]
        ++ (   Unguarded
           <$> [ TermName "vkGetDeviceProcAddr"
               , TermName "vkGetInstanceProcAddr"
               ]
           )
  pure WriteElement {..}

writeLoaderDoc
  :: (Text -> Maybe Text)
  -- ^ Platform guard info
  -> [Command]
  -> Either [SpecError] (DocMap -> Doc (), [Import], [Text])
writeLoaderDoc platformGuardMap commands = do
  let deviceLevelCommands =
        [ c | c <- commands, cCommandLevel c == Just Device ]
      instanceLevelCommands =
        [ c
        | c <- commands
        , cCommandLevel c `elem` [Just Instance, Just PhysicalDevice]
        ]
  (drs, is , es ) <- unzip3 <$> traverseP (writeRecordMember platformGuardMap) deviceLevelCommands
  (dfs, is', es') <-
    unzip3 <$> traverseP (writeFunction platformGuardMap "Device") deviceLevelCommands
  (irs, is'', es'') <-
    unzip3 <$> traverseP (writeRecordMember platformGuardMap) instanceLevelCommands
  (ifs, is''', es''') <-
    unzip3 <$> traverseP (writeFunction platformGuardMap "Instance") instanceLevelCommands
  pure $
    let d = \_ -> [qci|
      data DeviceCmds = DeviceCmds
        \{ {indent (-2) . separatedWithGuards "," $ drs}
        }
        deriving (Show)

      data InstanceCmds = InstanceCmds
        \{ {indent (-2) . separatedWithGuards "," $ irs}
        }
        deriving (Show)

      {initFunction platformGuardMap "Device" deviceLevelCommands}

      {initFunction platformGuardMap "Instance" instanceLevelCommands}

      {separatedWithGuards "" $ hasFunction platformGuardMap "Device" <$> deviceLevelCommands}

      {separatedWithGuards "" $ hasFunction platformGuardMap "Instance" <$> instanceLevelCommands}

      -- * Device commands
      {separatedWithGuards "" $ dfs}

      -- * Instance commands
      {separatedWithGuards "" $ ifs}
    |]
    in (d, concat $ concat [is, is', is'', is'''], concat $ concat [es, es', es'', es'''])

hasFunction :: (Text -> Maybe Text) -> Text -> Command -> (Doc (), Maybe Text)
hasFunction gm domain Command{..} = ([qci|
    has{T.upperCaseFirst $ dropVk cName} :: {domain}Cmds -> Bool
    has{T.upperCaseFirst $ dropVk cName} = (/= nullFunPtr) . p{T.upperCaseFirst cName}
  |], gm =<< cPlatform)

-- | The initialization function for a set of command pointers
initFunction :: (Text -> Maybe Text) -> Text -> [Command] -> Doc ()
initFunction gm domain commands = [qci|
    init{domain}Cmds :: Vk{domain} -> IO {domain}Cmds
    init{domain}Cmds handle = {domain}Cmds
      <$> {indent (-4) $ separatedWithGuards "<*>"
           ((initLine domain &&& (gm <=< cPlatform)) <$> commands)}
  |]
  where
    initLine :: Text -> Command -> Doc ()
    initLine domain Command{..} = [qci|(castPtrToFunPtr <$> vkGet{domain}ProcAddr handle (GHC.Ptr.Ptr "{cName}\NUL"#))|]

writeRecordMember
  :: (Text -> Maybe Text)
  -- ^ platform to guard
  -> Command
  -> Either [SpecError] ((Doc (), Maybe Text), [Import], [Text])
writeRecordMember gp c@Command{..} = do
  (t, (is, es)) <- toHsTypePrec 10 (commandType c)
  let d= [qci|
        p{T.upperCaseFirst cName} :: FunPtr {t}
      |]
  pure ((d, gp =<< cPlatform), is, es)

writeFunction
  :: (Text -> Maybe Text)
  -- ^ platform to guard
  -> Text
  -> Command
  -> Either [SpecError] ((Doc (), Maybe Text), [Import], [Text])
writeFunction gm domain c@Command{..} = do
  (t, (is, es)) <- toHsTypePrec 10 (commandType c)
  let upperCaseName = T.upperCaseFirst cName
      d= [qci|
        {dropVk cName} :: {domain}Cmds -> {t}
        {dropVk cName} deviceCmds = mk{upperCaseName} (p{upperCaseName} deviceCmds)
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "dynamic" mk{upperCaseName}
          :: FunPtr {t} -> {t}
      |]
  pure ((d, gm =<< cPlatform), is, es)

traverseP f xs = validationToEither $ traverse (eitherToValidation . f) xs

dropVk :: Text -> Text
dropVk = T.lowerCaseFirst . T.dropPrefix' "vk"

commandDepends :: (Text -> Maybe Text) -> Command -> [HaskellName]
commandDepends getEnumName Command {..} =
  let protoDepends = typeDepends $ Proto
        cReturnType
        [ (Just n, lowerArrayToPointer t) | Parameter n t <- cParameters ]
      protoDependsNoPointers = typeDepends $ Proto
        cReturnType
        [ (Just n, t)
        | Parameter n t <- cParameters
        , not (isPtrType t)
        , not (isArrayType t)
        ]
  in  protoDepends
        <> -- The constructors for an enum type need to be in scope
           -- Unless they're just used as pointers
           [ TypeName e
           | TypeName n <- protoDependsNoPointers
           , Just e <- [getEnumName n]
           ]
