{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Loader
  ( writeLoader
  ) where

import           Data.Either.Validation
import           Data.List.Extra
import           Data.Text.Extra                          (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Enum
import           Spec.Savvy.Error
import           Spec.Savvy.Type                          hiding (TypeName)
import           Spec.Savvy.Type.Haskell
import           Write.Element
import           Write.Type.Enum
import           Write.Util

writeLoader :: (Text -> Maybe Text) -> [Command] -> Either [SpecError] WriteElement
writeLoader getEnumName commands = do
  (weDoc, is, es) <- writeLoaderDoc commands
  let weName       = "Dynamic Function Pointer Loaders"
      weExtensions = ["ForeignFunctionInterface", "MagicHash"] ++ es
      weImports =
        [ Import "Foreign.Ptr" ["FunPtr", "castPtrToFunPtr", "nullFunPtr"]
          , QualifiedImport "GHC.Ptr" ["Ptr(..)"]
          ]
          ++ is
      weProvides = [TypeConstructor, Term] <*> ["DeviceCmds", "InstanceCmds"]
      weReexports = []
      weReexportable = []
      -- TODO: Write these like the imports and extensions, and move all that
      -- to some writer monad.
      weDepends =
        nubOrd
          $  [TermName "vkGetDeviceProcAddr", TermName "vkGetInstanceProcAddr"]
          ++ (commandDepends getEnumName =<< commands)
  pure WriteElement {..}

writeLoaderDoc :: [Command] -> Either [SpecError] (DocMap -> Doc (), [Import], [Text])
writeLoaderDoc commands = do
  let deviceLevelCommands =
        [ c | c <- commands, cCommandLevel c == Just Device ]
      instanceLevelCommands =
        [ c
        | c <- commands
        , cCommandLevel c `elem` [Just Instance, Just PhysicalDevice]
        ]
  (drs, is, es) <- unzip3 <$> traverseP writeRecordMember deviceLevelCommands
  (dfs, is', es') <- unzip3 <$> traverseP (writeFunction "Device") deviceLevelCommands
  (irs, is'', es'') <-
    unzip3 <$> traverseP writeRecordMember instanceLevelCommands
  (ifs, is''', es''') <-
    unzip3 <$> traverseP (writeFunction "Instance") instanceLevelCommands
  pure $
    let d = \_ -> [qci|
      data DeviceCmds = DeviceCmds
        \{ {indent (-2) . vcat . intercalatePrepend "," $ drs}
        }

      data InstanceCmds = InstanceCmds
        \{ {indent (-2) . vcat . intercalatePrepend "," $ irs}
        }

      {initFunction "Device" deviceLevelCommands}

      {initFunction "Instance" instanceLevelCommands}

      {vcat $ hasFunction "Device" <$> deviceLevelCommands}

      {vcat $ hasFunction "Instance" <$> instanceLevelCommands}

      -- * Device commands
      {vcat $ dfs}

      -- * Instance commands
      {vcat $ ifs}
    |]
    in (d, concat $ concat [is, is', is'', is'''], concat $ concat [es, es', es'', es'''])

hasFunction :: Text -> Command -> Doc ()
hasFunction domain Command{..} = [qci|
    has{T.upperCaseFirst $ dropVk cName} :: {domain}Cmds -> Bool
    has{T.upperCaseFirst $ dropVk cName} = (/= nullFunPtr) . p{T.upperCaseFirst cName}
  |]

-- | The initialization function for a set of command pointers
initFunction :: Text -> [Command] -> Doc ()
initFunction domain commands = [qci|
    init{domain}Cmds :: Vk{domain} -> IO {domain}Cmds
    init{domain}Cmds handle = {domain}Cmds
      <$> {indent (-4) . vcat . intercalatePrepend "<*>" $
           initLine domain <$> commands}
  |]
  where
    initLine :: Text -> Command -> Doc ()
    initLine domain Command{..} = [qci|(castPtrToFunPtr <$> vkGet{domain}ProcAddr handle (GHC.Ptr.Ptr "{cName}\NUL"#))|]

writeRecordMember :: Command -> Either [SpecError] (Doc (), [Import], [Text])
writeRecordMember c@Command{..} = do
  (t, (is, es)) <- toHsTypePrec 10 (commandType c)
  let d= [qci|
        p{T.upperCaseFirst cName} :: FunPtr {t}
      |]
  pure (d, is, es)

writeFunction :: Text -> Command -> Either [SpecError] (Doc (), [Import], [Text])
writeFunction domain c@Command{..} = do
  (t, (is, es)) <- toHsTypePrec 10 (commandType c)
  let upperCaseName = T.upperCaseFirst cName
      d= [qci|
        {dropVk cName} :: {domain}Cmds -> {t}
        {dropVk cName} deviceCmds = mk{upperCaseName} (p{upperCaseName} deviceCmds)
        foreign import ccall unsafe "dynamic" mk{upperCaseName}
          :: FunPtr {t} -> {t}
      |]
  pure (d, is, es)

traverseP f xs = validationToEither $ traverse (eitherToValidation . f) xs

dropVk :: Text -> Text
dropVk = T.lowerCaseFirst . T.dropPrefix' "vk"

commandDepends :: (Text -> Maybe Text) -> Command -> [HaskellName]
commandDepends getEnumName Command {..}
  = let protoDepends = typeDepends $ Proto
          cReturnType
          [ (Just n, lowerArrayToPointer t) | Parameter n t <- cParameters ]
    in  protoDepends
          <> -- The constructors for an enum type need to be in scope
             [ TypeName e
             | TypeName n <- protoDepends
             , Just e <- [getEnumName n]
             ]
