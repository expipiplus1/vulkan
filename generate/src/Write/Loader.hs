{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Loader
  ( writeLoader
  ) where

import           Control.Arrow                            ( (&&&) )
import           Control.Monad
import           Data.Foldable
import           Data.Traversable
import           Data.List.Extra                   hiding ( for )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Text.Extra                          ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented
import           Control.Monad.State.Strict

import           Spec.Savvy.Command
import           Spec.Savvy.Platform
import           Spec.Savvy.Type                   hiding ( TypeName )
import           Write.Element
import           Write.Monad
import           Write.Util

writeLoader
  :: [Platform]
  -- ^ Platform guard info
  -> [Command]
  -- ^ Commands in the spec
  -> Write WriteElement
writeLoader platforms commands = do
  boot <- runWE "Dynamic loader boot" $ do
    tellExport (TypeConstructor "InstanceCmds")
    pure $ \_ -> "data InstanceCmds"
  let
    platformGuardMap :: Text -> Maybe Text
    platformGuardMap =
      (`Map.lookup` Map.fromList
        ((Spec.Savvy.Platform.pName &&& pProtect) <$> platforms)
      )
    weName              = "Dynamic Function Pointer Loaders"
    deviceLevelCommands = [ c | c <- commands, cCommandLevel c == Just Device ]
    instanceLevelCommands =
      [ c
      | c <- commands
      , cCommandLevel c `elem` [Just Instance, Just PhysicalDevice]
      ]
  runWE weName $ do
    tellBootElem boot
    writeLoaderDoc platformGuardMap
                   deviceLevelCommands
                   instanceLevelCommands

writeLoaderDoc
  :: (Text -> Maybe Text)
  -- ^ Platform guard info
  -> [Command]
  -- ^ Device commands
  -> [Command]
  -- ^ Instance commands
  -> WE (DocMap -> Doc ())
writeLoaderDoc platformGuardMap deviceLevelCommands instanceLevelCommands = do
  drs <- traverse (writeRecordMember platformGuardMap) deviceLevelCommands
  irs <- traverse (writeRecordMember platformGuardMap) instanceLevelCommands
  ifi <- initInstanceFunction platformGuardMap instanceLevelCommands
  ifd <- initDeviceFunction platformGuardMap deviceLevelCommands
  tellExport (TypeConstructor "DeviceCmds")
  tellExport (TypeConstructor "InstanceCmds")
  pure $ \_ -> [qci|
    data DeviceCmds = DeviceCmds
      \{ deviceCmdsHandle :: VkDevice
      , {indent (-2) . separatedWithGuards "," $ drs}
      }
      deriving (Show)

    data InstanceCmds = InstanceCmds
      \{ instanceCmdsHandle :: VkInstance
      , {indent (-2) . separatedWithGuards "," $ irs}
      }
      deriving (Show)

    {ifd}

    {ifi}
  |]

-- | The initialization function for a set of command pointers
initInstanceFunction :: (Text -> Maybe Text) -> [Command] -> WE (Doc ())
initInstanceFunction gm commands = do
  initLines <- traverse (initLine gm "vkGetInstanceProcAddr'") commands
  tellSourceDepend (TypeName "VkInstance")
  tellExport (Term "initInstanceCmds")
  tellQualifiedImport "GHC.Ptr" "Ptr(..)"
  pure [qci|
    -- | A version of 'vkGetInstanceProcAddr' which can be called with a
    -- null pointer for the instance.
    foreign import ccall
    #if !defined(SAFE_FOREIGN_CALLS)
      unsafe
    #endif
      "vkGetInstanceProcAddr" vkGetInstanceProcAddr' :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction

    initInstanceCmds :: VkInstance -> IO InstanceCmds
    initInstanceCmds handle = InstanceCmds handle
      <$> {indent (-4) $ separatedWithGuards "<*>"
           (zip initLines ((gm <=< cPlatform) <$> commands))}
  |]

-- | The initialization function for a set of command pointers
initDeviceFunction :: (Text -> Maybe Text) -> [Command] -> WE (Doc ())
initDeviceFunction gm commands = do
  initLines <- traverse (initLine gm "getDeviceProcAddr'") commands
  tellSourceDepend (TypeName "VkDevice")
  tellSourceDepend (TermName "vkGetInstanceProcAddr")
  tellExport (Term "initDeviceCmds")
  tellQualifiedImport "GHC.Ptr" "Ptr(..)"
  pure [qci|
    foreign import ccall
    #if !defined(SAFE_FOREIGN_CALLS)
      unsafe
    #endif
      "dynamic" mkVkGetDeviceProcAddr
      :: FunPtr (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)

    initDeviceCmds :: InstanceCmds -> VkDevice -> IO DeviceCmds
    initDeviceCmds instanceCmds handle = do
      pGetDeviceProcAddr <- castPtrToFunPtr @_ @FN_vkGetDeviceProcAddr
        <$> vkGetInstanceProcAddr instanceCmds (instanceCmdsHandle instanceCmds) (GHC.Ptr.Ptr "vkGetDeviceProcAddr\NUL"#)
      let getDeviceProcAddr' = mkVkGetDeviceProcAddr pGetDeviceProcAddr
      DeviceCmds handle
        <$> {indent (-4) $ separatedWithGuards "<*>"
             (zip initLines ((gm <=< cPlatform) <$> commands))}
  |]

initLine :: (Text -> Maybe Text) -> Text -> Command -> WE (Doc ())
initLine gm getProcAddr c@Command{..} = censorGuarded gm c $ do
  tellSourceDepend (TypeName ("FN_" <> cName))
  tellExtension "MagicHash"
  tellExtension "TypeApplications"
  tellImport "Foreign.Ptr" "castPtrToFunPtr"
  pure [qci|(castPtrToFunPtr @_ @FN_{cName} <$> {getProcAddr} handle (GHC.Ptr.Ptr "{cName}\NUL"#))|]

writeRecordMember
  :: (Text -> Maybe Text)
  -- ^ platform to guard
  -> Command
  -> WE (Doc (), Maybe Text)
writeRecordMember gp c@Command {..} = do
  let excludedSourceDepends =
        TypeName <$> ["(:::)", "VkBool32", "VkFormat", "VkResult"]
  t <- makeSourceDepends excludedSourceDepends $ censorGuarded gp c $ toHsType
    (commandType c)
  tellImport "Foreign.Ptr" "FunPtr"
  _ <-
    makeSourceDepends excludedSourceDepends
    . traverse tellGuardedDepend
    . commandDepends gp
    $ c
  let d = [qci|p{T.upperCaseFirst cName} :: FunPtr ({t})|]
  pure (d, gp =<< cPlatform)

censorGuarded
  :: (Text -> Maybe Text)
  -- ^ platform to guard
  -> Command
  -> WE a
  -> WE a
censorGuarded gp Command {..}
  = let
      makeGuarded = case gp =<< cPlatform of
        Nothing -> id
        Just g ->
          let replaceGuards :: [Guarded a] -> [Guarded a]
              replaceGuards = fmap (Guarded (Guard g) . unGuarded)
          in  \we -> we
                { weProvides             = replaceGuards (weProvides we)
                , weUndependableProvides = replaceGuards
                                             (weUndependableProvides we)
                , weDepends              = replaceGuards (weDepends we)
                , weSourceDepends        = replaceGuards (weSourceDepends we)
                }
    in  WE . mapStateT (fmap (fmap makeGuarded)) . unWE

commandDepends
  :: (Text -> Maybe Text)
  -- ^ platform map
  -> Command
  -> [Guarded HaskellName]
commandDepends platformGuardMap Command {..} =
  let protoDepends = typeDepends $ Proto
        cReturnType
        [ (Just n, lowerArrayToPointer t) | Parameter n t _ _ <- cParameters ]
      names = protoDepends
  in  case platformGuardMap =<< cPlatform of
        Nothing -> Unguarded <$> names
        Just g  -> Guarded (Guard g) <$> names
