{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Loader
  ( writeLoader
  ) where

import           Control.Arrow                            ((&&&))
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Writer.Class
import           Data.Either.Validation
import           Data.Foldable
import           Data.Traversable
import           Data.List.Extra hiding (for)
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Text.Extra                          (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Error
import           Spec.Savvy.Platform
import           Spec.Savvy.Type                          hiding (TypeName)
import           Write.Element
import           Write.Marshal.Monad
import           Write.Marshal.Util
import           Write.Util

writeLoader
  :: [Platform]
  -- ^ Platform guard info
  -> [Command]
  -- ^ Commands in the spec
  -> Either [SpecError] [WriteElement]
writeLoader platforms commands = do
  boot <- wrapMToWriteElements "Dynamic loader boot" Nothing $ do
    tellExport (Unguarded (TypeConstructor "InstanceCmds"))
    pure $ \_ -> "data InstanceCmds"
  let
    platformGuardMap :: Text -> Maybe Text
    platformGuardMap =
      (`Map.lookup` Map.fromList
        ((Spec.Savvy.Platform.pName &&& pProtect) <$> platforms)
      )
    weName              = "Dynamic Function Pointer Loaders"
    weBootElement       = Just boot
    deviceLevelCommands = [ c | c <- commands, cCommandLevel c == Just Device ]
    instanceLevelCommands =
      [ c
      | c <- commands
      , cCommandLevel c `elem` [Just Instance, Just PhysicalDevice]
      ]
    noLevelCommands = [ c | c <- commands, cCommandLevel c == Nothing ]
    writeCommands :: [Command] -> Text -> Either [SpecError] [WriteElement]
    writeCommands commands level = for commands $ \c -> wrapMToWriteElements
      ("Dynamic loader for" T.<+> cName c)
      Nothing
      ((writeFunction platformGuardMap level) c)
  r <- wrapMToWriteElements
    weName
    weBootElement
    (writeLoaderDoc platformGuardMap
                    deviceLevelCommands
                    instanceLevelCommands
                    noLevelCommands
    )
  -- dfs <- writeCommands deviceLevelCommands "Device"
  -- ifs <- writeCommands instanceLevelCommands "Instance"
  pure $ [r] -- : dfs ++ ifs

writeLoaderDoc
  :: (Text -> Maybe Text)
  -- ^ Platform guard info
  -> [Command]
  -- ^ Device commands
  -> [Command]
  -- ^ Instance commands
  -> [Command]
  -- ^ No level commands
  -> WrapM (DocMap -> Doc ())
writeLoaderDoc platformGuardMap deviceLevelCommands instanceLevelCommands noLevelCommands = do
  drs <- traverse (writeRecordMember platformGuardMap) deviceLevelCommands
  irs <- traverse (writeRecordMember platformGuardMap) instanceLevelCommands
  ifi <- initInstanceFunction platformGuardMap instanceLevelCommands
  ifd <- initDeviceFunction platformGuardMap deviceLevelCommands
  tellExport (Unguarded (TypeConstructor "DeviceCmds"))
  tellExport (Unguarded (TypeConstructor "InstanceCmds"))
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

hasFunction :: (Text -> Maybe Text) -> Text -> Command -> (Doc (), Maybe Text)
hasFunction gm domain Command{..} = ([qci|
    has{T.upperCaseFirst $ dropVk cName} :: {domain}Cmds -> Bool
    has{T.upperCaseFirst $ dropVk cName} = (/= nullFunPtr) . p{T.upperCaseFirst cName}
  |], gm =<< cPlatform)

-- | The initialization function for a set of command pointers
initInstanceFunction :: (Text -> Maybe Text) -> [Command] -> WrapM (Doc ())
initInstanceFunction gm commands = do
  initLines <- traverse (initLine gm "vkGetInstanceProcAddr'") commands
  tellSourceDepend (Unguarded (TypeName "VkInstance"))
  tellExport (Unguarded (Term "initInstanceCmds"))
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
initDeviceFunction :: (Text -> Maybe Text) -> [Command] -> WrapM (Doc ())
initDeviceFunction gm commands = do
  initLines <- traverse (initLine gm "getDeviceProcAddr'") commands
  tellSourceDepend (Unguarded (TypeName "VkDevice"))
  tellSourceDepend (Unguarded (TermName "vkGetInstanceProcAddr"))
  tellExport (Unguarded (Term "initDeviceCmds"))
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

initLine :: (Text -> Maybe Text) -> Text -> Command -> WrapM (Doc ())
initLine gm getProcAddr c@Command{..} = censorGuarded gm c $ do
  tellSourceDepend (Unguarded (TypeName ("FN_" <> cName)))
  tellExtension "MagicHash"
  tellExtension "TypeApplications"
  tellImport "Foreign.Ptr" "castPtrToFunPtr"
  pure [qci|(castPtrToFunPtr @_ @FN_{cName} <$> {getProcAddr} handle (GHC.Ptr.Ptr "{cName}\NUL"#))|]

writeRecordMember
  :: (Text -> Maybe Text)
  -- ^ platform to guard
  -> Command
  -> WrapM (Doc (), Maybe Text)
writeRecordMember gp c@Command {..} = do
  t <- censorSourceDepends [TypeName "(:::)"] $ censorGuarded gp c $ toHsType
    (commandType c)
  tellImport "Foreign.Ptr" "FunPtr"
  censorSourceDepends [TypeName "(:::)"]
    . traverse tellDepend
    . commandDepends gp
    $ c
  let d = [qci|p{T.upperCaseFirst cName} :: FunPtr ({t})|]
  pure (d, gp =<< cPlatform)

censorGuarded
  :: (Text -> Maybe Text)
  -- ^ platform to guard
  -> Command
  -> WrapM a
  -> WrapM a
censorGuarded gp Command {..}
  = let
      makeGuarded = case gp =<< cPlatform of
        Nothing -> id
        Just g ->
          let replaceGuards :: [Guarded a] -> [Guarded a]
              replaceGuards = fmap (Guarded (Guard g) . unGuarded)
          in  \(a, (exports, undependableExports), (depends, sourceDepends), d, e) ->
                ( a
                , (replaceGuards exports, replaceGuards undependableExports)
                , (replaceGuards depends, replaceGuards sourceDepends)
                , d
                , e
                )
    in  censor makeGuarded

writeFunction
  :: (Text -> Maybe Text)
  -- ^ platform to guard
  -> Text
  -> Command
  -> WrapM (DocMap -> Doc ())
writeFunction gm domain c@Command{..} = censorGuarded gm c $ do
  -- This is taken care of in a better way with commandDepends (importing
  -- constructors)
  t <- censor (const mempty) $ toHsType (commandType c)
  tellDepends . commandDepends gm $ c
  tellExtension "ForeignFunctionInterface"
  tellImport "Foreign.Ptr" "FunPtr"
  tellExport (Unguarded (Term (dropVk cName)))
  let upperCaseName = T.upperCaseFirst cName
  pure $ \getDoc -> guarded (gm =<< cPlatform) [qci|
    {document getDoc (TopLevel cName)}
    {dropVk cName} :: {domain}Cmds -> ({t})
    {dropVk cName} deviceCmds = mk{upperCaseName} (p{upperCaseName} deviceCmds)
    foreign import ccall
    #if !defined(SAFE_FOREIGN_CALLS)
      unsafe
    #endif
      "dynamic" mk{upperCaseName}
      :: FunPtr ({t}) -> ({t})
  |]

traverseP
  :: (Semigroup e, Traversable t) => (a -> Either e b) -> t a -> Either e (t b)
traverseP f xs = validationToEither $ traverse (eitherToValidation . f) xs

commandDepends
  :: (Text -> Maybe Text)
  -- ^ platform map
  -> Command
  -> [Guarded HaskellName]
commandDepends platformGuardMap Command {..} =
  let protoDepends = typeDepends $ Proto
        cReturnType
        [ (Just n, lowerArrayToPointer t) | Parameter n t _ _ <- cParameters ]
      protoDependsNoPointers = typeDepends $ Proto
        cReturnType
        [ (Just n, t)
        | Parameter n t _ _ <- cParameters
        , not (isPtrType t)
        , not (isArrayType t)
        ]
      names = protoDepends
  in  case platformGuardMap =<< cPlatform of
        Nothing -> Unguarded <$> names
        Just g  -> Guarded (Guard g) <$> names


-- | Write the commands which do not require a valid instance
writeInitializationCommands :: [Command] -> WrapM (Doc ())
writeInitializationCommands commands = do
  let findCommand n = case find ((== n) . cName) commands of
        Nothing ->
          throwError [Other ("Couldn't find initialization command:" T.<+> n)]
        Just c -> pure c
  enumerateInstanceVersion <- findCommand "vkEnumerateInstanceVersion"
  enumerateInstanceExtensionProperties <- findCommand
    "vkEnumerateInstanceExtensionProperties"
  enumerateInstanceLayerProperties <- findCommand
    "vkEnumerateInstanceLayerProperties"
  createInstance     <- findCommand "vkCreateInstance"

  tellImport "Foreign.Ptr" "FunPtr"
  tellImport "Foreign.Ptr" "castPtrToFunPtr"
  tellImport "Foreign.Ptr" "nullPtr"
  tellImport "System.IO.Unsafe" "unsafeDupablePerformIO"
  tellDepend (Unguarded (TermName "vkGetInstanceProcAddr'"))
  tellExtension "MagicHash"

  let go c = do
        ty <- toHsType (commandType c)
        tellUndependableExport (Unguarded (Term (dropVk (cName c))))
        tellDepend (Unguarded (TypeName ("FN_" <> cName c)))
        pure [qci|
          foreign import ccall
          #if !defined(SAFE_FOREIGN_CALLS)
            unsafe
          #endif
            "dynamic" mk{T.upperCaseFirst (cName c)}
            :: FunPtr ({ty}) -> ({ty})

          {dropVk (cName c)} :: {ty}
          {dropVk (cName c)} = mk{T.upperCaseFirst (cName c)} procAddr
            where
              procAddr = castPtrToFunPtr @_ @FN_{cName c} $
                unsafeDupablePerformIO
                  $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "{cName c}\NUL"#)
        |]

  vcat <$> traverse go
    [ enumerateInstanceVersion
    , enumerateInstanceExtensionProperties
    , enumerateInstanceLayerProperties
    , createInstance
    ]
