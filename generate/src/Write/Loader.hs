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
import           Data.List.Extra
import qualified Data.Map                                 as Map
import           Data.Maybe
import           Data.Semigroup
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
  :: (Text -> Maybe Text)
  -- ^ Enum name resolver
  -> [Platform]
  -- ^ Platform guard info
  -> [Command]
  -- ^ Commands in the spec
  -> Either [SpecError] WriteElement
writeLoader getEnumName platforms commands = do
  let platformGuardMap :: Text -> Maybe Text
      platformGuardMap =
        (`Map.lookup` Map.fromList
          ((Spec.Savvy.Platform.pName &&& pProtect) <$> platforms)
        )
      weName          = "Dynamic Function Pointer Loaders"
      weBootElement          = Nothing
  wrapMToWriteElements weName weBootElement
        (writeLoaderDoc getEnumName platformGuardMap commands)

writeLoaderDoc
  :: (Text -> Maybe Text)
  -- ^ Enum name resolver
  -> (Text -> Maybe Text)
  -- ^ Platform guard info
  -> [Command]
  -> WrapM (DocMap -> Doc ())
writeLoaderDoc getEnumName platformGuardMap commands = do
  let deviceLevelCommands =
        [ c | c <- commands, cCommandLevel c == Just Device ]
      instanceLevelCommands =
        [ c
        | c <- commands
        , cCommandLevel c `elem` [Just Instance, Just PhysicalDevice]
        ]
  drs <- traverse (writeRecordMember getEnumName platformGuardMap) deviceLevelCommands
  dfs <- traverse (writeFunction platformGuardMap "Device") deviceLevelCommands
  irs <- traverse (writeRecordMember getEnumName platformGuardMap) instanceLevelCommands
  ifs <- traverse (writeFunction platformGuardMap "Instance") instanceLevelCommands
  ifi <- initFunction platformGuardMap "Instance" instanceLevelCommands
  ifd <- initFunction platformGuardMap "Device" deviceLevelCommands
  initializationCommands <- writeInitializationCommands commands
  tellExport (Unguarded (TypeConstructor "DeviceCmds"))
  tellExport (Unguarded (TypeConstructor "InstanceCmds"))
  pure $
    let d _ = [qci|
      {initializationCommands}

      data DeviceCmds = DeviceCmds
        \{ {indent (-2) . separatedWithGuards "," $ drs}
        }
        deriving (Show)

      data InstanceCmds = InstanceCmds
        \{ {indent (-2) . separatedWithGuards "," $ irs}
        }
        deriving (Show)

      {ifd}

      {ifi}

      -- * Device commands
      {separatedWithGuards "" $ dfs}

      -- * Instance commands
      {separatedWithGuards "" $ ifs}
    |]
    in d

      -- {separatedWithGuards "" $ hasFunction platformGuardMap "Device" <$>
      -- deviceLevelCommands}
      -- {separatedWithGuards "" $ hasFunction platformGuardMap "Instance" <$>
      -- instanceLevelCommands}

hasFunction :: (Text -> Maybe Text) -> Text -> Command -> (Doc (), Maybe Text)
hasFunction gm domain Command{..} = ([qci|
    has{T.upperCaseFirst $ dropVk cName} :: {domain}Cmds -> Bool
    has{T.upperCaseFirst $ dropVk cName} = (/= nullFunPtr) . p{T.upperCaseFirst cName}
  |], gm =<< cPlatform)

-- | The initialization function for a set of command pointers
initFunction :: (Text -> Maybe Text) -> Text -> [Command] -> WrapM (Doc ())
initFunction gm domain commands = do
  initLines <- traverse initLine commands
  tellDepend (Unguarded (TermName ("vkGet" <> domain <> "ProcAddr")))
  tellDepend (Unguarded (TypeName ("Vk" <> domain)))
  tellExport (Unguarded (Term ("init"<> domain <> "Cmds")))
  tellExtension "MagicHash"
  tellExtension "TypeApplications"
  tellImport "Foreign.Ptr" "castPtrToFunPtr"
  tellQualifiedImport "GHC.Ptr" "Ptr(..)"
  pure [qci|
    init{domain}Cmds :: Vk{domain} -> IO {domain}Cmds
    init{domain}Cmds handle = {domain}Cmds
      <$> {indent (-4) $ separatedWithGuards "<*>"
           (zip initLines ((gm <=< cPlatform) <$> commands))}
  |]
  where
    initLine :: Command -> WrapM (Doc ())
    initLine c@Command{..} = do
      censorGuarded gm c $ tellDepend (Unguarded (TypeName ("FN_" <> cName)))
      pure [qci|(castPtrToFunPtr @_ @FN_{cName} <$> vkGet{domain}ProcAddr handle (GHC.Ptr.Ptr "{cName}\NUL"#))|]

writeRecordMember
  :: (Text -> Maybe Text)
  -- ^ Enum resolver
  -> (Text -> Maybe Text)
  -- ^ platform to guard
  -> Command
  -> WrapM (Doc (), Maybe Text)
writeRecordMember getEnumName gp c@Command {..} = do
  t <- censorGuarded gp c $ toHsType (commandType c)
  tellDepends . commandDepends getEnumName gp $ c
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
  -> WrapM (Doc (), Maybe Text)
writeFunction gm domain c@Command{..} = censorGuarded gm c $ do
  -- This is taken care of in a better way with commandDepends (importing
  -- constructors)
  t <- censor (const mempty) $ toHsType (commandType c)
  tellExtension "ForeignFunctionInterface"
  tellImport "Foreign.Ptr" "FunPtr"
  tellUndependableExport (Unguarded (Term (dropVk cName)))
  let upperCaseName = T.upperCaseFirst cName
      d= [qci|
        {dropVk cName} :: {domain}Cmds -> ({t})
        {dropVk cName} deviceCmds = mk{upperCaseName} (p{upperCaseName} deviceCmds)
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "dynamic" mk{upperCaseName}
          :: FunPtr ({t}) -> ({t})
      |]
  pure (d, gm =<< cPlatform)

traverseP
  :: (Semigroup e, Traversable t) => (a -> Either e b) -> t a -> Either e (t b)
traverseP f xs = validationToEither $ traverse (eitherToValidation . f) xs

commandDepends
  :: (Text -> Maybe Text)
  -- ^ Enum resolver
  -> (Text -> Maybe Text)
  -- ^ platform map
  -> Command
  -> [Guarded HaskellName]
commandDepends getEnumName platformGuardMap Command {..} =
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
      names =
        protoDepends
          <> -- The constructors for an enum type need to be in scope
           -- Unless they're just used as pointers
             [ TypeName e
             | TypeName n <- protoDependsNoPointers
             , Just e <- [getEnumName n]
             ]
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
  tellDepend (Unguarded (TermName "vkGetInstanceProcAddr"))
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
                  $ vkGetInstanceProcAddr nullPtr (GHC.Ptr.Ptr "{cName c}\NUL"#)
        |]

  vcat <$> traverse go
    [ enumerateInstanceVersion
    , enumerateInstanceExtensionProperties
    , enumerateInstanceLayerProperties
    , createInstance
    ]
