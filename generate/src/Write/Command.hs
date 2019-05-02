{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Write.Command
  ( writeCommand
  , commandDynamicType
  ) where

import           Data.Maybe
import           Data.Foldable
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Type                   hiding ( TypeName )
import qualified Spec.Savvy.Type               as Type

import           Write.Element
import           Write.Util
import           Write.Monad

writeCommand
  :: (Text -> Maybe Text)
  -- ^ A function to get the target of an enum alias
  -> Command
  -> Write WriteElement
writeCommand getEnumName fp@Command {..} = do
  bootElem <-
    runWE ("Command boot:" T.<+> cName)
    . makeSourceDepends [TypeName "(:::)"]
    $ do
        tellImport "Foreign.Ptr" "FunPtr"
        synonyms <- commandTypeSynonyms fp
        d        <- case cName of
          "vkGetInstanceProcAddr" -> do
            tellExport (Term "vkGetInstanceProcAddr")
            tellSourceDepend (TypeName "InstanceCmds")
            pure $ vsep
              [ [qci|
                #if defined(EXPOSE_CORE10_COMMANDS)
                vkGetInstanceProcAddr :: (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
                #else
                vkGetInstanceProcAddr :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
                #endif
                |]
              , synonyms
              ]
          _ -> pure synonyms
        pure $ const d

  runWE ("Command:" T.<+> cName) $ do
    tellBootElem bootElem
    let staticGuard = case cAvailability of
          CoreCommand major minor ->
            "EXPOSE_CORE" <> T.tShow major <> T.tShow minor <> "_COMMANDS"
          ExtensionCommand -> "EXPOSE_STATIC_EXTENSION_COMMANDS"
        protoDepends = typeDepends $ Proto
          cReturnType
          [ (Just n, lowerArrayToPointer t) | Parameter n t _ _ <- cParameters ]
    tellDepends
      $  protoDepends
      <> -- The constructors for an enum type need to be in scope
         [ TypeName e
         | TypeName n <- protoDepends
         , Just e <- [getEnumName n]
         ]
    commandDoc staticGuard fp


commandDoc
  :: Text
  -> Command
  -> WE (DocMap -> Doc ())
commandDoc staticGuard c@Command {..} = do
  let upperCaseName = T.upperCaseFirst cName
  dynType <- toHsType (commandDynamicType c)
  staticType <- toHsType (commandType c)
  dynDoc :: Doc () <- case cCommandLevel of
    Nothing -> do
      tellExtension "MagicHash"
      tellExtension "TypeApplications"
      tellImport "Foreign.Ptr" "FunPtr"
      tellImport "Foreign.Ptr" "nullPtr"
      tellImport "Foreign.Ptr" "castPtrToFunPtr"
      tellImport "System.IO.Unsafe" "unsafeDupablePerformIO"
      tellQualifiedImport "GHC.Ptr" "Ptr(Ptr)"
      tellDepend (TermName "vkGetInstanceProcAddr'")
      pure [qci|
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "dynamic" mk{T.upperCaseFirst (cName)}
          :: FunPtr ({staticType}) -> ({staticType})

        {cName} :: {staticType}
        {cName} = mk{T.upperCaseFirst (cName)} procAddr
          where
            procAddr = castPtrToFunPtr @_ @FN_{cName} $
              unsafeDupablePerformIO
                $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "{cName}\NUL"#)
      |]
    Just d -> do
      let domain = case d of
                     Instance -> "Instance"
                     PhysicalDevice -> "Instance"
                     Device -> "Device"
      tellImport "Foreign.Ptr" "FunPtr"
      tellGuardedDepend (Guarded (InvGuard staticGuard) (TypeName (domain <> "Cmds")))
      pure [qci|
        {cName} :: {dynType}
        {cName} deviceCmds = mk{upperCaseName} (p{upperCaseName} deviceCmds)
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "dynamic" mk{upperCaseName}
          :: FunPtr ({staticType}) -> ({staticType})
      |]
  getInstanceProcAddrDoc :: Doc () <- case cName of
    "vkGetInstanceProcAddr" -> do
      tellExport (Term "vkGetInstanceProcAddr'")
      pure [qci|
        -- | A version of 'vkGetInstanceProcAddr' which can be called with a
        -- null pointer for the instance.
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "vkGetInstanceProcAddr" vkGetInstanceProcAddr' :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
      |]
    _ -> pure ""
  synonyms <- commandTypeSynonyms c
  tellExport (Term cName)
  pure $ \getDoc -> [qci|
    {document getDoc (TopLevel cName)}
    #if defined({staticGuard})
    foreign import ccall
    #if !defined(SAFE_FOREIGN_CALLS)
      unsafe
    #endif
      "{cName}" {cName} :: {staticType}
    #else
    {dynDoc}
    #endif
    {getInstanceProcAddrDoc}
    {synonyms}
  |]

commandDynamicType :: Command -> Type
commandDynamicType c@Command {..} = case cCommandLevel of
  Just d | Proto ret args <- commandType c ->
    let domain = case d of
          Instance       -> "Instance"
          PhysicalDevice -> "Instance"
          Device         -> "Device"
        commandTableType = Type.TypeName $ domain <> "Cmds"
    in  Proto ret ((Nothing, commandTableType) : args)
  _ -> commandType c

commandTypeSynonyms :: Command -> WE (Doc ())
commandTypeSynonyms c@Command{..} = do
  t <- toHsType (commandType c)
  traverse_
    tellExport
    [ TypeAlias ("FN_" <> cName)
    , TypeAlias ("PFN_" <> cName)
    ]
  pure [qci|
    type FN_{cName} = {t}
    type PFN_{cName} = FunPtr FN_{cName}
  |]
