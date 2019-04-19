{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Command
  ( writeCommand
  ) where

import           Data.Maybe
import           Data.Text                                (Text)
import qualified Data.Text.Extra                          as T
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Command
import           Spec.Savvy.Error
import           Spec.Savvy.Type                   hiding ( TypeName )
import           Spec.Savvy.Type.Haskell           hiding ( toHsType )

import           Write.Element
import           Write.Util
import           Write.Marshal.Monad

writeCommand
  :: (Text -> Maybe Text)
  -- ^ A function to get the target of an enum alias
  -> Command
  -> Either [SpecError] WriteElement
writeCommand getEnumName fp@Command {..} = do
  wrapMToWriteElements ("Command:" T.<+> cName) Nothing $ do
    let staticGuard = if cName == "vkGetInstanceProcAddr"
          then "EXPOSE_VKGETINSTANCEPROCADDR"
          else case cAvailability of
            CoreCommand major minor ->
              "EXPOSE_CORE" <> T.tShow major <> T.tShow minor <> "_COMMANDS"
            ExtensionCommand -> "EXPOSE_STATIC_EXTENSION_COMMANDS"
        protoDepends = typeDepends $ Proto
          cReturnType
          [ (Just n, lowerArrayToPointer t) | Parameter n t _ _ <- cParameters ]
    tellDepends
      $  (Unguarded <$> protoDepends)
      <> -- The constructors for an enum type need to be in scope
         [ Guarded (Guard staticGuard) (TypeName e)
         | TypeName n <- protoDepends
         , Just e <- [getEnumName n]
         ]
    traverse
      tellExport
      [ Unguarded (Term cName)
      , Unguarded (TypeAlias ("FN_" <> cName))
      , Unguarded (TypeAlias ("PFN_" <> cName))
      ]
    commandDoc staticGuard fp


commandDoc
  :: Text
  -> Command
  -> WrapM (DocMap -> Doc ())
commandDoc staticGuard c@Command {..} = do
  let upperCaseName = T.upperCaseFirst cName
  t <- toHsType (commandType c)
  dynDoc :: Doc () <- case cCommandLevel of
    Nothing -> do
      tellExtension "TypeApplications"
      tellImport "Foreign.Ptr" "FunPtr"
      tellImport "Foreign.Ptr" "nullPtr"
      tellImport "Foreign.Ptr" "castPtrToFunPtr"
      tellImport "System.IO.Unsafe" "unsafeDupablePerformIO"
      tellQualifiedImport "GHC.Ptr" "Ptr(Ptr)"
      tellDepend (Unguarded (TermName "vkGetInstanceProcAddr"))
      pure [qci|
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "dynamic" mk{T.upperCaseFirst (cName)}
          :: FunPtr ({t}) -> ({t})

        {cName} :: {t}
        {cName} = mk{T.upperCaseFirst (cName)} procAddr
          where
            procAddr = castPtrToFunPtr @_ @FN_{cName} $
              unsafeDupablePerformIO
                $ vkGetInstanceProcAddr nullPtr (GHC.Ptr.Ptr "{cName}\NUL"#)
      |]
    Just d -> do
      let domain = case d of
                     Instance -> "Instance"
                     _ -> "Device"
      tellImport "Foreign.Ptr" "FunPtr"
      tellDepend (Guarded (InvGuard staticGuard) (TypeName (domain <> "Cmds")))
      pure [qci|
        {cName} :: {domain}Cmds -> ({t})
        {cName} deviceCmds = mk{upperCaseName} (p{upperCaseName} deviceCmds)
        foreign import ccall
        #if !defined(SAFE_FOREIGN_CALLS)
          unsafe
        #endif
          "dynamic" mk{upperCaseName}
          :: FunPtr ({t}) -> ({t})
      |]
  -- tellImports
  --       [ Guarded (InvGuard staticGuard) (Import "Foreign.Ptr" ["nullPtr"])
  --       | cName == "vkGetInstanceProcAddr"
  --       ] ++ (Unguarded (Import "Foreign.Ptr" ["FunPtr"]))
  pure $ \getDoc -> [qci|
    {document getDoc (TopLevel cName)}
    #if defined({staticGuard})
    foreign import ccall
    #if !defined(SAFE_FOREIGN_CALLS)
      unsafe
    #endif
      "{cName}" {cName} :: {t}
    #else
    {dynDoc}
    #endif
    type FN_{cName} = {t}
    type PFN_{cName} = FunPtr FN_{cName}
  |]
