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
import           Spec.Savvy.Type                          hiding (TypeName)
import           Spec.Savvy.Type.Haskell

import           Write.Element
import           Write.Util

writeCommand
  :: (Text -> Maybe Text)
  -- ^ A function to get the target of an enum alias
  -> Command
  -> Either [SpecError] WriteElement
writeCommand getEnumName fp@Command {..} = do
  let staticGuard = if cName == "vkGetInstanceProcAddr"
        then "EXPOSE_VKGETINSTANCEPROCADDR"
        else case cAvailability of
          CoreCommand major minor ->
            "EXPOSE_CORE" <> T.tShow major <> T.tShow minor <> "_COMMANDS"
          ExtensionCommand -> "EXPOSE_STATIC_EXTENSION_COMMANDS"
  (weDoc, weImports, weExtensions) <- commandDoc staticGuard fp
  let weName       = "Command: " <> cName
      protoDepends = typeDepends $ Proto
        cReturnType
        [ (Just n, lowerArrayToPointer t) | Parameter n t _ _ <- cParameters ]
      weProvides =
        [ if cName == "vkGetInstanceProcAddr"
          then Unguarded (Term cName)
          else Guarded (Guard staticGuard) $ Term cName
        , Unguarded (TypeAlias ("FN_" <> cName))
        , Unguarded (TypeAlias ("PFN_" <> cName))
        ]
      weDepends =
        (Unguarded <$> protoDepends)
          <> -- The constructors for an enum type need to be in scope
             [ Guarded (Guard staticGuard) (TypeName e)
             | TypeName n <- protoDepends
             , Just e <- [getEnumName n]
             ]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
  pure WriteElement {..}

commandDoc
  :: Text
  -> Command
  -> Either [SpecError] (DocMap -> Doc (), [Guarded Import], [Text])
commandDoc staticGuard c@Command {..} = do
  (t, (is, es)) <- toHsType (commandType c)
  let d getDoc = [qci|
  #if defined({staticGuard})
  {document getDoc (TopLevel cName)}
  foreign import ccall
  #if !defined(SAFE_FOREIGN_CALLS)
    unsafe
  #endif
    "{cName}" {cName} :: {t}
  {if cName == "vkGetInstanceProcAddr"
    then "#else" <> line <> pretty cName <+> "::" <+> t <> line <>
         pretty cName <+> "_" <+> "_" <+> "=" <+> "pure nullPtr"
    else mempty
  }
  #endif
  type FN_{cName} = {t}
  type PFN_{cName} = FunPtr FN_{cName}
|]
  let imports =
        [ Guarded (InvGuard staticGuard) (Import "Foreign.Ptr" ["nullPtr"])
        | cName == "vkGetInstanceProcAddr"
        ] ++ (Unguarded (Import "Foreign.Ptr" ["FunPtr"]) : is)
  pure (d, imports, es)
