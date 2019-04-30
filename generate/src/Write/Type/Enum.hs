{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Type.Enum
  ( writeEnum
  ) where

import           Data.Int
import           Data.List.Extra
import           Data.Maybe
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc
import           Data.Word
import           Data.Foldable
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented
import           Text.Printf
import           Control.Monad.Trans
import           Control.Monad

import           Spec.Savvy.Enum

import           Write.Element
import           Write.Util
import           Write.Monad

writeEnum :: Enum -> Write WriteElement
writeEnum e@Enum {..} = runWE ("Enum:" T.<+> eName) $ do
  tellExtension "GeneralizedNewtypeDeriving"
  tellExtension "PatternSynonyms"
  tellImports
    [ Import "Foreign.Storable"                ["Storable(..)"]
    , Import "Text.Read"                       ["Read(..)", "parens"]
    , Import "Text.Read.Lex"                   ["Lexeme(Ident)"]
    , Import "Text.ParserCombinators.ReadPrec" ["(+++)", "prec", "step"]
    , Import "GHC.Read"                        ["expectP", "choose"]
    ]
  tellDepend (TypeName "Zero")
  case eType of
    EnumTypeEnum    -> tellImport "Data.Int" "Int32"
    EnumTypeBitmask -> do
      tellImport "Data.Bits" "Bits"
      tellImport "Data.Bits" "FiniteBits"
      tellDepend (TypeName "VkFlags")
  traverse_
    tellExport
    (  [TypeConstructor eName, Term eName]
    ++ [ Pattern eeName | EnumElement {..} <- eElements ]
    )
  tellBootElem <=< liftWrite . runWE ("Enum: " <> eName <> " boot") $ do
    tellExport (TypeConstructor eName)
    pure $ \_ -> pretty $ "data" T.<+> eName
  pure $ enumDoc e

enumDoc :: Enum -> DocMap -> Doc ()
enumDoc e@Enum{..} getDoc = [qci|
  -- ** {eName}

  {document getDoc (TopLevel eName)}
  newtype {eName} = {eName} {enumBackingType e}
    deriving ({hcat $ intercalatePrepend "," (enumDerivedClasses e)})

  instance Show {eName} where
    {indent 0 . vcat $
      (writeElementShowsPrec <$> eElements) ++
      ["-- The following values are from extensions, the patterns \\
        \\themselves are exported from the extension modules"
      | not (null eExtensions)
      ] ++
      (writeExtensionElementShowsPrec eName <$> (nubOrdOn exValue eExtensions))
    }
    showsPrec p ({eName} x) = showParen (p >= 11) (showString "{eName} " . showsPrec 11 x)

  instance Read {eName} where
    readPrec = parens ( choose [ {indent (-2) $ separatedSections ","
                                    [ (Nothing, writeReadTuples eElements)
                                    , (Just "-- The following values are from extensions, the patterns themselves are exported from the extension modules"
                                      , writeReadExtensionTuples eName eExtensions)
                                    ]
                                 }
                               ] +++
                        prec 10 (do
                          expectP (Ident "{eName}")
                          v <- step readPrec
                          pure ({eName} v)
                          )
                      )

  {emptyLineSep $ writeElement getDoc e <$> eElements}
|]

writeElement :: DocMap -> Enum -> EnumElement -> Doc ()
writeElement getDoc Enum{..} EnumElement{..} = [qci|
  {document getDoc (Nested eName eeName)}
  pattern {eeName} :: {eName}
  pattern {eeName} = {eName} {writeValue eeValue}
|]

writeElementShowsPrec :: EnumElement -> Doc ()
writeElementShowsPrec EnumElement{..} = [qci|
  showsPrec _ {eeName} = showString "{eeName}"
|]

writeExtensionElementShowsPrec :: Text -> EnumExtension -> Doc ()
writeExtensionElementShowsPrec eName EnumExtension{..} = [qci|
  showsPrec _ ({eName} {writeValue exValue}) = showString "{exName}"
|]

writeReadTuples :: [EnumElement] -> [Doc ()]
writeReadTuples ees =
  let maxNameLength = maximum (0:[T.length eeName | EnumElement{..} <- ees])
      spaces n = T.replicate (maxNameLength - T.length n) " "
  in [[qci|("{eeName}", {spaces eeName}pure {eeName})|] | EnumElement{..} <- ees]

writeReadExtensionTuples :: Text -> [EnumExtension] -> [Doc ()]
writeReadExtensionTuples eName ees =
  let maxNameLength = maximum (0:[T.length exName | EnumExtension{..} <- ees])
      spaces n = T.replicate (maxNameLength - T.length n) " "
  in [[qci|("{exName}", {spaces exName}pure ({eName} {writeValue exValue}))|] | EnumExtension{..} <- ees]

writeValue :: Either Int32 Word32 -> String
writeValue = \case
  Left i -> showsPrec 10 i ""
  Right i -> printf "0x%08x" i

enumBackingType :: Enum -> Doc ()
enumBackingType Enum {..} = case eType of
  EnumTypeEnum    -> "Int32"
  EnumTypeBitmask -> "VkFlags"

enumDerivedClasses :: Enum -> [Doc ()]
enumDerivedClasses Enum{..} = case eType of
  EnumTypeEnum    ->["Eq", "Ord", "Storable", "Zero"]
  EnumTypeBitmask -> ["Eq","Ord","Storable", "Bits", "FiniteBits", "Zero"]
