{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Type.Enum
  ( writeEnum
  ) where

import           Control.Monad
import           Data.Either.Validation
import           Data.Int
import qualified Data.Map                                 as Map
import           Data.Maybe
import qualified Data.MultiMap                            as MultiMap
import qualified Data.Set                                 as Set
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Data.Text.Prettyprint.Doc
import           Data.Traversable
import           Data.Word
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented
import           Text.Printf

import           Spec.Savvy.Enum

import           Write.Element
import           Write.Error
import           Write.Util

writeEnum :: Enum -> WriteElement
writeEnum e@Enum {..} =
  let weName       = "Enum: " <> eName
      weDoc        = enumDoc e
      weExtensions = ["GeneralizedNewtypeDeriving", "PatternSynonyms"]
      weImports =
        [ Import "Foreign.Storable"                ["Storable(..)"]
          , Import "Text.Read"                       ["Read(..)", "parens"]
          , Import "Text.Read.Lex"                   ["Lexeme(Ident)"]
          , Import "Text.ParserCombinators.ReadPrec" ["(+++)", "prec", "step"]
          , Import "GHC.Read"                        ["expectP", "choose"]
          ]
          ++ case eType of
               EnumTypeEnum -> [Import "Data.Int" ["Int32"]]
               EnumTypeBitmask ->
                 [ Import "Data.Word" ["Word32"]
                 , Import "Data.Bits" ["Bits", "FiniteBits"]
                 ]

      weProvides =
        [Type eName, Term eName]
          ++ [ Pattern eeName | EnumElement {..} <- eElements ]
      weDepends = []
  in  WriteElement {..}

enumDoc :: Enum -> Doc ()
enumDoc e@Enum{..} = [qci|
  -- ** {eName}

  -- | {fromMaybe "" (eComment)}
  newtype {eName} = {eName} {enumBackingType e}
    deriving ({hcat $ intercalatePrepend "," (enumDerivedClasses e)})

  instance Show {eName} where
    {indent 0 . vcat $
      (writeElementShowsPrec <$> eElements) ++
      ["-- The following values are from extensions, the patterns \\
        \\themselves are exported from the extension modules"
      | not (null eExtensions)
      ] ++
      (writeExtensionElementShowsPrec eName <$> eExtensions)
    }
    showsPrec p ({eName} x) = showParen (p >= 11) (showString "{eName} " . showsPrec 11 x)

  instance Read {eName} where
    readPrec = parens ( choose [ {indent (-2) . vcat $
                                   (intercalatePrepend "," (writeReadTuples eElements)) ++
                                   ["-- The following values are from extensions, the patterns \\
                                     \\themselves are exported from the extension modules"
                                   | not (null eExtensions)
                                   ] ++
                                   -- If we have no preceding elements, skip the first ","
                                   -- Otherwise prepend a "," on every line
                                   ((if null eElements then intercalatePrepend "," else (fmap ("," <+>)))
                                      (writeReadExtensionTuples eName eExtensions))
                                 }
                               ] +++
                        prec 10 (do
                          expectP (Ident "{eName}")
                          v <- step readPrec
                          pure ({eName} v)
                          )
                      )

  {emptyLineSep $ writeElement e <$> eElements}
|]

writeElement :: Enum -> EnumElement -> Doc ()
writeElement Enum{..} EnumElement{..} = [qci|
  -- | {fromMaybe "" eeComment}
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

writeAlias
  :: Text
  -- ^ Enum name
  -> Text
  -- ^ Alias
  -> Doc ()
writeAlias name alias = [qci|type {alias} = {name}|]

writeValue :: Either Int32 Word32 -> String
writeValue = \case
  Left i -> showsPrec 10 i ""
  Right i -> printf "0x%08x" i

enumBackingType :: Enum -> Doc ()
enumBackingType Enum {..} = case eType of
  EnumTypeEnum    -> "Int32"
  EnumTypeBitmask -> "Word32"

enumDerivedClasses :: Enum -> [Doc ()]
enumDerivedClasses Enum{..} = case eType of
  EnumTypeEnum    ->["Eq", "Ord", "Storable"]
  EnumTypeBitmask -> ["Eq","Ord","Storable", "Bits", "FiniteBits"]
