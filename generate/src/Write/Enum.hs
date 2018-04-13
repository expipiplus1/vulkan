{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Enum
  ( writeEnum
  ) where

import           Data.Maybe
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Enum
import           Write.Element
import           Write.Util

writeEnum :: Enum -> WriteElement
writeEnum e =
  let weDoc        = enumDoc e
      weExtensions = ["GeneralizedNewtypeDeriving", "PatternSynonyms"]
      weImports =
        [ Import "Data.Int"                        ["Int32"]
        , Import "Foreign.Storable"                ["Storable(..)"]
        , Import "Text.Read"                       ["Read(..)", "parens"]
        , Import "Text.Read.Lex"                   ["Lexeme(Ident)"]
        , Import "Text.ParserCombinators.ReadPrec" ["(+++)", "prec", "step"]
        , Import "GHC.Read"                        ["expectP", "choose"]
        ]
  in  WriteElement {..}



enumDoc :: Enum -> Doc ()
enumDoc e@Enum{..} = [qci|
  -- ** {eName}

  -- | {fromMaybe "" (eComment)}
  newtype {eName} = {eName} Int32
    deriving (Eq, Ord, Storable)

  instance Show {eName} where
  {indent 2 $ vcat (writeElementShowsPrec <$> eElements)}
    showsPrec p ({eName} x) = showParen (p >= 11) (showString "{eName} " . showsPrec 11 x)

  instance Read {eName} where
    readPrec = parens ( choose [ {indent (-2) . vcat $ intercalatePrepend "," (writeReadTuple <$> eElements)}
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
  pattern {eeName} = {eName} {showsPrec 10 (eeValue) ""}
|]

writeElementShowsPrec :: EnumElement -> Doc ()
writeElementShowsPrec EnumElement{..} = [qci|
  showsPrec _ {eeName} = showString "{eeName}"
|]

writeReadTuple :: EnumElement -> Doc ()
writeReadTuple EnumElement{..} = [qci|("{eeName}", pure {eeName})|]
