{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Write.Type.Bitmask
  ( writeBitmaskType
  ) where

import           Data.Bits                     (shiftL)
import           Data.Maybe                    (fromMaybe)
import           Data.Word                     (Word32)
import           Spec.Bitmask
import           Spec.Type
import           Text.InterpolatedString.Perl6
import           Text.PrettyPrint.Leijen.Text  hiding ((<$>))
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

-- | This writes the bitmasks, and adds patterns for them if they require an
-- enumeration in 'bitmasks'.
writeBitmaskType :: BitmaskType -> Maybe Bitmask -> Write Doc
writeBitmaskType bmt Nothing = writeOpaqueBitmaskType bmt
writeBitmaskType bmt (Just bm) = writeBitmaskTypeWithBits bmt bm

writeOpaqueBitmaskType :: BitmaskType -> Write Doc
writeOpaqueBitmaskType bmt = do
  doesDeriveStorable
  bmtHsType <- cTypeToHsTypeString (bmtCType bmt)
  pure $ [qc|-- ** {bmtHsName bmt}
{predocComment "Opaque flag"}
newtype {bmtHsName bmt} = {bmtHsName bmt} {bmtHsType}
  deriving (Eq, Storable)
|]

writeBitmaskTypeWithBits :: BitmaskType -> Bitmask -> Write Doc
writeBitmaskTypeWithBits bmt bm = do
  bmtHsType <- cTypeToHsTypeString (bmtCType bmt)
  doesDeriveStorable
  tellExtension "GeneralizedNewtypeDeriving"
  tellExtension "PatternSynonyms"
  tellRequiredName (ExternalName (ModuleName "Data.Bits") "Bits")
  tellRequiredName (ExternalName (ModuleName "Data.Bits") "FiniteBits")
  tellRequiredNames
    (ExternalName (ModuleName "Text.Read") <$> [ "Read(..)"
                                               , "parens"
                                               ])
  tellRequiredName (ExternalName (ModuleName "Text.Read.Lex") "Lexeme(Ident)")
  tellRequiredNames
    (ExternalName (ModuleName "Text.ParserCombinators.ReadPrec") <$>
      [ "(+++)"
      , "prec"
      , "step"
      ])
  tellRequiredNames
    (ExternalName (ModuleName "GHC.Read") <$> [ "expectP"
                                              , "choose"
                                              ])
  pure [qc|-- ** {bmtName bmt}
{predocComment $ fromMaybe "" (bmComment bm)}
newtype {bmtHsName bmt} = {bmtHsName bmt} {bmtHsType}
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show {bmtHsName bmt} where
  {indent 0 $ vcat (writeBitPositionShowsPrec <$> bmBitPositions bm)}
  {indent 0 $ vcat (writeValueShowsPrec <$> bmValues bm)}
  showsPrec p ({bmtHsName bmt} x) = showParen (p >= 11) (showString "{bmtHsName bmt} " . showsPrec 11 x)

instance Read {bmtHsName bmt} where
  readPrec = parens ( choose [ {indent (-2) . vcat $ intercalatePrepend "," ((writeBitPositionReadTuple <$> bmBitPositions bm) ++ (writeValueReadTuple <$> bmValues bm))}
                             ] +++
                      prec 10 (do
                        expectP (Ident "{bmtHsName bmt}")
                        v <- step readPrec
                        pure ({bmtHsName bmt} v)
                        )
                    )

{vcat $ writeBitPosition bmt <$> bmBitPositions bm}
{vcat $ writeValue bmt <$> bmValues bm}
|]

writeValue :: BitmaskType -> BitmaskValue -> Doc
writeValue bmt v =
  [qc|{maybe "" predocComment (bmvComment v)}
pattern {bmvHsName v} = {bmtHsName bmt} {showHex' $ bmvValue v}|]

writeBitPosition :: BitmaskType -> BitmaskBitPosition -> Doc
writeBitPosition bmt bp =
  [qc|{maybe "" predocComment (bmbpComment bp)}
pattern {bmbpHsName bp} = {bmtHsName bmt} {showHex' $ (1 `shiftL` bmbpBitPos bp :: Word32)}|]

writeBitPositionShowsPrec :: BitmaskBitPosition -> Doc
writeBitPositionShowsPrec bp =
  [qc|showsPrec _ {bmbpHsName bp} = showString "{bmbpHsName bp}"|]

writeValueShowsPrec :: BitmaskValue -> Doc
writeValueShowsPrec v =
  [qc|showsPrec _ {bmvHsName v} = showString "{bmvHsName v}"|]

writeBitPositionReadTuple :: BitmaskBitPosition -> Doc
writeBitPositionReadTuple bp = [qc|("{bmbpHsName bp}", pure {bmbpHsName bp})|]

writeValueReadTuple :: BitmaskValue -> Doc
writeValueReadTuple v = [qc|("{bmvHsName v}", pure {bmvHsName v})|]
