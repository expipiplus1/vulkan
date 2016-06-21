{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Write.Type.Bitmask
  ( writeBitmaskType
  , writeExtensionBitmask
  ) where

import           Data.Bits                     (shiftL)
import           Data.Maybe                    (fromMaybe)
import           Data.Word                     (Word32)
import           Spec.Bitmask
import           Spec.Extension
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
  tellExtension "GeneralizedNewtypeDeriving"
  tellRequiredName (ExternalName (ModuleName "Data.Bits") "Bits")
  tellRequiredName (ExternalName (ModuleName "Data.Bits") "FiniteBits")
  bmtHsType <- cTypeToHsTypeString (bmtCType bmt)
  pure $ [qc|-- ** {bmtName bmt}
{predocComment "Opaque flag"}
newtype {bmtName bmt} = {bmtName bmt} {bmtHsType}
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Show)
|]

writeBitmaskTypeWithBits :: BitmaskType -> Bitmask -> Write Doc
writeBitmaskTypeWithBits bmt bm = do
  bmtHsType <- cTypeToHsTypeString (bmtCType bmt)
  doesDeriveStorable
  tellExtension "GeneralizedNewtypeDeriving"
  tellExtension "PatternSynonyms"
  tellExtension "ScopedTypeVariables"
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
newtype {bmName bm} = {bmName bm} {bmtHsType}
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

-- | Alias for {bmName bm}
type {bmtName bmt} = {bmName bm}

instance Show {bmName bm} where
  {indent 0 $ vcat (writeBitPositionShowsPrec <$> bmBitPositions bm)}
  {indent 0 $ vcat (writeValueShowsPrec <$> bmValues bm)}
  showsPrec p ({bmName bm} x) = showParen (p >= 11) (showString "{bmName bm} " . showsPrec 11 x)

instance Read {bmName bm} where
  readPrec = parens ( choose [ {indent (-2) . vcat $ intercalatePrepend "," ((writeBitPositionReadTuple <$> bmBitPositions bm) ++ (writeValueReadTuple <$> bmValues bm))}
                             ] +++
                      prec 10 (do
                        expectP (Ident "{bmName bm}")
                        v <- step readPrec
                        pure ({bmName bm} v)
                        )
                    )

{vcat $ writeBitPosition bm <$> bmBitPositions bm}
{vcat $ writeValue bm <$> bmValues bm}
|]

writeValue :: Bitmask -> BitmaskValue -> Doc
writeValue bm v =
  [qc|{maybe "" predocComment (bmvComment v)}
pattern {bmvName v} = {bmName bm} {showHex' $ bmvValue v}|]

writeBitPosition :: Bitmask -> BitmaskBitPosition -> Doc
writeBitPosition bm bp =
  [qc|{maybe "" predocComment (bmbpComment bp)}
pattern {bmbpName bp} = {bmName bm} {showHex' $ (1 `shiftL` bmbpBitPos bp :: Word32)}|]

writeBitPositionShowsPrec :: BitmaskBitPosition -> Doc
writeBitPositionShowsPrec bp =
  [qc|showsPrec _ {bmbpName bp} = showString "{bmbpName bp}"|]

writeValueShowsPrec :: BitmaskValue -> Doc
writeValueShowsPrec v =
  [qc|showsPrec _ {bmvName v} = showString "{bmvName v}"|]

writeBitPositionReadTuple :: BitmaskBitPosition -> Doc
writeBitPositionReadTuple bp = [qc|("{bmbpName bp}", pure {bmbpName bp})|]

writeValueReadTuple :: BitmaskValue -> Doc
writeValueReadTuple v = [qc|("{bmvName v}", pure {bmvName v})|]

writeExtensionBitmask :: ExtensionBitmask -> Write Doc
writeExtensionBitmask bm = do
  tellExtension "PatternSynonyms"
  tellExtension "ScopedTypeVariables"
  pure [qc|pattern {ebmName bm} = {fromMaybe "" $ ebmExtends bm} {showHex' $ (1 `shiftL` ebmBitpos bm :: Word32)}|]
