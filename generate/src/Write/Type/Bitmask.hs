{-# LANGUAGE QuasiQuotes #-}

module Write.Type.Bitmask
  ( writeBitmaskTypes
  ) where

import Data.Word(Word32)
import Control.Arrow((&&&))
import Data.Maybe(fromMaybe)
import Data.Bits(shiftL)
import Spec.Bitmask
import Spec.Type
import Text.InterpolatedString.Perl6
import Text.PrettyPrint.Leijen.Text hiding ((<$>))
import Write.Utils
import Write.TypeConverter

-- | This writes the bitmasks, and adds patterns for them if they require an
-- enumeration in 'bitmasks'.
writeBitmaskTypes :: TypeConverter -> [Bitmask] -> [BitmaskType] -> String
writeBitmaskTypes tc bms bmts = let as = associateBitmasks bms bmts
                                in [qc|-- * Flags

{vcat $ writeBitmaskType tc <$> as}|] 

writeBitmaskType :: TypeConverter -> (BitmaskType, Maybe Bitmask) -> Doc
writeBitmaskType tc (bmt, Nothing) = writeOpaqueBitmaskType tc bmt
writeBitmaskType tc (bmt, Just bm) = writeBitmaskTypeWithBits tc bmt bm

writeOpaqueBitmaskType :: TypeConverter -> BitmaskType -> Doc
writeOpaqueBitmaskType tc bmt = [qc|-- ** {bmtName bmt}
{predocComment "Opaque flag"}
newtype {bmtName bmt} = {bmtName bmt} {tc (bmtCType bmt)}
  deriving (Eq, Storable)
|]

writeBitmaskTypeWithBits :: TypeConverter -> BitmaskType -> Bitmask -> Doc
writeBitmaskTypeWithBits tc bmt bm = [qc|-- ** {bmtName bmt}
{predocComment $ fromMaybe "" (bmComment bm)}
newtype {bmName bm} = {bmName bm} {tc (bmtCType bmt)}
  deriving (Eq, Storable, Bits, FiniteBits)
-- | Alias for {bmName bm}
type {bmtName bmt} = {bmName bm}
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

----------------------------------------
-- * Helpers
----------------------------------------

associateBitmasks :: [Bitmask] -> [BitmaskType] 
                  -> [(BitmaskType, Maybe Bitmask)]
associateBitmasks bms bmts = (id &&& getBitmaskForType) <$> bmts
  where 
    getBitmaskForType bt = getBitmaskNamed <$> bmtRequires bt 
    getBitmaskNamed n = case filter ((==n) . bmName) $ bms of 
                          [] -> error ("required bitmask missing for " ++ n)
                          [b] -> b
                          _ -> error ("too many bitmasks found for" ++ n)
