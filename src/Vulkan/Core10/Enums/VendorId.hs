{-# language CPP #-}
module Vulkan.Core10.Enums.VendorId  (VendorId( VENDOR_ID_VIV
                                              , VENDOR_ID_VSI
                                              , VENDOR_ID_KAZAN
                                              , VENDOR_ID_CODEPLAY
                                              , ..
                                              )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkVendorId - Khronos vendor IDs
--
-- = Description
--
-- Note
--
-- Khronos vendor IDs may be allocated by vendors at any time. Only the
-- latest canonical versions of this Specification, of the corresponding
-- @vk.xml@ API Registry, and of the corresponding @vulkan_core.h@ header
-- file /must/ contain all reserved Khronos vendor IDs.
--
-- Only Khronos vendor IDs are given symbolic names at present. PCI vendor
-- IDs returned by the implementation can be looked up in the PCI-SIG
-- database.
--
-- = See Also
--
-- No cross-references are available
newtype VendorId = VendorId Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkVendorId" "VK_VENDOR_ID_VIV"
pattern VENDOR_ID_VIV = VendorId 65537
-- No documentation found for Nested "VkVendorId" "VK_VENDOR_ID_VSI"
pattern VENDOR_ID_VSI = VendorId 65538
-- No documentation found for Nested "VkVendorId" "VK_VENDOR_ID_KAZAN"
pattern VENDOR_ID_KAZAN = VendorId 65539
-- No documentation found for Nested "VkVendorId" "VK_VENDOR_ID_CODEPLAY"
pattern VENDOR_ID_CODEPLAY = VendorId 65540
{-# complete VENDOR_ID_VIV,
             VENDOR_ID_VSI,
             VENDOR_ID_KAZAN,
             VENDOR_ID_CODEPLAY :: VendorId #-}

instance Show VendorId where
  showsPrec p = \case
    VENDOR_ID_VIV -> showString "VENDOR_ID_VIV"
    VENDOR_ID_VSI -> showString "VENDOR_ID_VSI"
    VENDOR_ID_KAZAN -> showString "VENDOR_ID_KAZAN"
    VENDOR_ID_CODEPLAY -> showString "VENDOR_ID_CODEPLAY"
    VendorId x -> showParen (p >= 11) (showString "VendorId " . showsPrec 11 x)

instance Read VendorId where
  readPrec = parens (choose [("VENDOR_ID_VIV", pure VENDOR_ID_VIV)
                            , ("VENDOR_ID_VSI", pure VENDOR_ID_VSI)
                            , ("VENDOR_ID_KAZAN", pure VENDOR_ID_KAZAN)
                            , ("VENDOR_ID_CODEPLAY", pure VENDOR_ID_CODEPLAY)]
                     +++
                     prec 10 (do
                       expectP (Ident "VendorId")
                       v <- step readPrec
                       pure (VendorId v)))

