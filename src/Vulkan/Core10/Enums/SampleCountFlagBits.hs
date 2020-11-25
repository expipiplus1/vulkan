{-# language CPP #-}
-- No documentation found for Chapter "SampleCountFlagBits"
module Vulkan.Core10.Enums.SampleCountFlagBits  ( SampleCountFlags
                                                , SampleCountFlagBits( SAMPLE_COUNT_1_BIT
                                                                     , SAMPLE_COUNT_2_BIT
                                                                     , SAMPLE_COUNT_4_BIT
                                                                     , SAMPLE_COUNT_8_BIT
                                                                     , SAMPLE_COUNT_16_BIT
                                                                     , SAMPLE_COUNT_32_BIT
                                                                     , SAMPLE_COUNT_64_BIT
                                                                     , ..
                                                                     )
                                                ) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type SampleCountFlags = SampleCountFlagBits

-- | VkSampleCountFlagBits - Bitmask specifying sample counts supported for
-- an image used for storage operations
--
-- = See Also
--
-- 'Vulkan.Core10.Pass.AttachmentDescription',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentDescription2',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.FramebufferMixedSamplesCombinationNV',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PhysicalDeviceFragmentShadingRateEnumsPropertiesNV',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRatePropertiesKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceSparseImageFormatInfo2',
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo',
-- 'SampleCountFlags',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.getPhysicalDeviceSparseImageFormatProperties'
newtype SampleCountFlagBits = SampleCountFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SAMPLE_COUNT_1_BIT' specifies an image with one sample per pixel.
pattern SAMPLE_COUNT_1_BIT  = SampleCountFlagBits 0x00000001
-- | 'SAMPLE_COUNT_2_BIT' specifies an image with 2 samples per pixel.
pattern SAMPLE_COUNT_2_BIT  = SampleCountFlagBits 0x00000002
-- | 'SAMPLE_COUNT_4_BIT' specifies an image with 4 samples per pixel.
pattern SAMPLE_COUNT_4_BIT  = SampleCountFlagBits 0x00000004
-- | 'SAMPLE_COUNT_8_BIT' specifies an image with 8 samples per pixel.
pattern SAMPLE_COUNT_8_BIT  = SampleCountFlagBits 0x00000008
-- | 'SAMPLE_COUNT_16_BIT' specifies an image with 16 samples per pixel.
pattern SAMPLE_COUNT_16_BIT = SampleCountFlagBits 0x00000010
-- | 'SAMPLE_COUNT_32_BIT' specifies an image with 32 samples per pixel.
pattern SAMPLE_COUNT_32_BIT = SampleCountFlagBits 0x00000020
-- | 'SAMPLE_COUNT_64_BIT' specifies an image with 64 samples per pixel.
pattern SAMPLE_COUNT_64_BIT = SampleCountFlagBits 0x00000040

conNameSampleCountFlagBits :: String
conNameSampleCountFlagBits = "SampleCountFlagBits"

enumPrefixSampleCountFlagBits :: String
enumPrefixSampleCountFlagBits = "SAMPLE_COUNT_"

showTableSampleCountFlagBits :: [(SampleCountFlagBits, String)]
showTableSampleCountFlagBits =
  [ (SAMPLE_COUNT_1_BIT , "1_BIT")
  , (SAMPLE_COUNT_2_BIT , "2_BIT")
  , (SAMPLE_COUNT_4_BIT , "4_BIT")
  , (SAMPLE_COUNT_8_BIT , "8_BIT")
  , (SAMPLE_COUNT_16_BIT, "16_BIT")
  , (SAMPLE_COUNT_32_BIT, "32_BIT")
  , (SAMPLE_COUNT_64_BIT, "64_BIT")
  ]

instance Show SampleCountFlagBits where
  showsPrec p e = case lookup e showTableSampleCountFlagBits of
    Just s -> showString enumPrefixSampleCountFlagBits . showString s
    Nothing ->
      let SampleCountFlagBits x = e
      in  showParen (p >= 11) (showString conNameSampleCountFlagBits . showString " 0x" . showHex x)

instance Read SampleCountFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixSampleCountFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableSampleCountFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameSampleCountFlagBits)
            v <- step readPrec
            pure (SampleCountFlagBits v)
          )
    )

