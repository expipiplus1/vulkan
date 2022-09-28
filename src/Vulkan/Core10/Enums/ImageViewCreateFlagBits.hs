{-# language CPP #-}
-- No documentation found for Chapter "ImageViewCreateFlagBits"
module Vulkan.Core10.Enums.ImageViewCreateFlagBits  ( ImageViewCreateFlags
                                                    , ImageViewCreateFlagBits( IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT
                                                                             , IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
                                                                             , ..
                                                                             )
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type ImageViewCreateFlags = ImageViewCreateFlagBits

-- | VkImageViewCreateFlagBits - Bitmask specifying additional parameters of
-- an image view
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'ImageViewCreateFlags'
newtype ImageViewCreateFlagBits = ImageViewCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT' specifies that
-- the fragment density map will be read by the host during
-- 'Vulkan.Core10.CommandBuffer.endCommandBuffer' for the primary command
-- buffer that the render pass is recorded into
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT = ImageViewCreateFlagBits 0x00000002

-- | 'IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT' specifies that
-- the fragment density map will be read by device during
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT = ImageViewCreateFlagBits 0x00000001

conNameImageViewCreateFlagBits :: String
conNameImageViewCreateFlagBits = "ImageViewCreateFlagBits"

enumPrefixImageViewCreateFlagBits :: String
enumPrefixImageViewCreateFlagBits = "IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_D"

showTableImageViewCreateFlagBits :: [(ImageViewCreateFlagBits, String)]
showTableImageViewCreateFlagBits =
  [
    ( IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT
    , "EFERRED_BIT_EXT"
    )
  ,
    ( IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
    , "YNAMIC_BIT_EXT"
    )
  ]

instance Show ImageViewCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixImageViewCreateFlagBits
      showTableImageViewCreateFlagBits
      conNameImageViewCreateFlagBits
      (\(ImageViewCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ImageViewCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixImageViewCreateFlagBits
      showTableImageViewCreateFlagBits
      conNameImageViewCreateFlagBits
      ImageViewCreateFlagBits
