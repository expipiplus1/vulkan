{-# language CPP #-}
-- No documentation found for Chapter "HostImageCopyFlagBits"
module Vulkan.Core14.Enums.HostImageCopyFlagBits  ( pattern HOST_IMAGE_COPY_MEMCPY
                                                  , HostImageCopyFlags
                                                  , HostImageCopyFlagBits( HOST_IMAGE_COPY_MEMCPY_BIT
                                                                         , ..
                                                                         )
                                                  ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
-- No documentation found for TopLevel "VK_HOST_IMAGE_COPY_MEMCPY"
pattern HOST_IMAGE_COPY_MEMCPY = HOST_IMAGE_COPY_MEMCPY_BIT


type HostImageCopyFlags = HostImageCopyFlagBits

-- | VkHostImageCopyFlagBits - Bitmask specifying additional copy parameters
--
-- = Description
--
-- -   'HOST_IMAGE_COPY_MEMCPY_BIT' specifies that no memory layout
--     swizzling is to be applied during data copy. For copies between
--     memory and images, this flag indicates that image data in host
--     memory is swizzled in exactly the same way as the image data on the
--     device. Using this flag indicates that the implementations /may/ use
--     a simple memory copy to transfer the data between the host memory
--     and the device memory. The format of the swizzled data in host
--     memory is platform dependent and is not defined in this
--     specification.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'HostImageCopyFlags'
newtype HostImageCopyFlagBits = HostImageCopyFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkHostImageCopyFlagBits" "VK_HOST_IMAGE_COPY_MEMCPY_BIT"
pattern HOST_IMAGE_COPY_MEMCPY_BIT = HostImageCopyFlagBits 0x00000001

conNameHostImageCopyFlagBits :: String
conNameHostImageCopyFlagBits = "HostImageCopyFlagBits"

enumPrefixHostImageCopyFlagBits :: String
enumPrefixHostImageCopyFlagBits = "HOST_IMAGE_COPY_MEMCPY_BIT"

showTableHostImageCopyFlagBits :: [(HostImageCopyFlagBits, String)]
showTableHostImageCopyFlagBits = [(HOST_IMAGE_COPY_MEMCPY_BIT, "")]

instance Show HostImageCopyFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixHostImageCopyFlagBits
      showTableHostImageCopyFlagBits
      conNameHostImageCopyFlagBits
      (\(HostImageCopyFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read HostImageCopyFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixHostImageCopyFlagBits
      showTableHostImageCopyFlagBits
      conNameHostImageCopyFlagBits
      HostImageCopyFlagBits
