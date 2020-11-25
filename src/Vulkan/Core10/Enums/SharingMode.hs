{-# language CPP #-}
-- No documentation found for Chapter "SharingMode"
module Vulkan.Core10.Enums.SharingMode  (SharingMode( SHARING_MODE_EXCLUSIVE
                                                    , SHARING_MODE_CONCURRENT
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
-- | VkSharingMode - Buffer and image sharing modes
--
-- = Description
--
-- Note
--
-- 'SHARING_MODE_CONCURRENT' /may/ result in lower performance access to
-- the buffer or image than 'SHARING_MODE_EXCLUSIVE'.
--
-- Ranges of buffers and image subresources of image objects created using
-- 'SHARING_MODE_EXCLUSIVE' /must/ only be accessed by queues in the queue
-- family that has /ownership/ of the resource. Upon creation, such
-- resources are not owned by any queue family; ownership is implicitly
-- acquired upon first use within a queue. Once a resource using
-- 'SHARING_MODE_EXCLUSIVE' is owned by some queue family, the application
-- /must/ perform a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
-- to make the memory contents of a range or image subresource accessible
-- to a different queue family.
--
-- Note
--
-- Images still require a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-layouts layout transition>
-- from 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED' or
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED' before
-- being used on the first queue.
--
-- A queue family /can/ take ownership of an image subresource or buffer
-- range of a resource created with 'SHARING_MODE_EXCLUSIVE', without an
-- ownership transfer, in the same way as for a resource that was just
-- created; however, taking ownership in this way has the effect that the
-- contents of the image subresource or buffer range are undefined.
--
-- Ranges of buffers and image subresources of image objects created using
-- 'SHARING_MODE_CONCURRENT' /must/ only be accessed by queues from the
-- queue families specified through the @queueFamilyIndexCount@ and
-- @pQueueFamilyIndices@ members of the corresponding create info
-- structures.
--
-- = See Also
--
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
newtype SharingMode = SharingMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SHARING_MODE_EXCLUSIVE' specifies that access to any range or image
-- subresource of the object will be exclusive to a single queue family at
-- a time.
pattern SHARING_MODE_EXCLUSIVE = SharingMode 0
-- | 'SHARING_MODE_CONCURRENT' specifies that concurrent access to any range
-- or image subresource of the object from multiple queue families is
-- supported.
pattern SHARING_MODE_CONCURRENT = SharingMode 1
{-# complete SHARING_MODE_EXCLUSIVE,
             SHARING_MODE_CONCURRENT :: SharingMode #-}

instance Show SharingMode where
  showsPrec p = \case
    SHARING_MODE_EXCLUSIVE -> showString "SHARING_MODE_EXCLUSIVE"
    SHARING_MODE_CONCURRENT -> showString "SHARING_MODE_CONCURRENT"
    SharingMode x -> showParen (p >= 11) (showString "SharingMode " . showsPrec 11 x)

instance Read SharingMode where
  readPrec = parens (choose [("SHARING_MODE_EXCLUSIVE", pure SHARING_MODE_EXCLUSIVE)
                            , ("SHARING_MODE_CONCURRENT", pure SHARING_MODE_CONCURRENT)]
                     +++
                     prec 10 (do
                       expectP (Ident "SharingMode")
                       v <- step readPrec
                       pure (SharingMode v)))

