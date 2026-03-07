{-# language CPP #-}
-- No documentation found for Chapter "SharingMode"
module Vulkan.Core10.Enums.SharingMode  (SharingMode( SHARING_MODE_EXCLUSIVE
                                                    , SHARING_MODE_CONCURRENT
                                                    , ..
                                                    )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkSharingMode - Buffer and image sharing modes
--
-- = Description
--
-- -   'SHARING_MODE_EXCLUSIVE' specifies that access to any range or image
--     subresource of the object will be exclusive to a single queue family
--     at a time.
--
-- -   'SHARING_MODE_CONCURRENT' specifies that concurrent access to any
--     range or image subresource of the object from multiple queue
--     families is supported.
--
-- 'SHARING_MODE_CONCURRENT' /may/ result in lower performance access to
-- the buffer or image than 'SHARING_MODE_EXCLUSIVE'.
--
-- Ranges of buffers and image subresources of image objects created using
-- 'SHARING_MODE_EXCLUSIVE' /must/ only be accessed by queues in the queue
-- family that has /ownership/ of the resource. Upon creation, such
-- resources are not owned by any queue family; ownership is implicitly
-- acquired upon first use within a queue. Once a resource using
-- 'SHARING_MODE_EXCLUSIVE' is owned by some queue family, unless the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
-- feature is enabled, the application /must/ perform a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
-- if it wishes to make the memory contents of a range or image subresource
-- accessible to a different queue family. 'SHARING_MODE_EXCLUSIVE'
-- resources that are already owned by a queue family /may/ be acquired by
-- a different queue family without a queue family ownership transfer, but
-- unless the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
-- feature is enabled, their contents become undefined.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
-- feature is enabled, the contents of buffer resources, and of linear
-- image resources (i.e., those created with @tiling@ set to
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR') are always
-- preserved when they are implicitly acquired by a different queue family
-- on the same logical device (i.e., neither queue family is
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT' or
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL'). This means that
-- whenever the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
-- feature is enabled, explicit queue family ownership transfers of buffer
-- and linear image resources between different queue families on the same
-- logical device are /optional/.
--
-- Additionally, if the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
-- feature is enabled, the contents of some optimal image resources (i.e.,
-- those created with
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL') are always
-- preserved when they are implicitly acquired by a different queue family
-- on the same logical device (i.e., neither queue family is
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_FOREIGN_EXT' or
-- 'Vulkan.Core10.APIConstants.QUEUE_FAMILY_EXTERNAL'). This applies only
-- to optimal images that are being implicitly acquired by a queue family
-- whose index bit is set in the current queue family’s
-- 'Vulkan.Extensions.VK_KHR_maintenance9.QueueFamilyOwnershipTransferPropertiesKHR'::@optimalImageTransferToQueueFamilies@,
-- and that were created without any of the following bits set in @usage@:
--
-- -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
--
-- -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
-- -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- This means that whenever the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
-- feature is enabled, explicit queue family ownership transfers of such
-- image resources between such combinations of queue families are
-- /optional/. For all other optimal images and\/or combinations of queue
-- families, the application /must/ still perform an explicit queue family
-- ownership transfer if it wishes to make the memory contents of an
-- optimal image subresource already owned by a queue family accessible to
-- a different queue family.
--
-- Applications are allowed to perform explicit queue family ownership
-- transfers in circumstances where they are not required, but there is no
-- functional nor performance advantage in doing so. Performing explicit
-- transfers in such cases remains supported for backward compatibility and
-- is not recommended for new applications.
--
-- Before being used on the first queue, images still require a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-image-layouts layout transition>
-- from these layouts:
--
-- -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_UNDEFINED'
--
-- -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ZERO_INITIALIZED_EXT'
--
-- A queue family /can/ take ownership of an image subresource, tensor
-- subresource, or buffer range of a resource created with
-- 'SHARING_MODE_EXCLUSIVE', without an ownership transfer, in the same way
-- as for a resource that was just created; however, taking ownership in
-- this way has the effect that the contents of the image subresource or
-- buffer range are undefined.
--
-- Ranges of buffers, tensor subresources of tensor objects, and image
-- subresources of image objects created using 'SHARING_MODE_CONCURRENT'
-- /must/ only be accessed by queues from the queue families specified
-- through the @queueFamilyIndexCount@ and @pQueueFamilyIndices@ members of
-- the corresponding create info structures.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM'
newtype SharingMode = SharingMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSharingMode" "VK_SHARING_MODE_EXCLUSIVE"
pattern SHARING_MODE_EXCLUSIVE = SharingMode 0

-- No documentation found for Nested "VkSharingMode" "VK_SHARING_MODE_CONCURRENT"
pattern SHARING_MODE_CONCURRENT = SharingMode 1

{-# COMPLETE
  SHARING_MODE_EXCLUSIVE
  , SHARING_MODE_CONCURRENT ::
    SharingMode
  #-}

conNameSharingMode :: String
conNameSharingMode = "SharingMode"

enumPrefixSharingMode :: String
enumPrefixSharingMode = "SHARING_MODE_"

showTableSharingMode :: [(SharingMode, String)]
showTableSharingMode =
  [ (SHARING_MODE_EXCLUSIVE, "EXCLUSIVE")
  , (SHARING_MODE_CONCURRENT, "CONCURRENT")
  ]

instance Show SharingMode where
  showsPrec =
    enumShowsPrec
      enumPrefixSharingMode
      showTableSharingMode
      conNameSharingMode
      (\(SharingMode x) -> x)
      (showsPrec 11)

instance Read SharingMode where
  readPrec =
    enumReadPrec
      enumPrefixSharingMode
      showTableSharingMode
      conNameSharingMode
      SharingMode
