{-# language CPP #-}
-- No documentation found for Chapter "Handles"
module Vulkan.Core13.Handles  ( PrivateDataSlot(..)
                              , PhysicalDevice(..)
                              , Device(..)
                              , Queue(..)
                              , CommandBuffer(..)
                              , Buffer(..)
                              , Image(..)
                              , ImageView(..)
                              , Fence(..)
                              , Semaphore(..)
                              , Event(..)
                              , QueryPool(..)
                              ) where

import GHC.Show (showParen)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Word (Word64)
import Vulkan.Core10.APIConstants (HasObjectType(..))
import Vulkan.Core10.APIConstants (IsHandle)
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PRIVATE_DATA_SLOT))
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Event(..))
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Semaphore(..))
-- | VkPrivateDataSlot - Opaque handle to a private data slot object
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_private_data VK_EXT_private_data>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core13.Promoted_From_VK_EXT_private_data.createPrivateDataSlot',
-- 'Vulkan.Extensions.VK_EXT_private_data.createPrivateDataSlotEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_private_data.destroyPrivateDataSlot',
-- 'Vulkan.Extensions.VK_EXT_private_data.destroyPrivateDataSlotEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_private_data.getPrivateData',
-- 'Vulkan.Extensions.VK_EXT_private_data.getPrivateDataEXT',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_private_data.setPrivateData',
-- 'Vulkan.Extensions.VK_EXT_private_data.setPrivateDataEXT'
newtype PrivateDataSlot = PrivateDataSlot Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PrivateDataSlot where
  objectTypeAndHandle (PrivateDataSlot h) = (OBJECT_TYPE_PRIVATE_DATA_SLOT, h)
instance Show PrivateDataSlot where
  showsPrec p (PrivateDataSlot x) = showParen (p >= 11) (showString "PrivateDataSlot 0x" . showHex x)

