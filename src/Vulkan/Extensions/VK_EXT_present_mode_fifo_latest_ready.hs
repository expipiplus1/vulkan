{-# language CPP #-}
-- | = Name
--
-- VK_EXT_present_mode_fifo_latest_ready - device extension
--
-- = VK_EXT_present_mode_fifo_latest_ready
--
-- [__Name String__]
--     @VK_EXT_present_mode_fifo_latest_ready@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     362
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--
-- [__Contact__]
--
--     -   Lionel Duc
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_present_mode_fifo_latest_ready] @nvlduc%0A*Here describe the issue or question you have about the VK_EXT_present_mode_fifo_latest_ready extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_present_mode_fifo_latest_ready.adoc VK_EXT_present_mode_fifo_latest_ready>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-05-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
-- == Description
--
-- This device extension adds a new present mode,
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_EXT'.
--
-- This tear-free present mode behaves much like
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR', except that
-- each vertical blanking period dequeues consecutive present requests
-- until the latest ready is found to update the current image.
--
-- While this seems similar in concept to
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR', the
-- fundamental difference is that the processing of the present requests is
-- done during vblank. From the application perspective, this means for
-- example, that in a flip-based model, a single vblank /may/ cause
-- multiple swapchain images to be released at once, while
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR' /may/
-- continuously be releasing images as new requests become ready.
--
-- This additional present mode is useful when using a time-based present
-- API.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME'
--
-- -   'EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2024-05-28 (Lionel Duc)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_present_mode_fifo_latest_ready Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_present_mode_fifo_latest_ready  ( PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT(..)
                                                                , EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION
                                                                , pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION
                                                                , EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME
                                                                , pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME
                                                                , PresentModeKHR(..)
                                                                ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_EXT))
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
-- | VkPhysicalDevicePresentModeFifoLatestReadyFeaturesEXT - Structure
-- describing support for VK_PRESENT_MODE_FIFO_LATEST_READY_EXT
--
-- = Description
--
-- If the 'PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT' structure
-- is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT' /can/
-- also be used in the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively enable these
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_mode_fifo_latest_ready VK_EXT_present_mode_fifo_latest_ready>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT = PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT
  { -- | #features-presentModeFifoLatestReady# @presentModeFifoLatestReady@
    -- specifies whether the implementation supports the
    -- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_EXT'
    -- present mode.
    presentModeFifoLatestReady :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT

instance ToCStruct PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentModeFifoLatestReady))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_MODE_FIFO_LATEST_READY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT where
  peekCStruct p = do
    presentModeFifoLatestReady <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT
             (bool32ToBool presentModeFifoLatestReady)

instance Storable PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT where
  zero = PhysicalDevicePresentModeFifoLatestReadyFeaturesEXT
           zero


type EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION"
pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_SPEC_VERSION = 1


type EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME = "VK_EXT_present_mode_fifo_latest_ready"

-- No documentation found for TopLevel "VK_EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME"
pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRESENT_MODE_FIFO_LATEST_READY_EXTENSION_NAME = "VK_EXT_present_mode_fifo_latest_ready"

