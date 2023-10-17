{-# language CPP #-}
-- | = Name
--
-- VK_NV_present_barrier - device extension
--
-- == VK_NV_present_barrier
--
-- [__Name String__]
--     @VK_NV_present_barrier@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     293
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--
-- [__Contact__]
--
--     -   Liya Li
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_present_barrier] @liyli%0A*Here describe the issue or question you have about the VK_NV_present_barrier extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-16
--
-- [__Contributors__]
--
--     -   Liya Li, Nvidia
--
--     -   Martin Schwarzer, Nvidia
--
--     -   Andy Wolf, Nvidia
--
--     -   Ian Williams, Nvidia
--
--     -   Ben Morris, Nvidia
--
--     -   James Jones, Nvidia
--
--     -   Jeff Juliano, Nvidia
--
-- == Description
--
-- This extension adds support for synchronizing corresponding presentation
-- requests across multiple swapchains using the /present barrier/.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentBarrierFeaturesNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceCapabilitiesPresentBarrierNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainPresentBarrierCreateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_PRESENT_BARRIER_EXTENSION_NAME'
--
-- -   'NV_PRESENT_BARRIER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_BARRIER_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_BARRIER_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_BARRIER_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) Is there a query interface to check if a swapchain is using the
-- present barrier?
--
-- __RESOLVED__. There is no such query interface. When creating a
-- swapchain, an application can specify to use the /present barrier/, and
-- if the swapchain is created successfully, this swapchain will be using
-- the present barrier.
--
-- 2) Do we need an extra interface to set up the present barrier across
-- distributed systems?
--
-- __RESOLVED__. If the required hardware is presented in the system, and
-- all settings for the physical synchronization with other systems are set
-- up, an implementation manages the configuration automatically when
-- creating a swapchain, without any extra calls from the application.
--
-- == Version History
--
-- -   Revision 1, 2022-07-20
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDevicePresentBarrierFeaturesNV',
-- 'SurfaceCapabilitiesPresentBarrierNV',
-- 'SwapchainPresentBarrierCreateInfoNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_present_barrier Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_present_barrier  ( PhysicalDevicePresentBarrierFeaturesNV(..)
                                                , SurfaceCapabilitiesPresentBarrierNV(..)
                                                , SwapchainPresentBarrierCreateInfoNV(..)
                                                , NV_PRESENT_BARRIER_SPEC_VERSION
                                                , pattern NV_PRESENT_BARRIER_SPEC_VERSION
                                                , NV_PRESENT_BARRIER_EXTENSION_NAME
                                                , pattern NV_PRESENT_BARRIER_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_BARRIER_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_BARRIER_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_BARRIER_CREATE_INFO_NV))
-- | VkPhysicalDevicePresentBarrierFeaturesNV - Structure indicating support
-- for VK_NV_present_barrier extension
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePresentBarrierFeaturesNV' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePresentBarrierFeaturesNV' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_present_barrier VK_NV_present_barrier>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePresentBarrierFeaturesNV = PhysicalDevicePresentBarrierFeaturesNV
  { -- | #features-presentBarrier# @presentBarrier@ indicates that the
    -- implementation supports the present barrier feature.
    presentBarrier :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePresentBarrierFeaturesNV)
#endif
deriving instance Show PhysicalDevicePresentBarrierFeaturesNV

instance ToCStruct PhysicalDevicePresentBarrierFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePresentBarrierFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_BARRIER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentBarrier))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_BARRIER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePresentBarrierFeaturesNV where
  peekCStruct p = do
    presentBarrier <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePresentBarrierFeaturesNV
             (bool32ToBool presentBarrier)

instance Storable PhysicalDevicePresentBarrierFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePresentBarrierFeaturesNV where
  zero = PhysicalDevicePresentBarrierFeaturesNV
           zero


-- | VkSurfaceCapabilitiesPresentBarrierNV - Structure describing present
-- barrier capabilities of a surface
--
-- = Description
--
-- This structure /can/ be included in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR'
-- to determine support for present barrier access. If
-- @presentBarrierSupported@ is 'Vulkan.Core10.FundamentalTypes.FALSE', it
-- indicates that the present barrier feature is not obtainable for this
-- surface.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_present_barrier VK_NV_present_barrier>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceCapabilitiesPresentBarrierNV = SurfaceCapabilitiesPresentBarrierNV
  { -- | @presentBarrierSupported@ is a boolean describing whether the surface is
    -- able to make use of the present barrier feature.
    presentBarrierSupported :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilitiesPresentBarrierNV)
#endif
deriving instance Show SurfaceCapabilitiesPresentBarrierNV

instance ToCStruct SurfaceCapabilitiesPresentBarrierNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilitiesPresentBarrierNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_BARRIER_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentBarrierSupported))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_BARRIER_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SurfaceCapabilitiesPresentBarrierNV where
  peekCStruct p = do
    presentBarrierSupported <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SurfaceCapabilitiesPresentBarrierNV
             (bool32ToBool presentBarrierSupported)

instance Storable SurfaceCapabilitiesPresentBarrierNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilitiesPresentBarrierNV where
  zero = SurfaceCapabilitiesPresentBarrierNV
           zero


-- | VkSwapchainPresentBarrierCreateInfoNV - specify the present barrier
-- membership of this swapchain
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR' does not
-- include this structure, the default value for @presentBarrierEnable@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE', meaning the swapchain does not
-- request to use the present barrier. Additionally, when recreating a
-- swapchain that was using the present barrier, and the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR' does not
-- include this structure, it means the swapchain will stop using the
-- present barrier.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_present_barrier VK_NV_present_barrier>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainPresentBarrierCreateInfoNV = SwapchainPresentBarrierCreateInfoNV
  { -- | @presentBarrierEnable@ is a boolean value indicating a request for using
    -- the /present barrier/.
    presentBarrierEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainPresentBarrierCreateInfoNV)
#endif
deriving instance Show SwapchainPresentBarrierCreateInfoNV

instance ToCStruct SwapchainPresentBarrierCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainPresentBarrierCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_BARRIER_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentBarrierEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_PRESENT_BARRIER_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SwapchainPresentBarrierCreateInfoNV where
  peekCStruct p = do
    presentBarrierEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SwapchainPresentBarrierCreateInfoNV
             (bool32ToBool presentBarrierEnable)

instance Storable SwapchainPresentBarrierCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainPresentBarrierCreateInfoNV where
  zero = SwapchainPresentBarrierCreateInfoNV
           zero


type NV_PRESENT_BARRIER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_PRESENT_BARRIER_SPEC_VERSION"
pattern NV_PRESENT_BARRIER_SPEC_VERSION :: forall a . Integral a => a
pattern NV_PRESENT_BARRIER_SPEC_VERSION = 1


type NV_PRESENT_BARRIER_EXTENSION_NAME = "VK_NV_present_barrier"

-- No documentation found for TopLevel "VK_NV_PRESENT_BARRIER_EXTENSION_NAME"
pattern NV_PRESENT_BARRIER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_PRESENT_BARRIER_EXTENSION_NAME = "VK_NV_present_barrier"

