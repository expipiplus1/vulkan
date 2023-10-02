{-# language CPP #-}
-- | = Name
--
-- VK_KHR_surface_protected_capabilities - instance extension
--
-- == VK_KHR_surface_protected_capabilities
--
-- [__Name String__]
--     @VK_KHR_surface_protected_capabilities@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     240
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--
-- [__Contact__]
--
--     -   Sandeep Shinde
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_surface_protected_capabilities] @sashinde%0A*Here describe the issue or question you have about the VK_KHR_surface_protected_capabilities extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Sandeep Shinde, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension extends
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR',
-- providing applications a way to query whether swapchains /can/ be
-- created with the
-- 'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PROTECTED_BIT_KHR'
-- flag set.
--
-- Vulkan 1.1 added (optional) support for protect memory and protected
-- resources including buffers
-- ('Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'),
-- images
-- ('Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'),
-- and swapchains
-- ('Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PROTECTED_BIT_KHR').
-- However, on implementations which support multiple windowing systems,
-- not all window systems /may/ be able to provide a protected display
-- path.
--
-- This extension provides a way to query if a protected swapchain created
-- for a surface (and thus a specific windowing system) /can/ be displayed
-- on screen. It extends the existing
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR'
-- structure with a new 'SurfaceProtectedCapabilitiesKHR' structure from
-- which the application /can/ obtain information about support for
-- protected swapchain creation through
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceProtectedCapabilitiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME'
--
-- -   'KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2018-12-18 (Sandeep Shinde, Daniel Koch)
--
--     -   Internal revisions.
--
-- == See Also
--
-- 'SurfaceProtectedCapabilitiesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_surface_protected_capabilities Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_surface_protected_capabilities  ( SurfaceProtectedCapabilitiesKHR(..)
                                                                , KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION
                                                                , pattern KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION
                                                                , KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
                                                                , pattern KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR))
-- | VkSurfaceProtectedCapabilitiesKHR - Structure describing capability of a
-- surface to be protected
--
-- = Description
--
-- If the @VK_GOOGLE_surfaceless_query@ extension is enabled, the value
-- returned in @supportsProtected@ will be identical for every valid
-- surface created on this physical device, and so in the
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
-- call,
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'::@surface@
-- /can/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'. In that case, the
-- contents of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR'::@surfaceCapabilities@
-- as well as any other struct chained to it will be undefined.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_protected_capabilities VK_KHR_surface_protected_capabilities>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceProtectedCapabilitiesKHR = SurfaceProtectedCapabilitiesKHR
  { -- | @supportsProtected@ specifies whether a protected swapchain created from
    -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'::@surface@
    -- for a particular windowing system /can/ be displayed on screen or not.
    -- If @supportsProtected@ is 'Vulkan.Core10.FundamentalTypes.TRUE', then
    -- creation of swapchains with the
    -- 'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PROTECTED_BIT_KHR'
    -- flag set /must/ be supported for @surface@.
    supportsProtected :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceProtectedCapabilitiesKHR)
#endif
deriving instance Show SurfaceProtectedCapabilitiesKHR

instance ToCStruct SurfaceProtectedCapabilitiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceProtectedCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (supportsProtected))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SurfaceProtectedCapabilitiesKHR where
  peekCStruct p = do
    supportsProtected <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SurfaceProtectedCapabilitiesKHR
             (bool32ToBool supportsProtected)

instance Storable SurfaceProtectedCapabilitiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceProtectedCapabilitiesKHR where
  zero = SurfaceProtectedCapabilitiesKHR
           zero


type KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION"
pattern KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION = 1


type KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME = "VK_KHR_surface_protected_capabilities"

-- No documentation found for TopLevel "VK_KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME"
pattern KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME = "VK_KHR_surface_protected_capabilities"

