{-# language CPP #-}
-- | = Name
--
-- VK_KHR_surface_maintenance1 - instance extension
--
-- = VK_KHR_surface_maintenance1
--
-- [__Name String__]
--     @VK_KHR_surface_maintenance1@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     487
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_surface_maintenance1] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_surface_maintenance1 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_surface_maintenance1.adoc VK_KHR_surface_maintenance1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-03-31
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Lionel Landwerlin, Intel
--
--     -   Shahbaz Youssefi, Google
--
--     -   Chris Forbes, Google
--
--     -   Ian Elliott, Google
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Daniel Stone, Collabora
--
-- == Description
--
-- This extension is based off the @VK_EXT_surface_maintenance1@ extension.
--
-- @VK_KHR_surface_maintenance1@ adds a collection of window system
-- integration features that were intentionally left out or overlooked in
-- the original @VK_KHR_surface@ extension.
--
-- The new features are as follows:
--
-- -   Allow querying number of min\/max images from a surface for a
--     particular presentation mode.
--
-- -   Allow querying a surface’s scaled presentation capabilities.
--
-- -   Allow querying a surface for the set of presentation modes which can
--     be easily switched between without requiring swapchain recreation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR':
--
--     -   'SurfacePresentModeKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfacePresentModeCompatibilityKHR'
--
--     -   'SurfacePresentScalingCapabilitiesKHR'
--
-- == New Enums
--
-- -   'PresentGravityFlagBitsKHR'
--
-- -   'PresentScalingFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'PresentGravityFlagsKHR'
--
-- -   'PresentScalingFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'KHR_SURFACE_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2025-03-31 (Shahbaz Youssefi)
--
--     -   Based on VK_EXT_surface_maintenance1
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_surface_maintenance1 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_surface_maintenance1  ( pattern PRESENT_SCALING_ONE_TO_ONE_BIT_EXT
                                                      , pattern PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_EXT
                                                      , pattern PRESENT_SCALING_STRETCH_BIT_EXT
                                                      , pattern PRESENT_GRAVITY_MIN_BIT_EXT
                                                      , pattern PRESENT_GRAVITY_MAX_BIT_EXT
                                                      , pattern PRESENT_GRAVITY_CENTERED_BIT_EXT
                                                      , SurfacePresentModeKHR(..)
                                                      , SurfacePresentScalingCapabilitiesKHR(..)
                                                      , SurfacePresentModeCompatibilityKHR(..)
                                                      , PresentScalingFlagsKHR
                                                      , PresentScalingFlagBitsKHR( PRESENT_SCALING_ONE_TO_ONE_BIT_KHR
                                                                                 , PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_KHR
                                                                                 , PRESENT_SCALING_STRETCH_BIT_KHR
                                                                                 , ..
                                                                                 )
                                                      , PresentGravityFlagsKHR
                                                      , PresentGravityFlagBitsKHR( PRESENT_GRAVITY_MIN_BIT_KHR
                                                                                 , PRESENT_GRAVITY_MAX_BIT_KHR
                                                                                 , PRESENT_GRAVITY_CENTERED_BIT_KHR
                                                                                 , ..
                                                                                 )
                                                      , KHR_SURFACE_MAINTENANCE_1_SPEC_VERSION
                                                      , pattern KHR_SURFACE_MAINTENANCE_1_SPEC_VERSION
                                                      , KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME
                                                      , pattern KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME
                                                      , PresentModeKHR(..)
                                                      ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR))
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
-- No documentation found for TopLevel "VK_PRESENT_SCALING_ONE_TO_ONE_BIT_EXT"
pattern PRESENT_SCALING_ONE_TO_ONE_BIT_EXT = PRESENT_SCALING_ONE_TO_ONE_BIT_KHR


-- No documentation found for TopLevel "VK_PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_EXT"
pattern PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_EXT = PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_KHR


-- No documentation found for TopLevel "VK_PRESENT_SCALING_STRETCH_BIT_EXT"
pattern PRESENT_SCALING_STRETCH_BIT_EXT = PRESENT_SCALING_STRETCH_BIT_KHR


-- No documentation found for TopLevel "VK_PRESENT_GRAVITY_MIN_BIT_EXT"
pattern PRESENT_GRAVITY_MIN_BIT_EXT = PRESENT_GRAVITY_MIN_BIT_KHR


-- No documentation found for TopLevel "VK_PRESENT_GRAVITY_MAX_BIT_EXT"
pattern PRESENT_GRAVITY_MAX_BIT_EXT = PRESENT_GRAVITY_MAX_BIT_KHR


-- No documentation found for TopLevel "VK_PRESENT_GRAVITY_CENTERED_BIT_EXT"
pattern PRESENT_GRAVITY_CENTERED_BIT_EXT = PRESENT_GRAVITY_CENTERED_BIT_KHR


-- | VkSurfacePresentModeKHR - Structure describing present mode of a surface
--
-- = Description
--
-- If the 'SurfacePresentModeKHR' structure is included in the @pNext@
-- chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
-- the values returned in
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@minImageCount@,
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@maxImageCount@,
-- 'SurfacePresentScalingCapabilitiesKHR'::@minScaledImageExtent@, and
-- 'SurfacePresentScalingCapabilitiesKHR'::@maxScaledImageExtent@ are valid
-- only for the specified @presentMode@. If @presentMode@ is
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
-- or
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR',
-- the per-present mode image counts /must/ both be one. The per-present
-- mode image counts /may/ be less-than or greater-than the image counts
-- returned when 'SurfacePresentModeKHR' is not provided.
--
-- If
-- 'Vulkan.Extensions.VK_KHR_swapchain_maintenance1.SwapchainPresentModesCreateInfoKHR'
-- is provided to swapchain creation, the requirements for forward progress
-- may be less strict. For example, a FIFO swapchain might only require 2
-- images to guarantee forward progress, but a MAILBOX one might require 4.
-- Without the per-present image counts, such an implementation would have
-- to return 4 in
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@minImageCount@,
-- which pessimizes FIFO. Conversely, an implementation may return a low
-- number for minImageCount, but internally bump the image count when
-- application queries
-- 'Vulkan.Extensions.VK_KHR_swapchain.getSwapchainImagesKHR', which can
-- surprise applications, and is not discoverable until swapchain creation.
-- Using 'SurfacePresentModeKHR' and
-- 'Vulkan.Extensions.VK_KHR_swapchain_maintenance1.SwapchainPresentModesCreateInfoKHR'
-- together effectively removes this problem.
--
-- 'Vulkan.Extensions.VK_KHR_swapchain_maintenance1.SwapchainPresentModesCreateInfoKHR'
-- is required for the specification to be backwards compatible with
-- applications that do not know about, or make use of this feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_maintenance1 VK_KHR_surface_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfacePresentModeKHR = SurfacePresentModeKHR
  { -- | @presentMode@ is the presentation mode the swapchain will use.
    --
    -- #VUID-VkSurfacePresentModeKHR-presentMode-07780# @presentMode@ /must/ be
    -- a value reported by
    -- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR'
    -- for the specified surface
    --
    -- #VUID-VkSurfacePresentModeKHR-presentMode-parameter# @presentMode@
    -- /must/ be a valid 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
    -- value
    presentMode :: PresentModeKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfacePresentModeKHR)
#endif
deriving instance Show SurfacePresentModeKHR

instance ToCStruct SurfacePresentModeKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfacePresentModeKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentModeKHR)) (presentMode)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentModeKHR)) (zero)
    f

instance FromCStruct SurfacePresentModeKHR where
  peekCStruct p = do
    presentMode <- peek @PresentModeKHR ((p `plusPtr` 16 :: Ptr PresentModeKHR))
    pure $ SurfacePresentModeKHR
             presentMode

instance Storable SurfacePresentModeKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfacePresentModeKHR where
  zero = SurfacePresentModeKHR
           zero


-- | VkSurfacePresentScalingCapabilitiesKHR - Structure describing the
-- presentation scaling capabilities of the surface
--
-- = Description
--
-- To query the set of supported scaling modes for a given present mode,
-- add a 'SurfacePresentModeKHR' structure in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
-- when calling
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'.
-- The implementation /must/ return the same values in
-- 'SurfacePresentScalingCapabilitiesKHR' for any of the compatible present
-- modes as obtained through 'SurfacePresentModeCompatibilityKHR'.
--
-- The application /can/ specify the scaling mode when creating a swapchain
-- through the use of
-- 'Vulkan.Extensions.VK_KHR_swapchain_maintenance1.SwapchainPresentScalingCreateInfoKHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_maintenance1 VK_KHR_surface_maintenance1>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D', 'PresentGravityFlagsKHR',
-- 'PresentScalingFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfacePresentScalingCapabilitiesKHR = SurfacePresentScalingCapabilitiesKHR
  { -- | @supportedPresentScaling@ is a bitmask of 'PresentScalingFlagBitsKHR'
    -- representing the scaling methods supported by the surface, or @0@ if
    -- application-defined scaling is not supported.
    --
    -- #VUID-VkSurfacePresentScalingCapabilitiesKHR-supportedPresentScaling-parameter#
    -- @supportedPresentScaling@ /must/ be a valid combination of
    -- 'PresentScalingFlagBitsKHR' values
    supportedPresentScaling :: PresentScalingFlagsKHR
  , -- | @supportedPresentGravityX@ is a bitmask of 'PresentGravityFlagBitsKHR'
    -- representing the X-axis pixel gravity supported by the surface, or @0@
    -- if Vulkan-defined pixel gravity is not supported for the X axis.
    --
    -- #VUID-VkSurfacePresentScalingCapabilitiesKHR-supportedPresentGravityX-parameter#
    -- @supportedPresentGravityX@ /must/ be a valid combination of
    -- 'PresentGravityFlagBitsKHR' values
    supportedPresentGravityX :: PresentGravityFlagsKHR
  , -- | @supportedPresentGravityY@ is a bitmask of 'PresentGravityFlagBitsKHR'
    -- representing the Y-axis pixel gravity supported by the surface, or @0@
    -- if Vulkan-defined pixel gravity is not supported for the Y axis.
    --
    -- #VUID-VkSurfacePresentScalingCapabilitiesKHR-supportedPresentGravityY-parameter#
    -- @supportedPresentGravityY@ /must/ be a valid combination of
    -- 'PresentGravityFlagBitsKHR' values
    supportedPresentGravityY :: PresentGravityFlagsKHR
  , -- | @minScaledImageExtent@ contains the smallest valid swapchain extent for
    -- the surface on the specified device when one of the scaling methods
    -- specified in @supportedPresentScaling@ is used, or the special value
    -- (0xFFFFFFFF, 0xFFFFFFFF) indicating that the surface size will be
    -- determined by the extent of a swapchain targeting the surface. The
    -- @width@ and @height@ of the extent will each be smaller than or equal to
    -- the corresponding @width@ and @height@ of
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@minImageExtent@.
    minScaledImageExtent :: Extent2D
  , -- | @maxScaledImageExtent@ contains the largest valid swapchain extent for
    -- the surface on the specified device when one of the scaling methods
    -- specified in @supportedPresentScaling@ is used, or the special value
    -- described above for @minScaledImageExtent@. The @width@ and @height@ of
    -- the extent will each be greater than or equal to the corresponding
    -- @width@ and @height@ of
    -- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@maxImageExtent@.
    maxScaledImageExtent :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfacePresentScalingCapabilitiesKHR)
#endif
deriving instance Show SurfacePresentScalingCapabilitiesKHR

instance ToCStruct SurfacePresentScalingCapabilitiesKHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfacePresentScalingCapabilitiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentScalingFlagsKHR)) (supportedPresentScaling)
    poke ((p `plusPtr` 20 :: Ptr PresentGravityFlagsKHR)) (supportedPresentGravityX)
    poke ((p `plusPtr` 24 :: Ptr PresentGravityFlagsKHR)) (supportedPresentGravityY)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (minScaledImageExtent)
    poke ((p `plusPtr` 36 :: Ptr Extent2D)) (maxScaledImageExtent)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SurfacePresentScalingCapabilitiesKHR where
  peekCStruct p = do
    supportedPresentScaling <- peek @PresentScalingFlagsKHR ((p `plusPtr` 16 :: Ptr PresentScalingFlagsKHR))
    supportedPresentGravityX <- peek @PresentGravityFlagsKHR ((p `plusPtr` 20 :: Ptr PresentGravityFlagsKHR))
    supportedPresentGravityY <- peek @PresentGravityFlagsKHR ((p `plusPtr` 24 :: Ptr PresentGravityFlagsKHR))
    minScaledImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 28 :: Ptr Extent2D))
    maxScaledImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 36 :: Ptr Extent2D))
    pure $ SurfacePresentScalingCapabilitiesKHR
             supportedPresentScaling
             supportedPresentGravityX
             supportedPresentGravityY
             minScaledImageExtent
             maxScaledImageExtent

instance Storable SurfacePresentScalingCapabilitiesKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfacePresentScalingCapabilitiesKHR where
  zero = SurfacePresentScalingCapabilitiesKHR
           zero
           zero
           zero
           zero
           zero


-- | VkSurfacePresentModeCompatibilityKHR - Structure describing the subset
-- of compatible presentation modes for the purposes of switching without
-- swapchain recreation
--
-- = Description
--
-- If @pPresentModes@ is @NULL@, then the number of present modes that are
-- compatible with the one specified in 'SurfacePresentModeKHR' is returned
-- in @presentModeCount@. Otherwise, @presentModeCount@ /must/ be set by
-- the application to the number of elements in the @pPresentModes@ array,
-- and on return is overwritten with the number of values actually written
-- to @pPresentModes@. If the value of @presentModeCount@ is less than the
-- number of compatible present modes that are supported, at most
-- @presentModeCount@ values will be written to @pPresentModes@. The
-- implementation /must/ include the present mode passed to
-- 'SurfacePresentModeKHR' in @pPresentModes@, unless @presentModeCount@ is
-- zero.
--
-- To query the set of present modes compatible with a given initial
-- present mode, add a 'SurfacePresentModeKHR' structure in the @pNext@
-- chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
-- when calling
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'.
--
-- The application /can/ create a swapchain whose present mode /can/ be
-- modified through the use of
-- 'Vulkan.Extensions.VK_KHR_swapchain_maintenance1.SwapchainPresentModesCreateInfoKHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSurfacePresentModeCompatibilityKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR'
--
-- -   #VUID-VkSurfacePresentModeCompatibilityKHR-pPresentModes-parameter#
--     If @presentModeCount@ is not @0@, and @pPresentModes@ is not @NULL@,
--     @pPresentModes@ /must/ be a valid pointer to an array of
--     @presentModeCount@ 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_maintenance1 VK_KHR_surface_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfacePresentModeCompatibilityKHR = SurfacePresentModeCompatibilityKHR
  { -- | @presentModeCount@ is an integer related to the number of present modes
    -- available or queried, as described below.
    presentModeCount :: Word32
  , -- | @pPresentModes@ is a pointer to an array of
    -- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR' in which present modes
    -- compatible with a given present mode are returned.
    presentModes :: Ptr PresentModeKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfacePresentModeCompatibilityKHR)
#endif
deriving instance Show SurfacePresentModeCompatibilityKHR

instance ToCStruct SurfacePresentModeCompatibilityKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfacePresentModeCompatibilityKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (presentModeCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR))) (presentModes)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SurfacePresentModeCompatibilityKHR where
  peekCStruct p = do
    presentModeCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentModes <- peek @(Ptr PresentModeKHR) ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR)))
    pure $ SurfacePresentModeCompatibilityKHR
             presentModeCount pPresentModes

instance Storable SurfacePresentModeCompatibilityKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfacePresentModeCompatibilityKHR where
  zero = SurfacePresentModeCompatibilityKHR
           zero
           zero


type PresentScalingFlagsKHR = PresentScalingFlagBitsKHR

-- | VkPresentScalingFlagBitsKHR - Bitmask specifying presentation scaling
-- methods
--
-- = Description
--
-- -   'PRESENT_SCALING_ONE_TO_ONE_BIT_KHR' specifies that no scaling
--     occurs, and pixels in the swapchain image are mapped to one and only
--     one pixel in the surface. The mapping between pixels is defined by
--     the chosen presentation gravity.
--
-- -   'PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_KHR' specifies that the
--     swapchain image will be minified or magnified such that at least one
--     of the resulting width or height is equal to the corresponding
--     surface dimension, and the other resulting dimension is less than or
--     equal to the corresponding surface dimension, with the aspect ratio
--     of the resulting image being identical to that of the original
--     swapchain image.
--
-- -   'PRESENT_SCALING_STRETCH_BIT_KHR' specifies that the swapchain image
--     will be minified or magnified such that the resulting image
--     dimensions are equal to those of the surface.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_maintenance1 VK_KHR_surface_maintenance1>,
-- 'PresentScalingFlagsKHR'
newtype PresentScalingFlagBitsKHR = PresentScalingFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPresentScalingFlagBitsKHR" "VK_PRESENT_SCALING_ONE_TO_ONE_BIT_KHR"
pattern PRESENT_SCALING_ONE_TO_ONE_BIT_KHR = PresentScalingFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkPresentScalingFlagBitsKHR" "VK_PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_KHR"
pattern PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_KHR = PresentScalingFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkPresentScalingFlagBitsKHR" "VK_PRESENT_SCALING_STRETCH_BIT_KHR"
pattern PRESENT_SCALING_STRETCH_BIT_KHR = PresentScalingFlagBitsKHR 0x00000004

conNamePresentScalingFlagBitsKHR :: String
conNamePresentScalingFlagBitsKHR = "PresentScalingFlagBitsKHR"

enumPrefixPresentScalingFlagBitsKHR :: String
enumPrefixPresentScalingFlagBitsKHR = "PRESENT_SCALING_"

showTablePresentScalingFlagBitsKHR :: [(PresentScalingFlagBitsKHR, String)]
showTablePresentScalingFlagBitsKHR =
  [
    ( PRESENT_SCALING_ONE_TO_ONE_BIT_KHR
    , "ONE_TO_ONE_BIT_KHR"
    )
  ,
    ( PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_KHR
    , "ASPECT_RATIO_STRETCH_BIT_KHR"
    )
  ,
    ( PRESENT_SCALING_STRETCH_BIT_KHR
    , "STRETCH_BIT_KHR"
    )
  ]

instance Show PresentScalingFlagBitsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixPresentScalingFlagBitsKHR
      showTablePresentScalingFlagBitsKHR
      conNamePresentScalingFlagBitsKHR
      (\(PresentScalingFlagBitsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PresentScalingFlagBitsKHR where
  readPrec =
    enumReadPrec
      enumPrefixPresentScalingFlagBitsKHR
      showTablePresentScalingFlagBitsKHR
      conNamePresentScalingFlagBitsKHR
      PresentScalingFlagBitsKHR

type PresentGravityFlagsKHR = PresentGravityFlagBitsKHR

-- | VkPresentGravityFlagBitsKHR - Bitmask specifying presentation pixel
-- gravity on either the x or y axis
--
-- = Description
--
-- -   'PRESENT_GRAVITY_MIN_BIT_KHR' means that the pixels will gravitate
--     towards the top or left side of the surface.
--
-- -   'PRESENT_GRAVITY_MAX_BIT_KHR' means that the pixels will gravitate
--     towards the bottom or right side of the surface.
--
-- -   'PRESENT_GRAVITY_CENTERED_BIT_KHR' means that the pixels will be
--     centered in the surface.
--
-- If the value in
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@currentTransform@
-- is not
-- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
-- it is implementation-defined whether the gravity configuration applies
-- to the presented image before or after transformation.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_maintenance1 VK_KHR_surface_maintenance1>,
-- 'PresentGravityFlagsKHR'
newtype PresentGravityFlagBitsKHR = PresentGravityFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPresentGravityFlagBitsKHR" "VK_PRESENT_GRAVITY_MIN_BIT_KHR"
pattern PRESENT_GRAVITY_MIN_BIT_KHR = PresentGravityFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkPresentGravityFlagBitsKHR" "VK_PRESENT_GRAVITY_MAX_BIT_KHR"
pattern PRESENT_GRAVITY_MAX_BIT_KHR = PresentGravityFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkPresentGravityFlagBitsKHR" "VK_PRESENT_GRAVITY_CENTERED_BIT_KHR"
pattern PRESENT_GRAVITY_CENTERED_BIT_KHR = PresentGravityFlagBitsKHR 0x00000004

conNamePresentGravityFlagBitsKHR :: String
conNamePresentGravityFlagBitsKHR = "PresentGravityFlagBitsKHR"

enumPrefixPresentGravityFlagBitsKHR :: String
enumPrefixPresentGravityFlagBitsKHR = "PRESENT_GRAVITY_"

showTablePresentGravityFlagBitsKHR :: [(PresentGravityFlagBitsKHR, String)]
showTablePresentGravityFlagBitsKHR =
  [
    ( PRESENT_GRAVITY_MIN_BIT_KHR
    , "MIN_BIT_KHR"
    )
  ,
    ( PRESENT_GRAVITY_MAX_BIT_KHR
    , "MAX_BIT_KHR"
    )
  ,
    ( PRESENT_GRAVITY_CENTERED_BIT_KHR
    , "CENTERED_BIT_KHR"
    )
  ]

instance Show PresentGravityFlagBitsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixPresentGravityFlagBitsKHR
      showTablePresentGravityFlagBitsKHR
      conNamePresentGravityFlagBitsKHR
      (\(PresentGravityFlagBitsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PresentGravityFlagBitsKHR where
  readPrec =
    enumReadPrec
      enumPrefixPresentGravityFlagBitsKHR
      showTablePresentGravityFlagBitsKHR
      conNamePresentGravityFlagBitsKHR
      PresentGravityFlagBitsKHR

type KHR_SURFACE_MAINTENANCE_1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SURFACE_MAINTENANCE_1_SPEC_VERSION"
pattern KHR_SURFACE_MAINTENANCE_1_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SURFACE_MAINTENANCE_1_SPEC_VERSION = 1


type KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME = "VK_KHR_surface_maintenance1"

-- No documentation found for TopLevel "VK_KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME"
pattern KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME = "VK_KHR_surface_maintenance1"

