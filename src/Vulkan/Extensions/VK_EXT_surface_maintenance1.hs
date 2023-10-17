{-# language CPP #-}
-- | = Name
--
-- VK_EXT_surface_maintenance1 - instance extension
--
-- == VK_EXT_surface_maintenance1
--
-- [__Name String__]
--     @VK_EXT_surface_maintenance1@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     275
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_surface_maintenance1] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_surface_maintenance1 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_surface_maintenance1.adoc VK_EXT_surface_maintenance1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-11-09
--
-- [__Contributors__]
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
-- @VK_EXT_surface_maintenance1@ adds a collection of window system
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
--     -   'SurfacePresentModeEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfacePresentModeCompatibilityEXT'
--
--     -   'SurfacePresentScalingCapabilitiesEXT'
--
-- == New Enums
--
-- -   'PresentGravityFlagBitsEXT'
--
-- -   'PresentScalingFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'PresentGravityFlagsEXT'
--
-- -   'PresentScalingFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2019-02-27 (Lionel Landwerlin)
--
--     -   Internal revisions
--
-- -   Revision 1, 2022-11-09 (Shahbaz Youssefi)
--
--     -   Add functionality and complete spec
--
-- == See Also
--
-- 'PresentGravityFlagBitsEXT', 'PresentGravityFlagsEXT',
-- 'PresentScalingFlagBitsEXT', 'PresentScalingFlagsEXT',
-- 'SurfacePresentModeCompatibilityEXT', 'SurfacePresentModeEXT',
-- 'SurfacePresentScalingCapabilitiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_surface_maintenance1  ( SurfacePresentModeEXT(..)
                                                      , SurfacePresentScalingCapabilitiesEXT(..)
                                                      , SurfacePresentModeCompatibilityEXT(..)
                                                      , PresentScalingFlagsEXT
                                                      , PresentScalingFlagBitsEXT( PRESENT_SCALING_ONE_TO_ONE_BIT_EXT
                                                                                 , PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_EXT
                                                                                 , PRESENT_SCALING_STRETCH_BIT_EXT
                                                                                 , ..
                                                                                 )
                                                      , PresentGravityFlagsEXT
                                                      , PresentGravityFlagBitsEXT( PRESENT_GRAVITY_MIN_BIT_EXT
                                                                                 , PRESENT_GRAVITY_MAX_BIT_EXT
                                                                                 , PRESENT_GRAVITY_CENTERED_BIT_EXT
                                                                                 , ..
                                                                                 )
                                                      , EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION
                                                      , pattern EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION
                                                      , EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME
                                                      , pattern EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT))
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
-- | VkSurfacePresentModeEXT - Structure describing present mode of a surface
--
-- = Description
--
-- If the 'SurfacePresentModeEXT' structure is included in the @pNext@
-- chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
-- the values returned in
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@minImageCount@,
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceCapabilitiesKHR'::@maxImageCount@,
-- 'SurfacePresentScalingCapabilitiesEXT'::@minScaledImageExtent@, and
-- 'SurfacePresentScalingCapabilitiesEXT'::@maxScaledImageExtent@ are valid
-- only for the specified @presentMode@. If @presentMode@ is
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
-- or
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR',
-- the per-present mode image counts /must/ both be one. The per-present
-- mode image counts /may/ be less-than or greater-than the image counts
-- returned when 'SurfacePresentModeEXT' is not provided.
--
-- Note
--
-- If
-- 'Vulkan.Extensions.VK_EXT_swapchain_maintenance1.SwapchainPresentModesCreateInfoEXT'
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
-- Using 'SurfacePresentModeEXT' and
-- 'Vulkan.Extensions.VK_EXT_swapchain_maintenance1.SwapchainPresentModesCreateInfoEXT'
-- together effectively removes this problem.
--
-- 'Vulkan.Extensions.VK_EXT_swapchain_maintenance1.SwapchainPresentModesCreateInfoEXT'
-- is required for the specification to be backwards compatible with
-- applications that do not know about, or make use of this feature.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfacePresentModeEXT = SurfacePresentModeEXT
  { -- | @presentMode@ is the presentation mode the swapchain will use.
    --
    -- #VUID-VkSurfacePresentModeEXT-presentMode-07780# @presentMode@ /must/ be
    -- a value reported by
    -- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR'
    -- for the specified surface.
    --
    -- #VUID-VkSurfacePresentModeEXT-presentMode-parameter# @presentMode@
    -- /must/ be a valid 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
    -- value
    presentMode :: PresentModeKHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfacePresentModeEXT)
#endif
deriving instance Show SurfacePresentModeEXT

instance ToCStruct SurfacePresentModeEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfacePresentModeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentModeKHR)) (presentMode)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentModeKHR)) (zero)
    f

instance FromCStruct SurfacePresentModeEXT where
  peekCStruct p = do
    presentMode <- peek @PresentModeKHR ((p `plusPtr` 16 :: Ptr PresentModeKHR))
    pure $ SurfacePresentModeEXT
             presentMode

instance Storable SurfacePresentModeEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfacePresentModeEXT where
  zero = SurfacePresentModeEXT
           zero


-- | VkSurfacePresentScalingCapabilitiesEXT - Structure describing the
-- presentation scaling capabilities of the surface
--
-- = Description
--
-- Before creating a swapchain whose scaling mode /can/ be specified
-- through the use of
-- 'Vulkan.Extensions.VK_EXT_swapchain_maintenance1.SwapchainPresentScalingCreateInfoEXT',
-- obtain the set of supported scaling modes by including a
-- 'SurfacePresentModeEXT' structure in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
-- when calling
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'.
-- The implementation /must/ return the same values in
-- 'SurfacePresentScalingCapabilitiesEXT' for any of the compatible present
-- modes as obtained through 'SurfacePresentModeCompatibilityEXT'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D', 'PresentGravityFlagsEXT',
-- 'PresentScalingFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfacePresentScalingCapabilitiesEXT = SurfacePresentScalingCapabilitiesEXT
  { -- | @supportedPresentScaling@ is a bitmask of 'PresentScalingFlagBitsEXT'
    -- representing the scaling methods supported by the surface, or @0@ if
    -- application-defined scaling is not supported.
    --
    -- #VUID-VkSurfacePresentScalingCapabilitiesEXT-supportedPresentScaling-parameter#
    -- @supportedPresentScaling@ /must/ be a valid combination of
    -- 'PresentScalingFlagBitsEXT' values
    supportedPresentScaling :: PresentScalingFlagsEXT
  , -- | @supportedPresentGravityX@ is a bitmask of 'PresentGravityFlagBitsEXT'
    -- representing the X-axis pixel gravity supported by the surface, or @0@
    -- if Vulkan-defined pixel gravity is not supported for the X axis.
    --
    -- #VUID-VkSurfacePresentScalingCapabilitiesEXT-supportedPresentGravityX-parameter#
    -- @supportedPresentGravityX@ /must/ be a valid combination of
    -- 'PresentGravityFlagBitsEXT' values
    supportedPresentGravityX :: PresentGravityFlagsEXT
  , -- | @supportedPresentGravityY@ is a bitmask of 'PresentGravityFlagBitsEXT'
    -- representing the Y-axis pixel gravity supported by the surface, or @0@
    -- if Vulkan-defined pixel gravity is not supported for the Y axis.
    --
    -- #VUID-VkSurfacePresentScalingCapabilitiesEXT-supportedPresentGravityY-parameter#
    -- @supportedPresentGravityY@ /must/ be a valid combination of
    -- 'PresentGravityFlagBitsEXT' values
    supportedPresentGravityY :: PresentGravityFlagsEXT
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
deriving instance Generic (SurfacePresentScalingCapabilitiesEXT)
#endif
deriving instance Show SurfacePresentScalingCapabilitiesEXT

instance ToCStruct SurfacePresentScalingCapabilitiesEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfacePresentScalingCapabilitiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentScalingFlagsEXT)) (supportedPresentScaling)
    poke ((p `plusPtr` 20 :: Ptr PresentGravityFlagsEXT)) (supportedPresentGravityX)
    poke ((p `plusPtr` 24 :: Ptr PresentGravityFlagsEXT)) (supportedPresentGravityY)
    poke ((p `plusPtr` 28 :: Ptr Extent2D)) (minScaledImageExtent)
    poke ((p `plusPtr` 36 :: Ptr Extent2D)) (maxScaledImageExtent)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SurfacePresentScalingCapabilitiesEXT where
  peekCStruct p = do
    supportedPresentScaling <- peek @PresentScalingFlagsEXT ((p `plusPtr` 16 :: Ptr PresentScalingFlagsEXT))
    supportedPresentGravityX <- peek @PresentGravityFlagsEXT ((p `plusPtr` 20 :: Ptr PresentGravityFlagsEXT))
    supportedPresentGravityY <- peek @PresentGravityFlagsEXT ((p `plusPtr` 24 :: Ptr PresentGravityFlagsEXT))
    minScaledImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 28 :: Ptr Extent2D))
    maxScaledImageExtent <- peekCStruct @Extent2D ((p `plusPtr` 36 :: Ptr Extent2D))
    pure $ SurfacePresentScalingCapabilitiesEXT
             supportedPresentScaling
             supportedPresentGravityX
             supportedPresentGravityY
             minScaledImageExtent
             maxScaledImageExtent

instance Storable SurfacePresentScalingCapabilitiesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfacePresentScalingCapabilitiesEXT where
  zero = SurfacePresentScalingCapabilitiesEXT
           zero
           zero
           zero
           zero
           zero


-- | VkSurfacePresentModeCompatibilityEXT - Structure describing the subset
-- of compatible presentation modes for the purposes of switching without
-- swapchain recreation
--
-- = Description
--
-- If @pPresentModes@ is @NULL@, then the number of present modes that are
-- compatible with the one specified in 'SurfacePresentModeEXT' is returned
-- in @presentModeCount@. Otherwise, @presentModeCount@ must be set by the
-- user to the number of elements in the @pPresentModes@ array, and on
-- return the variable is overwritten with the number of values actually
-- written to @pPresentModes@. If the value of @presentModeCount@ is less
-- than the number of compatible present modes that are supported, at most
-- @presentModeCount@ values will be written to @pPresentModes@. The
-- implementation /must/ include the present mode passed to
-- 'SurfacePresentModeEXT' in @pPresentModes@, unless @presentModeCount@ is
-- zero.
--
-- Before creating a swapchain whose present modes /can/ be modified
-- through the use of
-- 'Vulkan.Extensions.VK_EXT_swapchain_maintenance1.SwapchainPresentModesCreateInfoEXT',
-- obtain the set of present modes compatible with a given initial present
-- mode by including a 'SurfacePresentModeEXT' structure in the @pNext@
-- chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'
-- when calling
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSurfacePresentModeCompatibilityEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT'
--
-- -   #VUID-VkSurfacePresentModeCompatibilityEXT-pPresentModes-parameter#
--     If @presentModeCount@ is not @0@, and @pPresentModes@ is not @NULL@,
--     @pPresentModes@ /must/ be a valid pointer to an array of
--     @presentModeCount@ 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfacePresentModeCompatibilityEXT = SurfacePresentModeCompatibilityEXT
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
deriving instance Generic (SurfacePresentModeCompatibilityEXT)
#endif
deriving instance Show SurfacePresentModeCompatibilityEXT

instance ToCStruct SurfacePresentModeCompatibilityEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfacePresentModeCompatibilityEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (presentModeCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR))) (presentModes)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SurfacePresentModeCompatibilityEXT where
  peekCStruct p = do
    presentModeCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentModes <- peek @(Ptr PresentModeKHR) ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR)))
    pure $ SurfacePresentModeCompatibilityEXT
             presentModeCount pPresentModes

instance Storable SurfacePresentModeCompatibilityEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfacePresentModeCompatibilityEXT where
  zero = SurfacePresentModeCompatibilityEXT
           zero
           zero


type PresentScalingFlagsEXT = PresentScalingFlagBitsEXT

-- | VkPresentScalingFlagBitsEXT - Bitmask specifying presentation scaling
-- methods
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>,
-- 'PresentScalingFlagsEXT'
newtype PresentScalingFlagBitsEXT = PresentScalingFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PRESENT_SCALING_ONE_TO_ONE_BIT_EXT' specifies that no scaling occurs,
-- and pixels in the swapchain image are mapped to one and only one pixel
-- in the surface. The mapping between pixels is defined by the chosen
-- presentation gravity.
pattern PRESENT_SCALING_ONE_TO_ONE_BIT_EXT = PresentScalingFlagBitsEXT 0x00000001

-- | 'PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_EXT' specifies that the
-- swapchain image will be minified or magnified such that at least one of
-- the resulting width or height is equal to the corresponding surface
-- dimension, and the other resulting dimension is less than or equal to
-- the corresponding surface dimension, with the aspect ratio of the
-- resulting image being identical to that of the original swapchain image.
pattern PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_EXT = PresentScalingFlagBitsEXT 0x00000002

-- | 'PRESENT_SCALING_STRETCH_BIT_EXT' specifies that the swapchain image
-- will be minified or magnified such that the resulting image dimensions
-- are equal to those of the surface.
pattern PRESENT_SCALING_STRETCH_BIT_EXT = PresentScalingFlagBitsEXT 0x00000004

conNamePresentScalingFlagBitsEXT :: String
conNamePresentScalingFlagBitsEXT = "PresentScalingFlagBitsEXT"

enumPrefixPresentScalingFlagBitsEXT :: String
enumPrefixPresentScalingFlagBitsEXT = "PRESENT_SCALING_"

showTablePresentScalingFlagBitsEXT :: [(PresentScalingFlagBitsEXT, String)]
showTablePresentScalingFlagBitsEXT =
  [
    ( PRESENT_SCALING_ONE_TO_ONE_BIT_EXT
    , "ONE_TO_ONE_BIT_EXT"
    )
  ,
    ( PRESENT_SCALING_ASPECT_RATIO_STRETCH_BIT_EXT
    , "ASPECT_RATIO_STRETCH_BIT_EXT"
    )
  ,
    ( PRESENT_SCALING_STRETCH_BIT_EXT
    , "STRETCH_BIT_EXT"
    )
  ]

instance Show PresentScalingFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixPresentScalingFlagBitsEXT
      showTablePresentScalingFlagBitsEXT
      conNamePresentScalingFlagBitsEXT
      (\(PresentScalingFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PresentScalingFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixPresentScalingFlagBitsEXT
      showTablePresentScalingFlagBitsEXT
      conNamePresentScalingFlagBitsEXT
      PresentScalingFlagBitsEXT

type PresentGravityFlagsEXT = PresentGravityFlagBitsEXT

-- | VkPresentGravityFlagBitsEXT - Bitmask specifying presentation pixel
-- gravity on either the x or y axis
--
-- = Description
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
-- 'PresentGravityFlagsEXT'
newtype PresentGravityFlagBitsEXT = PresentGravityFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PRESENT_GRAVITY_MIN_BIT_EXT' means that the pixels will gravitate
-- towards the top or left side of the surface.
pattern PRESENT_GRAVITY_MIN_BIT_EXT = PresentGravityFlagBitsEXT 0x00000001

-- | 'PRESENT_GRAVITY_MAX_BIT_EXT' means that the pixels will gravitate
-- towards the bottom or right side of the surface.
pattern PRESENT_GRAVITY_MAX_BIT_EXT = PresentGravityFlagBitsEXT 0x00000002

-- | 'PRESENT_GRAVITY_CENTERED_BIT_EXT' means that the pixels will be
-- centered in the surface.
pattern PRESENT_GRAVITY_CENTERED_BIT_EXT = PresentGravityFlagBitsEXT 0x00000004

conNamePresentGravityFlagBitsEXT :: String
conNamePresentGravityFlagBitsEXT = "PresentGravityFlagBitsEXT"

enumPrefixPresentGravityFlagBitsEXT :: String
enumPrefixPresentGravityFlagBitsEXT = "PRESENT_GRAVITY_"

showTablePresentGravityFlagBitsEXT :: [(PresentGravityFlagBitsEXT, String)]
showTablePresentGravityFlagBitsEXT =
  [
    ( PRESENT_GRAVITY_MIN_BIT_EXT
    , "MIN_BIT_EXT"
    )
  ,
    ( PRESENT_GRAVITY_MAX_BIT_EXT
    , "MAX_BIT_EXT"
    )
  ,
    ( PRESENT_GRAVITY_CENTERED_BIT_EXT
    , "CENTERED_BIT_EXT"
    )
  ]

instance Show PresentGravityFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixPresentGravityFlagBitsEXT
      showTablePresentGravityFlagBitsEXT
      conNamePresentGravityFlagBitsEXT
      (\(PresentGravityFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PresentGravityFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixPresentGravityFlagBitsEXT
      showTablePresentGravityFlagBitsEXT
      conNamePresentGravityFlagBitsEXT
      PresentGravityFlagBitsEXT

type EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION"
pattern EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION = 1


type EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME = "VK_EXT_surface_maintenance1"

-- No documentation found for TopLevel "VK_EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME"
pattern EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME = "VK_EXT_surface_maintenance1"

