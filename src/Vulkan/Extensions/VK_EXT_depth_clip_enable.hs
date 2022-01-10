{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_clip_enable - device extension
--
-- == VK_EXT_depth_clip_enable
--
-- [__Name String__]
--     @VK_EXT_depth_clip_enable@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     103
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_clip_enable] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_EXT_depth_clip_enable extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-20
--
-- [__Contributors__]
--
--     -   Daniel Rakos, AMD
--
--     -   Henri Verbeet, CodeWeavers
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Philip Rebohle, DXVK
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension allows the depth clipping operation, that is normally
-- implicitly controlled by
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@depthClampEnable@,
-- to instead be controlled explicitly by
-- 'PipelineRasterizationDepthClipStateCreateInfoEXT'::@depthClipEnable@.
--
-- This is useful for translating DX content which assumes depth clamping
-- is always enabled, but depth clip can be controlled by the
-- DepthClipEnable rasterization state (D3D12_RASTERIZER_DESC).
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClipEnableFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationDepthClipStateCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'PipelineRasterizationDepthClipStateCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-12-20 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDepthClipEnableFeaturesEXT',
-- 'PipelineRasterizationDepthClipStateCreateFlagsEXT',
-- 'PipelineRasterizationDepthClipStateCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_enable Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_clip_enable  ( PhysicalDeviceDepthClipEnableFeaturesEXT(..)
                                                   , PipelineRasterizationDepthClipStateCreateInfoEXT(..)
                                                   , PipelineRasterizationDepthClipStateCreateFlagsEXT(..)
                                                   , EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
                                                   , pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION
                                                   , EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME
                                                   , pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME
                                                   ) where

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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT))
-- | VkPhysicalDeviceDepthClipEnableFeaturesEXT - Structure indicating
-- support for explicit enable of depth clip
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceDepthClipEnableFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceDepthClipEnableFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_enable VK_EXT_depth_clip_enable>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDepthClipEnableFeaturesEXT = PhysicalDeviceDepthClipEnableFeaturesEXT
  { -- | #features-depthClipEnable# @depthClipEnable@ indicates that the
    -- implementation supports setting the depth clipping operation explicitly
    -- via the 'PipelineRasterizationDepthClipStateCreateInfoEXT' pipeline
    -- state. Otherwise depth clipping is only enabled when
    -- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@depthClampEnable@
    -- is set to 'Vulkan.Core10.FundamentalTypes.FALSE'.
    depthClipEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDepthClipEnableFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceDepthClipEnableFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClipEnableFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDepthClipEnableFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (depthClipEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDepthClipEnableFeaturesEXT where
  peekCStruct p = do
    depthClipEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDepthClipEnableFeaturesEXT
             (bool32ToBool depthClipEnable)

instance Storable PhysicalDeviceDepthClipEnableFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDepthClipEnableFeaturesEXT where
  zero = PhysicalDeviceDepthClipEnableFeaturesEXT
           zero


-- | VkPipelineRasterizationDepthClipStateCreateInfoEXT - Structure
-- specifying depth clipping state
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_enable VK_EXT_depth_clip_enable>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'PipelineRasterizationDepthClipStateCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationDepthClipStateCreateInfoEXT = PipelineRasterizationDepthClipStateCreateInfoEXT
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkPipelineRasterizationDepthClipStateCreateInfoEXT-flags-zerobitmask#
    -- @flags@ /must/ be @0@
    flags :: PipelineRasterizationDepthClipStateCreateFlagsEXT
  , -- | @depthClipEnable@ controls whether depth clipping is enabled as
    -- described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>.
    depthClipEnable :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationDepthClipStateCreateInfoEXT)
#endif
deriving instance Show PipelineRasterizationDepthClipStateCreateInfoEXT

instance ToCStruct PipelineRasterizationDepthClipStateCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationDepthClipStateCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineRasterizationDepthClipStateCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (depthClipEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineRasterizationDepthClipStateCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @PipelineRasterizationDepthClipStateCreateFlagsEXT ((p `plusPtr` 16 :: Ptr PipelineRasterizationDepthClipStateCreateFlagsEXT))
    depthClipEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PipelineRasterizationDepthClipStateCreateInfoEXT
             flags (bool32ToBool depthClipEnable)

instance Storable PipelineRasterizationDepthClipStateCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationDepthClipStateCreateInfoEXT where
  zero = PipelineRasterizationDepthClipStateCreateInfoEXT
           zero
           zero


-- | VkPipelineRasterizationDepthClipStateCreateFlagsEXT - Reserved for
-- future use
--
-- = Description
--
-- 'PipelineRasterizationDepthClipStateCreateFlagsEXT' is a bitmask type
-- for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_clip_enable VK_EXT_depth_clip_enable>,
-- 'PipelineRasterizationDepthClipStateCreateInfoEXT'
newtype PipelineRasterizationDepthClipStateCreateFlagsEXT = PipelineRasterizationDepthClipStateCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineRasterizationDepthClipStateCreateFlagsEXT :: String
conNamePipelineRasterizationDepthClipStateCreateFlagsEXT = "PipelineRasterizationDepthClipStateCreateFlagsEXT"

enumPrefixPipelineRasterizationDepthClipStateCreateFlagsEXT :: String
enumPrefixPipelineRasterizationDepthClipStateCreateFlagsEXT = ""

showTablePipelineRasterizationDepthClipStateCreateFlagsEXT
  :: [(PipelineRasterizationDepthClipStateCreateFlagsEXT, String)]
showTablePipelineRasterizationDepthClipStateCreateFlagsEXT = []

instance Show PipelineRasterizationDepthClipStateCreateFlagsEXT where
  showsPrec = enumShowsPrec enumPrefixPipelineRasterizationDepthClipStateCreateFlagsEXT
                            showTablePipelineRasterizationDepthClipStateCreateFlagsEXT
                            conNamePipelineRasterizationDepthClipStateCreateFlagsEXT
                            (\(PipelineRasterizationDepthClipStateCreateFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineRasterizationDepthClipStateCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixPipelineRasterizationDepthClipStateCreateFlagsEXT
                          showTablePipelineRasterizationDepthClipStateCreateFlagsEXT
                          conNamePipelineRasterizationDepthClipStateCreateFlagsEXT
                          PipelineRasterizationDepthClipStateCreateFlagsEXT


type EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION"
pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_CLIP_ENABLE_SPEC_VERSION = 1


type EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME = "VK_EXT_depth_clip_enable"

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME"
pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_CLIP_ENABLE_EXTENSION_NAME = "VK_EXT_depth_clip_enable"

