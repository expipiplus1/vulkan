{-# language CPP #-}
-- | = Name
--
-- VK_EXT_graphics_pipeline_library - device extension
--
-- == VK_EXT_graphics_pipeline_library
--
-- [__Name String__]
--     @VK_EXT_graphics_pipeline_library@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     321
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   Requires @VK_KHR_pipeline_library@
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_graphics_pipeline_library] @tobski%0A<<Here describe the issue or question you have about the VK_EXT_graphics_pipeline_library extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_graphics_pipeline_library.asciidoc VK_EXT_graphics_pipeline_library>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-08-17
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Chris Glover, Google
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Piers Daniell, NVidia
--
--     -   Boris Zanin, Mobica
--
--     -   Krzysztof Niski, NVidia
--
--     -   Dan Ginsburg, Valve
--
--     -   Sebastian Aaltonen, Unity
--
--     -   Arseny Kapoulkine, Roblox
--
--     -   Calle Lejdfors, Ubisoft
--
--     -   Tiago Rodrigues, Ubisoft
--
--     -   Francois Duranleau, Gameloft
--
-- == Description
--
-- This extension allows the separate compilation of four distinct parts of
-- graphics pipelines, with the intent of allowing faster pipeline loading
-- for applications reusing the same shaders or state in multiple
-- pipelines. Each part can be independently compiled into a graphics
-- pipeline library, with a final link step required to create an
-- executable pipeline that can be bound to a command buffer.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'GraphicsPipelineLibraryCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT'
--
-- == New Enums
--
-- -   'GraphicsPipelineLibraryFlagBitsEXT'
--
-- -   'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlagBits'
--
-- == New Bitmasks
--
-- -   'GraphicsPipelineLibraryFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_GRAPHICS_PIPELINE_LIBRARY_EXTENSION_NAME'
--
-- -   'EXT_GRAPHICS_PIPELINE_LIBRARY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_LIBRARY_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-08-17 (Tobias Hector)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'GraphicsPipelineLibraryCreateInfoEXT',
-- 'GraphicsPipelineLibraryFlagBitsEXT', 'GraphicsPipelineLibraryFlagsEXT',
-- 'PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT',
-- 'PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT',
-- 'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PipelineLayoutCreateFlagBits'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_graphics_pipeline_library Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_graphics_pipeline_library  ( PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT(..)
                                                           , PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT(..)
                                                           , GraphicsPipelineLibraryCreateInfoEXT(..)
                                                           , GraphicsPipelineLibraryFlagsEXT
                                                           , GraphicsPipelineLibraryFlagBitsEXT( GRAPHICS_PIPELINE_LIBRARY_VERTEX_INPUT_INTERFACE_BIT_EXT
                                                                                               , GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT
                                                                                               , GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT
                                                                                               , GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT
                                                                                               , ..
                                                                                               )
                                                           , EXT_GRAPHICS_PIPELINE_LIBRARY_SPEC_VERSION
                                                           , pattern EXT_GRAPHICS_PIPELINE_LIBRARY_SPEC_VERSION
                                                           , EXT_GRAPHICS_PIPELINE_LIBRARY_EXTENSION_NAME
                                                           , pattern EXT_GRAPHICS_PIPELINE_LIBRARY_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_PIPELINE_LIBRARY_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_PROPERTIES_EXT))
-- | VkPhysicalDeviceGraphicsPipelineLibraryFeaturesEXT - Structure
-- describing support for graphics pipeline libraries
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_graphics_pipeline_library VK_EXT_graphics_pipeline_library>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT = PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT
  { -- | #features-graphicsPipelineLibrary# @graphicsPipelineLibrary@ indicates
    -- that the implementation supports
    -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-library graphics pipeline libraries>.
    graphicsPipelineLibrary :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT

instance ToCStruct PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (graphicsPipelineLibrary))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT where
  peekCStruct p = do
    graphicsPipelineLibrary <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT
             (bool32ToBool graphicsPipelineLibrary)

instance Storable PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT where
  zero = PhysicalDeviceGraphicsPipelineLibraryFeaturesEXT
           zero


-- | VkPhysicalDeviceGraphicsPipelineLibraryPropertiesEXT - Structure
-- describing additional properties of graphics pipeline libraries
--
-- = Description
--
-- If the 'PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_graphics_pipeline_library VK_EXT_graphics_pipeline_library>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT = PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT
  { -- | #limits-graphicsPipelineLibraryFastLinking#
    -- @graphicsPipelineLibraryFastLinking@ indicates whether fast linking of
    -- graphics pipelines is supported. If it is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', creating a graphics pipeline
    -- entirely from pipeline libraries without
    -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT'
    -- is comparable in cost to recording a command in a command buffer.
    graphicsPipelineLibraryFastLinking :: Bool
  , -- | #limits-graphicsPipelineLibraryIndependentInterpolationDecoration#
    -- @graphicsPipelineLibraryIndependentInterpolationDecoration@ indicates
    -- whether @NoPerspective@ and @Flat@ interpolation decorations in the last
    -- vertex processing stage and the fragment shader are required to match
    -- when using graphics pipeline libraries. If it is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE', the interpolation decorations do
    -- not need to match. If it is 'Vulkan.Core10.FundamentalTypes.FALSE',
    -- these decorations /must/ either be present in both stages or neither
    -- stage in order for a given interface variable to match.
    graphicsPipelineLibraryIndependentInterpolationDecoration :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT

instance ToCStruct PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (graphicsPipelineLibraryFastLinking))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (graphicsPipelineLibraryIndependentInterpolationDecoration))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GRAPHICS_PIPELINE_LIBRARY_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT where
  peekCStruct p = do
    graphicsPipelineLibraryFastLinking <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    graphicsPipelineLibraryIndependentInterpolationDecoration <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT
             (bool32ToBool graphicsPipelineLibraryFastLinking) (bool32ToBool graphicsPipelineLibraryIndependentInterpolationDecoration)

instance Storable PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT where
  zero = PhysicalDeviceGraphicsPipelineLibraryPropertiesEXT
           zero
           zero


-- | VkGraphicsPipelineLibraryCreateInfoEXT - Structure specifying the
-- subsets of the graphics pipeline being compiled
--
-- = Description
--
-- If a 'GraphicsPipelineLibraryCreateInfoEXT' structure is included in the
-- @pNext@ chain of 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo', it
-- specifies the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-graphics-subsets subsets of the graphics pipeline>
-- being created.
--
-- If this structure is omitted, and either
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@flags@ includes
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
-- or the 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo'::@pNext@
-- chain includes a
-- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
-- structure with a @libraryCount@ greater than @0@, it is as if @flags@ is
-- @0@. Otherwise if this structure is omitted, it is as if @flags@
-- includes all possible subsets of the graphics pipeline (i.e. a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-graphics-subsets-complete complete graphics pipeline>).
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_graphics_pipeline_library VK_EXT_graphics_pipeline_library>,
-- 'GraphicsPipelineLibraryFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GraphicsPipelineLibraryCreateInfoEXT = GraphicsPipelineLibraryCreateInfoEXT
  { -- | @flags@ is a bitmask of 'GraphicsPipelineLibraryFlagBitsEXT' specifying
    -- the subsets of the graphics pipeline that are being compiled.
    --
    -- #VUID-VkGraphicsPipelineLibraryCreateInfoEXT-flags-parameter# @flags@
    -- /must/ be a valid combination of 'GraphicsPipelineLibraryFlagBitsEXT'
    -- values
    --
    -- #VUID-VkGraphicsPipelineLibraryCreateInfoEXT-flags-requiredbitmask#
    -- @flags@ /must/ not be @0@
    flags :: GraphicsPipelineLibraryFlagsEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsPipelineLibraryCreateInfoEXT)
#endif
deriving instance Show GraphicsPipelineLibraryCreateInfoEXT

instance ToCStruct GraphicsPipelineLibraryCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsPipelineLibraryCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_LIBRARY_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GraphicsPipelineLibraryFlagsEXT)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_LIBRARY_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GraphicsPipelineLibraryFlagsEXT)) (zero)
    f

instance FromCStruct GraphicsPipelineLibraryCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @GraphicsPipelineLibraryFlagsEXT ((p `plusPtr` 16 :: Ptr GraphicsPipelineLibraryFlagsEXT))
    pure $ GraphicsPipelineLibraryCreateInfoEXT
             flags

instance Storable GraphicsPipelineLibraryCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GraphicsPipelineLibraryCreateInfoEXT where
  zero = GraphicsPipelineLibraryCreateInfoEXT
           zero


type GraphicsPipelineLibraryFlagsEXT = GraphicsPipelineLibraryFlagBitsEXT

-- | VkGraphicsPipelineLibraryFlagBitsEXT - Bitmask specifying the subset of
-- a graphics pipeline to compile
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_graphics_pipeline_library VK_EXT_graphics_pipeline_library>,
-- 'GraphicsPipelineLibraryFlagsEXT'
newtype GraphicsPipelineLibraryFlagBitsEXT = GraphicsPipelineLibraryFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'GRAPHICS_PIPELINE_LIBRARY_VERTEX_INPUT_INTERFACE_BIT_EXT' specifies
-- that a pipeline will include
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-graphics-subsets-vertex-input vertex input interface state>.
pattern GRAPHICS_PIPELINE_LIBRARY_VERTEX_INPUT_INTERFACE_BIT_EXT    = GraphicsPipelineLibraryFlagBitsEXT 0x00000001
-- | 'GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT' specifies
-- that a pipeline will include
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-graphics-subsets-pre-rasterization pre-rasterization shader state>.
pattern GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT = GraphicsPipelineLibraryFlagBitsEXT 0x00000002
-- | 'GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT' specifies that a
-- pipeline will include
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-graphics-subsets-fragment-shader fragment shader state>.
pattern GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT           = GraphicsPipelineLibraryFlagBitsEXT 0x00000004
-- | 'GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT' specifies
-- that a pipeline will include
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-graphics-subsets-fragment-output fragment output interface state>.
pattern GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT = GraphicsPipelineLibraryFlagBitsEXT 0x00000008

conNameGraphicsPipelineLibraryFlagBitsEXT :: String
conNameGraphicsPipelineLibraryFlagBitsEXT = "GraphicsPipelineLibraryFlagBitsEXT"

enumPrefixGraphicsPipelineLibraryFlagBitsEXT :: String
enumPrefixGraphicsPipelineLibraryFlagBitsEXT = "GRAPHICS_PIPELINE_LIBRARY_"

showTableGraphicsPipelineLibraryFlagBitsEXT :: [(GraphicsPipelineLibraryFlagBitsEXT, String)]
showTableGraphicsPipelineLibraryFlagBitsEXT =
  [ (GRAPHICS_PIPELINE_LIBRARY_VERTEX_INPUT_INTERFACE_BIT_EXT   , "VERTEX_INPUT_INTERFACE_BIT_EXT")
  , (GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT, "PRE_RASTERIZATION_SHADERS_BIT_EXT")
  , (GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT          , "FRAGMENT_SHADER_BIT_EXT")
  , (GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT, "FRAGMENT_OUTPUT_INTERFACE_BIT_EXT")
  ]

instance Show GraphicsPipelineLibraryFlagBitsEXT where
  showsPrec = enumShowsPrec enumPrefixGraphicsPipelineLibraryFlagBitsEXT
                            showTableGraphicsPipelineLibraryFlagBitsEXT
                            conNameGraphicsPipelineLibraryFlagBitsEXT
                            (\(GraphicsPipelineLibraryFlagBitsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read GraphicsPipelineLibraryFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixGraphicsPipelineLibraryFlagBitsEXT
                          showTableGraphicsPipelineLibraryFlagBitsEXT
                          conNameGraphicsPipelineLibraryFlagBitsEXT
                          GraphicsPipelineLibraryFlagBitsEXT


type EXT_GRAPHICS_PIPELINE_LIBRARY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_GRAPHICS_PIPELINE_LIBRARY_SPEC_VERSION"
pattern EXT_GRAPHICS_PIPELINE_LIBRARY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_GRAPHICS_PIPELINE_LIBRARY_SPEC_VERSION = 1


type EXT_GRAPHICS_PIPELINE_LIBRARY_EXTENSION_NAME = "VK_EXT_graphics_pipeline_library"

-- No documentation found for TopLevel "VK_EXT_GRAPHICS_PIPELINE_LIBRARY_EXTENSION_NAME"
pattern EXT_GRAPHICS_PIPELINE_LIBRARY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_GRAPHICS_PIPELINE_LIBRARY_EXTENSION_NAME = "VK_EXT_graphics_pipeline_library"

