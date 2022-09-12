{-# language CPP #-}
-- | = Name
--
-- VK_EXT_provoking_vertex - device extension
--
-- == VK_EXT_provoking_vertex
--
-- [__Name String__]
--     @VK_EXT_provoking_vertex@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     255
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_provoking_vertex] @jessehall%0A*Here describe the issue or question you have about the VK_EXT_provoking_vertex extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-02-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Alexis Hétu, Google
--
--     -   Bill Licea-Kane, Qualcomm
--
--     -   Daniel Koch, Nvidia
--
--     -   Jamie Madill, Google
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, Nvidia
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jesse Hall, Google
--
--     -   Jörg Wagner, Arm
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, Nvidia
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension allows changing the provoking vertex convention between
-- Vulkan’s default convention (first vertex) and OpenGL’s convention (last
-- vertex).
--
-- This extension is intended for use by API-translation layers that
-- implement APIs like OpenGL on top of Vulkan, and need to match the
-- source API’s provoking vertex convention. Applications using Vulkan
-- directly should use Vulkan’s default convention.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceProvokingVertexFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceProvokingVertexPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo':
--
--     -   'PipelineRasterizationProvokingVertexStateCreateInfoEXT'
--
-- == New Enums
--
-- -   'ProvokingVertexModeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PROVOKING_VERTEX_EXTENSION_NAME'
--
-- -   'EXT_PROVOKING_VERTEX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) At what granularity should this state be set?
--
-- __RESOLVED__: At pipeline bind, with an optional per-render pass
-- restriction.
--
-- The most natural place to put this state is in the graphics pipeline
-- object. Some implementations require it to be known when creating the
-- pipeline, and pipeline state is convenient for implementing OpenGL 3.2’s
-- glProvokingVertex, which can change the state between draw calls.
-- However, some implementations can only change it approximately render
-- pass granularity. To accommodate both, provoking vertex will be pipeline
-- state, but implementations can require that only one mode is used within
-- a render pass instance; the render pass’s mode is chosen implicitly when
-- the first pipeline is bound.
--
-- 2) Does the provoking vertex mode affect the order that vertices are
-- written to transform feedback buffers?
--
-- __RESOLVED__: Yes, to enable layered implementations of OpenGL and D3D.
--
-- All of OpenGL, OpenGL ES, and Direct3D 11 require that vertices are
-- written to transform feedback buffers such that flat-shaded attributes
-- have the same value when drawing the contents of the transform feedback
-- buffer as they did in the original drawing when the transform feedback
-- buffer was written (assuming the provoking vertex mode has not changed,
-- in APIs that support more than one mode).
--
-- == Version History
--
-- -   Revision 1, (1c) 2021-02-22 (Jesse Hall)
--
--     -   Added
--         VkPhysicalDeviceProvokingVertexPropertiesEXT::transformFeedbackPreservesTriangleFanProvokingVertex
--         to accommodate implementations that cannot change the transform
--         feedback vertex order for triangle fans.
--
-- -   Revision 1, (1b) 2020-06-14 (Jesse Hall)
--
--     -   Added
--         VkPhysicalDeviceProvokingVertexFeaturesEXT::transformFeedbackPreservesProvokingVertex
--         and required that transform feedback write vertices so as to
--         preserve the provoking vertex of each primitive.
--
-- -   Revision 1, (1a) 2019-10-23 (Jesse Hall)
--
--     -   Initial draft, based on a proposal by Alexis Hétu
--
-- == See Also
--
-- 'PhysicalDeviceProvokingVertexFeaturesEXT',
-- 'PhysicalDeviceProvokingVertexPropertiesEXT',
-- 'PipelineRasterizationProvokingVertexStateCreateInfoEXT',
-- 'ProvokingVertexModeEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_provoking_vertex Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_provoking_vertex  ( PhysicalDeviceProvokingVertexFeaturesEXT(..)
                                                  , PhysicalDeviceProvokingVertexPropertiesEXT(..)
                                                  , PipelineRasterizationProvokingVertexStateCreateInfoEXT(..)
                                                  , ProvokingVertexModeEXT( PROVOKING_VERTEX_MODE_FIRST_VERTEX_EXT
                                                                          , PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT
                                                                          , ..
                                                                          )
                                                  , EXT_PROVOKING_VERTEX_SPEC_VERSION
                                                  , pattern EXT_PROVOKING_VERTEX_SPEC_VERSION
                                                  , EXT_PROVOKING_VERTEX_EXTENSION_NAME
                                                  , pattern EXT_PROVOKING_VERTEX_EXTENSION_NAME
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
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
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT))
-- | VkPhysicalDeviceProvokingVertexFeaturesEXT - Structure describing the
-- provoking vertex features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceProvokingVertexFeaturesEXT' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceProvokingVertexFeaturesEXT' /can/ also be used
-- in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- When 'PhysicalDeviceProvokingVertexFeaturesEXT' is in the @pNext@ chain
-- of 'Vulkan.Core10.Device.DeviceCreateInfo' but the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
-- feature is not enabled, the value of
-- @transformFeedbackPreservesProvokingVertex@ is ignored.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_provoking_vertex VK_EXT_provoking_vertex>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceProvokingVertexFeaturesEXT = PhysicalDeviceProvokingVertexFeaturesEXT
  { -- | #features-provokingVertexLast# @provokingVertexLast@ indicates whether
    -- the implementation supports the 'PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT'
    -- <VkProvokingVertexModeEXT.html provoking vertex mode> for flat shading.
    provokingVertexLast :: Bool
  , -- | #features-transformFeedbackPreservesProvokingVertex#
    -- @transformFeedbackPreservesProvokingVertex@ indicates that the order of
    -- vertices within each primitive written by transform feedback will
    -- preserve the provoking vertex. This does not apply to triangle fan
    -- primitives when
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-transformFeedbackPreservesTriangleFanProvokingVertex transformFeedbackPreservesTriangleFanProvokingVertex>
    -- is 'Vulkan.Core10.FundamentalTypes.FALSE'.
    -- @transformFeedbackPreservesProvokingVertex@ /must/ be
    -- 'Vulkan.Core10.FundamentalTypes.FALSE' when the
    -- @VK_EXT_transform_feedback@ extension is not supported.
    transformFeedbackPreservesProvokingVertex :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceProvokingVertexFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceProvokingVertexFeaturesEXT

instance ToCStruct PhysicalDeviceProvokingVertexFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceProvokingVertexFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (provokingVertexLast))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (transformFeedbackPreservesProvokingVertex))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceProvokingVertexFeaturesEXT where
  peekCStruct p = do
    provokingVertexLast <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    transformFeedbackPreservesProvokingVertex <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceProvokingVertexFeaturesEXT
             (bool32ToBool provokingVertexLast) (bool32ToBool transformFeedbackPreservesProvokingVertex)

instance Storable PhysicalDeviceProvokingVertexFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceProvokingVertexFeaturesEXT where
  zero = PhysicalDeviceProvokingVertexFeaturesEXT
           zero
           zero


-- | VkPhysicalDeviceProvokingVertexPropertiesEXT - Structure describing
-- provoking vertex properties supported by an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceProvokingVertexPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_provoking_vertex VK_EXT_provoking_vertex>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceProvokingVertexPropertiesEXT = PhysicalDeviceProvokingVertexPropertiesEXT
  { -- | #limits-provokingVertexModePerPipeline# @provokingVertexModePerPipeline@
    -- indicates whether the implementation supports graphics pipelines with
    -- different provoking vertex modes within the same render pass instance.
    provokingVertexModePerPipeline :: Bool
  , -- | #limits-transformFeedbackPreservesTriangleFanProvokingVertex#
    -- @transformFeedbackPreservesTriangleFanProvokingVertex@ indicates whether
    -- the implementation can preserve the provoking vertex order when writing
    -- triangle fan vertices to transform feedback.
    transformFeedbackPreservesTriangleFanProvokingVertex :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceProvokingVertexPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceProvokingVertexPropertiesEXT

instance ToCStruct PhysicalDeviceProvokingVertexPropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceProvokingVertexPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (provokingVertexModePerPipeline))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (transformFeedbackPreservesTriangleFanProvokingVertex))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROVOKING_VERTEX_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceProvokingVertexPropertiesEXT where
  peekCStruct p = do
    provokingVertexModePerPipeline <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    transformFeedbackPreservesTriangleFanProvokingVertex <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceProvokingVertexPropertiesEXT
             (bool32ToBool provokingVertexModePerPipeline) (bool32ToBool transformFeedbackPreservesTriangleFanProvokingVertex)

instance Storable PhysicalDeviceProvokingVertexPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceProvokingVertexPropertiesEXT where
  zero = PhysicalDeviceProvokingVertexPropertiesEXT
           zero
           zero


-- | VkPipelineRasterizationProvokingVertexStateCreateInfoEXT - Structure
-- specifying provoking vertex mode used by a graphics pipeline
--
-- = Description
--
-- If this struct is not provided when creating the pipeline, the pipeline
-- will use the 'PROVOKING_VERTEX_MODE_FIRST_VERTEX_EXT' mode.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-provokingVertexModePerPipeline provokingVertexModePerPipeline>
-- limit is 'Vulkan.Core10.FundamentalTypes.FALSE', then all pipelines
-- bound within a render pass instance /must/ have the same
-- @provokingVertexMode@.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineRasterizationProvokingVertexStateCreateInfoEXT-provokingVertexMode-04883#
--     If @provokingVertexMode@ is 'PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT',
--     then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-provokingVertexLast provokingVertexLast>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineRasterizationProvokingVertexStateCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineRasterizationProvokingVertexStateCreateInfoEXT-provokingVertexMode-parameter#
--     @provokingVertexMode@ /must/ be a valid 'ProvokingVertexModeEXT'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_provoking_vertex VK_EXT_provoking_vertex>,
-- 'ProvokingVertexModeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationProvokingVertexStateCreateInfoEXT = PipelineRasterizationProvokingVertexStateCreateInfoEXT
  { -- | @provokingVertexMode@ is a 'ProvokingVertexModeEXT' value selecting the
    -- provoking vertex mode.
    provokingVertexMode :: ProvokingVertexModeEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationProvokingVertexStateCreateInfoEXT)
#endif
deriving instance Show PipelineRasterizationProvokingVertexStateCreateInfoEXT

instance ToCStruct PipelineRasterizationProvokingVertexStateCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationProvokingVertexStateCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ProvokingVertexModeEXT)) (provokingVertexMode)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_PROVOKING_VERTEX_STATE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ProvokingVertexModeEXT)) (zero)
    f

instance FromCStruct PipelineRasterizationProvokingVertexStateCreateInfoEXT where
  peekCStruct p = do
    provokingVertexMode <- peek @ProvokingVertexModeEXT ((p `plusPtr` 16 :: Ptr ProvokingVertexModeEXT))
    pure $ PipelineRasterizationProvokingVertexStateCreateInfoEXT
             provokingVertexMode

instance Storable PipelineRasterizationProvokingVertexStateCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineRasterizationProvokingVertexStateCreateInfoEXT where
  zero = PipelineRasterizationProvokingVertexStateCreateInfoEXT
           zero


-- | VkProvokingVertexModeEXT - Specify which vertex in a primitive is the
-- provoking vertex
--
-- = Description
--
-- These modes are described more precisely in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#drawing-primitive-topologies Primitive Topologies>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_provoking_vertex VK_EXT_provoking_vertex>,
-- 'PipelineRasterizationProvokingVertexStateCreateInfoEXT'
newtype ProvokingVertexModeEXT = ProvokingVertexModeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PROVOKING_VERTEX_MODE_FIRST_VERTEX_EXT' specifies that the provoking
-- vertex is the first non-adjacency vertex in the list of vertices used by
-- a primitive.
pattern PROVOKING_VERTEX_MODE_FIRST_VERTEX_EXT = ProvokingVertexModeEXT 0
-- | 'PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT' specifies that the provoking
-- vertex is the last non-adjacency vertex in the list of vertices used by
-- a primitive.
pattern PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT  = ProvokingVertexModeEXT 1
{-# complete PROVOKING_VERTEX_MODE_FIRST_VERTEX_EXT,
             PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT :: ProvokingVertexModeEXT #-}

conNameProvokingVertexModeEXT :: String
conNameProvokingVertexModeEXT = "ProvokingVertexModeEXT"

enumPrefixProvokingVertexModeEXT :: String
enumPrefixProvokingVertexModeEXT = "PROVOKING_VERTEX_MODE_"

showTableProvokingVertexModeEXT :: [(ProvokingVertexModeEXT, String)]
showTableProvokingVertexModeEXT =
  [ (PROVOKING_VERTEX_MODE_FIRST_VERTEX_EXT, "FIRST_VERTEX_EXT")
  , (PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT , "LAST_VERTEX_EXT")
  ]

instance Show ProvokingVertexModeEXT where
  showsPrec = enumShowsPrec enumPrefixProvokingVertexModeEXT
                            showTableProvokingVertexModeEXT
                            conNameProvokingVertexModeEXT
                            (\(ProvokingVertexModeEXT x) -> x)
                            (showsPrec 11)

instance Read ProvokingVertexModeEXT where
  readPrec = enumReadPrec enumPrefixProvokingVertexModeEXT
                          showTableProvokingVertexModeEXT
                          conNameProvokingVertexModeEXT
                          ProvokingVertexModeEXT


type EXT_PROVOKING_VERTEX_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PROVOKING_VERTEX_SPEC_VERSION"
pattern EXT_PROVOKING_VERTEX_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PROVOKING_VERTEX_SPEC_VERSION = 1


type EXT_PROVOKING_VERTEX_EXTENSION_NAME = "VK_EXT_provoking_vertex"

-- No documentation found for TopLevel "VK_EXT_PROVOKING_VERTEX_EXTENSION_NAME"
pattern EXT_PROVOKING_VERTEX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PROVOKING_VERTEX_EXTENSION_NAME = "VK_EXT_provoking_vertex"

