{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_data_graph_model - device extension
--
-- = VK_QCOM_data_graph_model
--
-- [__Name String__]
--     @VK_QCOM_data_graph_model@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     630
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_data_graph_model] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_data_graph_model extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_data_graph_model.adoc VK_QCOM_data_graph_model>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-24
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_ARM_tensors@
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Rob VanReenen, Qualcomm Technologies, Inc
--
--     -   Balaji Calidas, Qualcomm Technologies, Inc
--
--     -   Jacob Yenney, Qualcomm Technologies, Inc
--
--     -   Kévin Petit, Arm Ltd.
--
-- == Description
--
-- This extension supports new
-- 'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphProcessingEngineTypeARM',
-- and
-- 'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationTypeARM'
-- types for data graph pipelines added in @VK_ARM_data_graph@.
--
-- A new pipeline cache type is also added to seamlessly import ML models
-- such as ONNX through QNN workflow, and run them on the device or an
-- external compute engine.
--
-- == New Structures
--
-- -   'PipelineCacheHeaderVersionDataGraphQCOM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM':
--
--     -   'DataGraphPipelineBuiltinModelCreateInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDataGraphModelFeaturesQCOM'
--
-- == New Enums
--
-- -   'DataGraphModelCacheTypeQCOM'
--
-- == New Enum Constants
--
-- -   'Vulkan.Core10.APIConstants.DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM'
--
-- -   'QCOM_DATA_GRAPH_MODEL_EXTENSION_NAME'
--
-- -   'QCOM_DATA_GRAPH_MODEL_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationTypeARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_BUILTIN_MODEL_QCOM'
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_NEURAL_MODEL_QCOM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphProcessingEngineTypeARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM'
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion':
--
--     -   'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PIPELINE_CACHE_HEADER_VERSION_DATA_GRAPH_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_BUILTIN_MODEL_CREATE_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_MODEL_FEATURES_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2025-06-24 (Matthew Netsch)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_data_graph_model Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_data_graph_model  ( PipelineCacheHeaderVersionDataGraphQCOM(..)
                                                   , DataGraphPipelineBuiltinModelCreateInfoQCOM(..)
                                                   , PhysicalDeviceDataGraphModelFeaturesQCOM(..)
                                                   , DataGraphModelCacheTypeQCOM( DATA_GRAPH_MODEL_CACHE_TYPE_GENERIC_BINARY_QCOM
                                                                                , ..
                                                                                )
                                                   , QCOM_DATA_GRAPH_MODEL_SPEC_VERSION
                                                   , pattern QCOM_DATA_GRAPH_MODEL_SPEC_VERSION
                                                   , QCOM_DATA_GRAPH_MODEL_EXTENSION_NAME
                                                   , pattern QCOM_DATA_GRAPH_MODEL_EXTENSION_NAME
                                                   , PhysicalDeviceDataGraphOperationSupportARM(..)
                                                   , PhysicalDeviceDataGraphProcessingEngineTypeARM(..)
                                                   , PhysicalDeviceDataGraphOperationTypeARM(..)
                                                   , MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                                   , pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                                   , DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM
                                                   , pattern DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM
                                                   ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Monad (unless)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.APIConstants (DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM)
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphOperationSupportARM)
import Vulkan.Core10.Enums.PipelineCacheHeaderVersion (PipelineCacheHeaderVersion)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (pattern DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_BUILTIN_MODEL_CREATE_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_MODEL_FEATURES_QCOM))
import Vulkan.Core10.APIConstants (DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM)
import Vulkan.Core10.APIConstants (MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphOperationSupportARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphOperationTypeARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphProcessingEngineTypeARM(..))
import Vulkan.Core10.APIConstants (pattern DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM)
import Vulkan.Core10.APIConstants (pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
-- | VkPipelineCacheHeaderVersionDataGraphQCOM - Structure describing the
-- layout of the pipeline cache header for data graphs
--
-- = Description
--
-- The application /should/ verify that the header info is compatible with
-- the
-- 'Vulkan.Extensions.VK_ARM_data_graph.DataGraphProcessingEngineCreateInfoARM'
-- passed during pipeline creation. Implementations /may/ return
-- 'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PIPELINE_COMPILE_REQUIRED_EXT'
-- from 'Vulkan.Extensions.VK_ARM_data_graph.createDataGraphPipelinesARM'
-- if the cache is not compatible.
--
-- This cache type is built using offline compilation, therefore Vulkan
-- does not define engine compatibility. The application should refer to
-- the offline compiler used to create the cache for guidance on
-- compatibility.
--
-- Unlike most structures declared by the Vulkan API, all fields of this
-- structure are written with the least significant byte first, regardless
-- of host byte-order.
--
-- The C language specification does not define the packing of structure
-- members. This layout assumes tight structure member packing, with
-- members laid out in the order listed in the structure, and the intended
-- size of the structure is 28 bytes. If a compiler produces code that
-- diverges from that pattern, applications /must/ employ another method to
-- set values at the correct offsets.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineCacheHeaderVersionDataGraphQCOM-None-11835# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dataGraphModelQCOM dataGraphModel>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineCacheHeaderVersionDataGraphQCOM-headerSize-11836#
--     @headerSize@ /must/ be 28
--
-- -   #VUID-VkPipelineCacheHeaderVersionDataGraphQCOM-headerVersion-11837#
--     @headerVersion@ /must/ be
--     'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PIPELINE_CACHE_HEADER_VERSION_DATA_GRAPH_QCOM'
--
-- -   #VUID-VkPipelineCacheHeaderVersionDataGraphQCOM-headerSize-11838#
--     @headerSize@ /must/ not exceed the size of the pipeline cache
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineCacheHeaderVersionDataGraphQCOM-headerVersion-parameter#
--     @headerVersion@ /must/ be a valid
--     'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
--     value
--
-- -   #VUID-VkPipelineCacheHeaderVersionDataGraphQCOM-cacheType-parameter#
--     @cacheType@ /must/ be a valid 'DataGraphModelCacheTypeQCOM' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_data_graph_model VK_QCOM_data_graph_model>,
-- 'DataGraphModelCacheTypeQCOM',
-- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
data PipelineCacheHeaderVersionDataGraphQCOM = PipelineCacheHeaderVersionDataGraphQCOM
  { -- | @headerSize@ is the length in bytes of the pipeline cache header.
    headerSize :: Word32
  , -- | @headerVersion@ is a
    -- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
    -- value specifying the version of the header. A consumer of the pipeline
    -- cache /should/ use the cache version to interpret the remainder of the
    -- cache header. @headerVersion@ /must/ be written as exactly 4 bytes.
    headerVersion :: PipelineCacheHeaderVersion
  , -- | @cacheType@ is the 'DataGraphModelCacheTypeQCOM' type of data graph
    -- cache encoded in the data.
    cacheType :: DataGraphModelCacheTypeQCOM
  , -- | @cacheVersion@ is the version of the encoding of the data graph cache.
    cacheVersion :: Word32
  , -- | @toolchainVersion@ is a null-terminated UTF-8 string specifying the
    -- version of the compiler that built the data graph cache.
    toolchainVersion :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCacheHeaderVersionDataGraphQCOM)
#endif
deriving instance Show PipelineCacheHeaderVersionDataGraphQCOM

instance ToCStruct PipelineCacheHeaderVersionDataGraphQCOM where
  withCStruct x f = allocaBytes 28 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCacheHeaderVersionDataGraphQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (headerSize)
    poke ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion)) (headerVersion)
    poke ((p `plusPtr` 8 :: Ptr DataGraphModelCacheTypeQCOM)) (cacheType)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (cacheVersion)
    unless ((Data.Vector.length $ (toolchainVersion)) <= DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM) $
      throwIO $ IOError Nothing InvalidArgument "" "toolchainVersion is too long, a maximum of DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM Word32)))) `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (toolchainVersion)
    f
  cStructSize = 28
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DataGraphModelCacheTypeQCOM)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineCacheHeaderVersionDataGraphQCOM where
  peekCStruct p = do
    headerSize <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    headerVersion <- peek @PipelineCacheHeaderVersion ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion))
    cacheType <- peek @DataGraphModelCacheTypeQCOM ((p `plusPtr` 8 :: Ptr DataGraphModelCacheTypeQCOM))
    cacheVersion <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    toolchainVersion <- generateM (DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM) (\i -> peek @Word32 (((lowerArrayPtr @Word32 ((p `plusPtr` 16 :: Ptr (FixedArray DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM Word32)))) `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ PipelineCacheHeaderVersionDataGraphQCOM
             headerSize headerVersion cacheType cacheVersion toolchainVersion

instance Storable PipelineCacheHeaderVersionDataGraphQCOM where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCacheHeaderVersionDataGraphQCOM where
  zero = PipelineCacheHeaderVersionDataGraphQCOM
           zero
           zero
           zero
           zero
           mempty


-- | VkDataGraphPipelineBuiltinModelCreateInfoQCOM - Structure specifying a
-- built-in model for the newly created data graph pipeline
--
-- = Description
--
-- The @pipelineCache@ is ignored for the creation of this pipeline.
--
-- Applications /can/ specify arguments to the built-in operation named in
-- @pOperation@ with
-- 'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCompilerControlCreateInfoARM'.
--
-- Applications /should/ verify that the @pVendorOptions@, @layout@, and
-- other state included with this pipeline creation are compatible with the
-- @pOperation@. Implementations /may/ fail if any state is not compatible
-- and return 'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED'.
--
-- Built-in models are defined by the provider of the model, therefore
-- Vulkan does not define model compatibility. The application should refer
-- to the provider of the built-in model for guidance on compatibility.
--
-- == Valid Usage
--
-- -   #VUID-VkDataGraphPipelineBuiltinModelCreateInfoQCOM-pOperation-11842#
--     All members of @pOperation@ /must/ be identical to a
--     'Vulkan.Extensions.VK_ARM_data_graph.QueueFamilyDataGraphPropertiesARM'::@operation@
--     retrieved from
--     'Vulkan.Extensions.VK_ARM_data_graph.getPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
--     with the @physicalDevice@ that was used to create @device@ and
--     paired in the retrieved results with a
--     'Vulkan.Extensions.VK_ARM_data_graph.QueueFamilyDataGraphPropertiesARM'::@engine@
--     identical to an element of
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphProcessingEngineCreateInfoARM'::@pProcessingEngines@
--     provided in the @pNext@ chain
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDataGraphPipelineBuiltinModelCreateInfoQCOM-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_BUILTIN_MODEL_CREATE_INFO_QCOM'
--
-- -   #VUID-VkDataGraphPipelineBuiltinModelCreateInfoQCOM-pOperation-parameter#
--     @pOperation@ /must/ be a valid pointer to a valid
--     'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationSupportARM'
--     structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_data_graph_model VK_QCOM_data_graph_model>,
-- 'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationSupportARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineBuiltinModelCreateInfoQCOM = DataGraphPipelineBuiltinModelCreateInfoQCOM
  { -- | @pOperation@ is a
    -- 'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationSupportARM'
    -- specifying the built-in operation.
    operation :: PhysicalDeviceDataGraphOperationSupportARM }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineBuiltinModelCreateInfoQCOM)
#endif
deriving instance Show DataGraphPipelineBuiltinModelCreateInfoQCOM

instance ToCStruct DataGraphPipelineBuiltinModelCreateInfoQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineBuiltinModelCreateInfoQCOM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_BUILTIN_MODEL_CREATE_INFO_QCOM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pOperation'' <- ContT $ withCStruct (operation)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr PhysicalDeviceDataGraphOperationSupportARM))) pOperation''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_BUILTIN_MODEL_CREATE_INFO_QCOM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pOperation'' <- ContT $ withCStruct (zero)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr PhysicalDeviceDataGraphOperationSupportARM))) pOperation''
    lift $ f

instance FromCStruct DataGraphPipelineBuiltinModelCreateInfoQCOM where
  peekCStruct p = do
    pOperation <- peekCStruct @PhysicalDeviceDataGraphOperationSupportARM =<< peek ((p `plusPtr` 16 :: Ptr (Ptr PhysicalDeviceDataGraphOperationSupportARM)))
    pure $ DataGraphPipelineBuiltinModelCreateInfoQCOM
             pOperation

instance Zero DataGraphPipelineBuiltinModelCreateInfoQCOM where
  zero = DataGraphPipelineBuiltinModelCreateInfoQCOM
           zero


-- | VkPhysicalDeviceDataGraphModelFeaturesQCOM - Structure describing
-- features to control data graph model
--
-- = Description
--
-- If the 'PhysicalDeviceDataGraphModelFeaturesQCOM' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDataGraphModelFeaturesQCOM', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_data_graph_model VK_QCOM_data_graph_model>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDataGraphModelFeaturesQCOM = PhysicalDeviceDataGraphModelFeaturesQCOM
  { -- | #features-dataGraphModelQCOM# @dataGraphModel@ specifies whether the
    -- functionality defined by this extension is available, and guarantees
    -- that the implementation supports a data graph queue family with at least
    -- one of the following engine types:
    --
    -- -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM'
    --
    -- -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM'
    dataGraphModel :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDataGraphModelFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceDataGraphModelFeaturesQCOM

instance ToCStruct PhysicalDeviceDataGraphModelFeaturesQCOM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDataGraphModelFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_MODEL_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dataGraphModel))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_MODEL_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDataGraphModelFeaturesQCOM where
  peekCStruct p = do
    dataGraphModel <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDataGraphModelFeaturesQCOM
             (bool32ToBool dataGraphModel)

instance Storable PhysicalDeviceDataGraphModelFeaturesQCOM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDataGraphModelFeaturesQCOM where
  zero = PhysicalDeviceDataGraphModelFeaturesQCOM
           zero


-- | VkDataGraphModelCacheTypeQCOM - Encode pipeline data graph cache type
--
-- = Description
--
-- -   'DATA_GRAPH_MODEL_CACHE_TYPE_GENERIC_BINARY_QCOM' specifies a
--     general binary layout type.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_data_graph_model VK_QCOM_data_graph_model>,
-- 'PipelineCacheHeaderVersionDataGraphQCOM'
newtype DataGraphModelCacheTypeQCOM = DataGraphModelCacheTypeQCOM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDataGraphModelCacheTypeQCOM" "VK_DATA_GRAPH_MODEL_CACHE_TYPE_GENERIC_BINARY_QCOM"
pattern DATA_GRAPH_MODEL_CACHE_TYPE_GENERIC_BINARY_QCOM = DataGraphModelCacheTypeQCOM 0

{-# COMPLETE DATA_GRAPH_MODEL_CACHE_TYPE_GENERIC_BINARY_QCOM :: DataGraphModelCacheTypeQCOM #-}

conNameDataGraphModelCacheTypeQCOM :: String
conNameDataGraphModelCacheTypeQCOM = "DataGraphModelCacheTypeQCOM"

enumPrefixDataGraphModelCacheTypeQCOM :: String
enumPrefixDataGraphModelCacheTypeQCOM = "DATA_GRAPH_MODEL_CACHE_TYPE_GENERIC_BINARY_QCOM"

showTableDataGraphModelCacheTypeQCOM :: [(DataGraphModelCacheTypeQCOM, String)]
showTableDataGraphModelCacheTypeQCOM =
  [
    ( DATA_GRAPH_MODEL_CACHE_TYPE_GENERIC_BINARY_QCOM
    , ""
    )
  ]

instance Show DataGraphModelCacheTypeQCOM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphModelCacheTypeQCOM
      showTableDataGraphModelCacheTypeQCOM
      conNameDataGraphModelCacheTypeQCOM
      (\(DataGraphModelCacheTypeQCOM x) -> x)
      (showsPrec 11)

instance Read DataGraphModelCacheTypeQCOM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphModelCacheTypeQCOM
      showTableDataGraphModelCacheTypeQCOM
      conNameDataGraphModelCacheTypeQCOM
      DataGraphModelCacheTypeQCOM

type QCOM_DATA_GRAPH_MODEL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_QCOM_DATA_GRAPH_MODEL_SPEC_VERSION"
pattern QCOM_DATA_GRAPH_MODEL_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_DATA_GRAPH_MODEL_SPEC_VERSION = 1


type QCOM_DATA_GRAPH_MODEL_EXTENSION_NAME = "VK_QCOM_data_graph_model"

-- No documentation found for TopLevel "VK_QCOM_DATA_GRAPH_MODEL_EXTENSION_NAME"
pattern QCOM_DATA_GRAPH_MODEL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_DATA_GRAPH_MODEL_EXTENSION_NAME = "VK_QCOM_data_graph_model"

