{-# language CPP #-}
-- | = Name
--
-- VK_ARM_data_graph_neural_accelerator_statistics - device extension
--
-- = VK_ARM_data_graph_neural_accelerator_statistics
--
-- [__Name String__]
--     @VK_ARM_data_graph_neural_accelerator_statistics@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     677
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_data_graph_neural_accelerator_statistics] @kpet%0A*Here describe the issue or question you have about the VK_ARM_data_graph_neural_accelerator_statistics extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-04-28
--
-- [__Interactions and External Dependencies__]
--
--     -   None
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Emma Lynn Mulier (Benyossef), Arm Ltd.
--
-- == Description
--
-- This extension adds support for getting data graph execution statistics
-- for Arm neural accelerators.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM':
--
--     -   'DataGraphPipelineNeuralStatisticsCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionCreateInfoARM':
--
--     -   'DataGraphPipelineSessionNeuralStatisticsCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM'
--
-- == New Enums
--
-- -   'NeuralAcceleratorStatisticsModeARM'
--
-- == New Enum Constants
--
-- -   'ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_EXTENSION_NAME'
--
-- -   'ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelinePropertyARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_PROPERTY_NEURAL_ACCELERATOR_DEBUG_DATABASE_ARM'
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_PROPERTY_NEURAL_ACCELERATOR_STATISTICS_INFO_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionBindPointARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_NEURAL_ACCELERATOR_STATISTICS_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_NEURAL_STATISTICS_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_NEURAL_STATISTICS_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_FEATURES_ARM'
--
-- == New SPIR-V Capabilities
--
-- None.
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2026-04-28 (Kévin Petit)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_data_graph_neural_accelerator_statistics Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_data_graph_neural_accelerator_statistics  ( PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM(..)
                                                                          , DataGraphPipelineNeuralStatisticsCreateInfoARM(..)
                                                                          , DataGraphPipelineSessionNeuralStatisticsCreateInfoARM(..)
                                                                          , NeuralAcceleratorStatisticsModeARM( NEURAL_ACCELERATOR_STATISTICS_MODE_DISABLED_ARM
                                                                                                              , NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS0_ARM
                                                                                                              , NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS1_ARM
                                                                                                              , ..
                                                                                                              )
                                                                          , ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_SPEC_VERSION
                                                                          , pattern ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_SPEC_VERSION
                                                                          , ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_EXTENSION_NAME
                                                                          , pattern ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_EXTENSION_NAME
                                                                          , DataGraphPipelineSessionBindPointARM(..)
                                                                          , DataGraphPipelinePropertyARM(..)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_NEURAL_STATISTICS_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_NEURAL_STATISTICS_CREATE_INFO_ARM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_FEATURES_ARM))
import Vulkan.Extensions.VK_ARM_data_graph (DataGraphPipelinePropertyARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (DataGraphPipelineSessionBindPointARM(..))
-- | VkPhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM -
-- Structure describing features to control data graph neural accelerator
-- statistics
--
-- = Description
--
-- If the 'PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM'
-- structure is included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM', it
-- /must/ add an instance of the structure, with the desired feature
-- members set to 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@
-- chain of 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_neural_accelerator_statistics VK_ARM_data_graph_neural_accelerator_statistics>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM = PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM
  { -- | #features-dataGraphNeuralAcceleratorStatistics#
    -- @dataGraphNeuralAcceleratorStatistics@ indicates that the implementation
    -- supports gathering neural accelerator statistics for data graphs.
    dataGraphNeuralAcceleratorStatistics :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM)
#endif
deriving instance Show PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM

instance ToCStruct PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dataGraphNeuralAcceleratorStatistics))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_FEATURES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM where
  peekCStruct p = do
    dataGraphNeuralAcceleratorStatistics <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM
             (bool32ToBool dataGraphNeuralAcceleratorStatistics)

instance Storable PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM where
  zero = PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM
           zero


-- | VkDataGraphPipelineNeuralStatisticsCreateInfoARM - Structure specifying
-- neural statistics parameters of a newly created graph pipeline
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_neural_accelerator_statistics VK_ARM_data_graph_neural_accelerator_statistics>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineNeuralStatisticsCreateInfoARM = DataGraphPipelineNeuralStatisticsCreateInfoARM
  { -- | @allowNeuralStatistics@ specifies whether sessions for the newly created
    -- pipeline /may/ enable neural statistics reporting.
    allowNeuralStatistics :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineNeuralStatisticsCreateInfoARM)
#endif
deriving instance Show DataGraphPipelineNeuralStatisticsCreateInfoARM

instance ToCStruct DataGraphPipelineNeuralStatisticsCreateInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineNeuralStatisticsCreateInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_NEURAL_STATISTICS_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (allowNeuralStatistics))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_NEURAL_STATISTICS_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct DataGraphPipelineNeuralStatisticsCreateInfoARM where
  peekCStruct p = do
    allowNeuralStatistics <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ DataGraphPipelineNeuralStatisticsCreateInfoARM
             (bool32ToBool allowNeuralStatistics)

instance Storable DataGraphPipelineNeuralStatisticsCreateInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineNeuralStatisticsCreateInfoARM where
  zero = DataGraphPipelineNeuralStatisticsCreateInfoARM
           zero


-- | VkDataGraphPipelineSessionNeuralStatisticsCreateInfoARM - Structure
-- specifying neural statistics parameters of a newly created data graph
-- pipeline session
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionCreateInfoARM'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_neural_accelerator_statistics VK_ARM_data_graph_neural_accelerator_statistics>,
-- 'NeuralAcceleratorStatisticsModeARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DataGraphPipelineSessionNeuralStatisticsCreateInfoARM = DataGraphPipelineSessionNeuralStatisticsCreateInfoARM
  { -- | @mode@ is a 'NeuralAcceleratorStatisticsModeARM' specifying the neural
    -- statistics mode for the session being created.
    --
    -- #VUID-VkDataGraphPipelineSessionNeuralStatisticsCreateInfoARM-mode-parameter#
    -- @mode@ /must/ be a valid 'NeuralAcceleratorStatisticsModeARM' value
    mode :: NeuralAcceleratorStatisticsModeARM }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphPipelineSessionNeuralStatisticsCreateInfoARM)
#endif
deriving instance Show DataGraphPipelineSessionNeuralStatisticsCreateInfoARM

instance ToCStruct DataGraphPipelineSessionNeuralStatisticsCreateInfoARM where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphPipelineSessionNeuralStatisticsCreateInfoARM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_NEURAL_STATISTICS_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NeuralAcceleratorStatisticsModeARM)) (mode)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_NEURAL_STATISTICS_CREATE_INFO_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr NeuralAcceleratorStatisticsModeARM)) (zero)
    f

instance FromCStruct DataGraphPipelineSessionNeuralStatisticsCreateInfoARM where
  peekCStruct p = do
    mode <- peek @NeuralAcceleratorStatisticsModeARM ((p `plusPtr` 16 :: Ptr NeuralAcceleratorStatisticsModeARM))
    pure $ DataGraphPipelineSessionNeuralStatisticsCreateInfoARM
             mode

instance Storable DataGraphPipelineSessionNeuralStatisticsCreateInfoARM where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphPipelineSessionNeuralStatisticsCreateInfoARM where
  zero = DataGraphPipelineSessionNeuralStatisticsCreateInfoARM
           zero


-- | VkNeuralAcceleratorStatisticsModeARM - Enum specifying the mode of
-- operation for neural accelerator statistics
--
-- = Description
--
-- -   'NEURAL_ACCELERATOR_STATISTICS_MODE_DISABLED_ARM' specifies that
--     neural accelerator statistics are disabled.
--
-- -   'NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS0_ARM' specifies the
--     @statistics0@ mode of operation.
--
-- -   'NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS1_ARM' specifies the
--     @statistics1@ mode of operation.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_neural_accelerator_statistics VK_ARM_data_graph_neural_accelerator_statistics>,
-- 'DataGraphPipelineSessionNeuralStatisticsCreateInfoARM'
newtype NeuralAcceleratorStatisticsModeARM = NeuralAcceleratorStatisticsModeARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkNeuralAcceleratorStatisticsModeARM" "VK_NEURAL_ACCELERATOR_STATISTICS_MODE_DISABLED_ARM"
pattern NEURAL_ACCELERATOR_STATISTICS_MODE_DISABLED_ARM = NeuralAcceleratorStatisticsModeARM 0

-- No documentation found for Nested "VkNeuralAcceleratorStatisticsModeARM" "VK_NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS0_ARM"
pattern NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS0_ARM = NeuralAcceleratorStatisticsModeARM 1

-- No documentation found for Nested "VkNeuralAcceleratorStatisticsModeARM" "VK_NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS1_ARM"
pattern NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS1_ARM = NeuralAcceleratorStatisticsModeARM 2

{-# COMPLETE
  NEURAL_ACCELERATOR_STATISTICS_MODE_DISABLED_ARM
  , NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS0_ARM
  , NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS1_ARM ::
    NeuralAcceleratorStatisticsModeARM
  #-}

conNameNeuralAcceleratorStatisticsModeARM :: String
conNameNeuralAcceleratorStatisticsModeARM = "NeuralAcceleratorStatisticsModeARM"

enumPrefixNeuralAcceleratorStatisticsModeARM :: String
enumPrefixNeuralAcceleratorStatisticsModeARM = "NEURAL_ACCELERATOR_STATISTICS_MODE_"

showTableNeuralAcceleratorStatisticsModeARM :: [(NeuralAcceleratorStatisticsModeARM, String)]
showTableNeuralAcceleratorStatisticsModeARM =
  [
    ( NEURAL_ACCELERATOR_STATISTICS_MODE_DISABLED_ARM
    , "DISABLED_ARM"
    )
  ,
    ( NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS0_ARM
    , "STATISTICS0_ARM"
    )
  ,
    ( NEURAL_ACCELERATOR_STATISTICS_MODE_STATISTICS1_ARM
    , "STATISTICS1_ARM"
    )
  ]

instance Show NeuralAcceleratorStatisticsModeARM where
  showsPrec =
    enumShowsPrec
      enumPrefixNeuralAcceleratorStatisticsModeARM
      showTableNeuralAcceleratorStatisticsModeARM
      conNameNeuralAcceleratorStatisticsModeARM
      (\(NeuralAcceleratorStatisticsModeARM x) -> x)
      (showsPrec 11)

instance Read NeuralAcceleratorStatisticsModeARM where
  readPrec =
    enumReadPrec
      enumPrefixNeuralAcceleratorStatisticsModeARM
      showTableNeuralAcceleratorStatisticsModeARM
      conNameNeuralAcceleratorStatisticsModeARM
      NeuralAcceleratorStatisticsModeARM

type ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_SPEC_VERSION"
pattern ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_SPEC_VERSION = 1


type ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_EXTENSION_NAME = "VK_ARM_data_graph_neural_accelerator_statistics"

-- No documentation found for TopLevel "VK_ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_EXTENSION_NAME"
pattern ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_EXTENSION_NAME = "VK_ARM_data_graph_neural_accelerator_statistics"

