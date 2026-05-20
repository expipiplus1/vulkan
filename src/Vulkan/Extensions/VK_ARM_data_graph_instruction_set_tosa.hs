{-# language CPP #-}
-- | = Name
--
-- VK_ARM_data_graph_instruction_set_tosa - device extension
--
-- = VK_ARM_data_graph_instruction_set_tosa
--
-- [__Name String__]
--     @VK_ARM_data_graph_instruction_set_tosa@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     509
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
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_data_graph_instruction_set_tosa] @kpet%0A*Here describe the issue or question you have about the VK_ARM_data_graph_instruction_set_tosa extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-30
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://github.khronos.org/SPIRV-Registry//extended/TOSA.001000.1.html the TOSA SPIR-V 001000.1 extended instruction set>
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
-- == Description
--
-- This extensions adds support for the @TOSA.001000.1@ extended
-- instruction set for use in data graphs as defined by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>.
-- It also adds detailed queries to report the TOSA profiles, levels, and
-- extensions that are supported along with the quality of the
-- implementation (e.g. accelerated, experimental, deprecated, etc).
--
-- == New Commands
--
-- -   'getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM'
--
-- == New Structures
--
-- -   'DataGraphTOSANameQualityARM'
--
-- -   'QueueFamilyDataGraphTOSAPropertiesARM'
--
-- == New Enums
--
-- -   'DataGraphTOSALevelARM'
--
-- -   'DataGraphTOSAQualityFlagBitsARM'
--
-- == New Bitmasks
--
-- -   'DataGraphTOSAQualityFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_EXTENSION_NAME'
--
-- -   'ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_TOSA_PROPERTIES_ARM'
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
-- -   Revision 1, 2026-03-30 (Kévin Petit)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_data_graph_instruction_set_tosa Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_data_graph_instruction_set_tosa  ( getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM
                                                                 , DataGraphTOSANameQualityARM(..)
                                                                 , QueueFamilyDataGraphTOSAPropertiesARM(..)
                                                                 , DataGraphTOSAQualityFlagsARM
                                                                 , DataGraphTOSAQualityFlagBitsARM( DATA_GRAPH_TOSA_QUALITY_ACCELERATED_ARM
                                                                                                  , DATA_GRAPH_TOSA_QUALITY_CONFORMANT_ARM
                                                                                                  , DATA_GRAPH_TOSA_QUALITY_EXPERIMENTAL_ARM
                                                                                                  , DATA_GRAPH_TOSA_QUALITY_DEPRECATED_ARM
                                                                                                  , ..
                                                                                                  )
                                                                 , DataGraphTOSALevelARM( DATA_GRAPH_TOSA_LEVEL_NONE_ARM
                                                                                        , DATA_GRAPH_TOSA_LEVEL_8K_ARM
                                                                                        , ..
                                                                                        )
                                                                 , ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_SPEC_VERSION
                                                                 , pattern ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_SPEC_VERSION
                                                                 , ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_EXTENSION_NAME
                                                                 , pattern ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_EXTENSION_NAME
                                                                 , PhysicalDeviceDataGraphProcessingEngineARM(..)
                                                                 , PhysicalDeviceDataGraphOperationSupportARM(..)
                                                                 , QueueFamilyDataGraphPropertiesARM(..)
                                                                 , PhysicalDeviceDataGraphProcessingEngineTypeARM(..)
                                                                 , PhysicalDeviceDataGraphOperationTypeARM(..)
                                                                 , MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                                                 , pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM
                                                                 , MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM
                                                                 , pattern MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM
                                                                 ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (packCString)
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
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (BaseOutStructure)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM))
import Vulkan.Core10.APIConstants (MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Extensions.VK_ARM_data_graph (QueueFamilyDataGraphPropertiesARM)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_TOSA_PROPERTIES_ARM))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.APIConstants (MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM)
import Vulkan.Core10.APIConstants (MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphOperationSupportARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphOperationTypeARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphProcessingEngineARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (PhysicalDeviceDataGraphProcessingEngineTypeARM(..))
import Vulkan.Extensions.VK_ARM_data_graph (QueueFamilyDataGraphPropertiesARM(..))
import Vulkan.Core10.APIConstants (pattern MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM)
import Vulkan.Core10.APIConstants (pattern MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM
  :: FunPtr (Ptr PhysicalDevice_T -> Word32 -> Ptr QueueFamilyDataGraphPropertiesARM -> Ptr BaseOutStructure -> IO Result) -> Ptr PhysicalDevice_T -> Word32 -> Ptr QueueFamilyDataGraphPropertiesARM -> Ptr BaseOutStructure -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM"
getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM :: forall io
                                                                   . ( MonadIO io )
                                                                  => -- No documentation found for Nested "vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM" "physicalDevice"
                                                                     PhysicalDevice
                                                                  -> -- No documentation found for Nested "vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM" "queueFamilyIndex"
                                                                     ("queueFamilyIndex" ::: Word32)
                                                                  -> -- No documentation found for Nested "vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM" "pQueueFamilyDataGraphProperties"
                                                                     QueueFamilyDataGraphPropertiesARM
                                                                  -> -- No documentation found for Nested "vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM" "pProperties"
                                                                     ("properties" ::: Ptr BaseOutStructure)
                                                                  -> io ()
getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM physicalDevice
                                                                    queueFamilyIndex
                                                                    queueFamilyDataGraphProperties
                                                                    properties = liftIO . evalContT $ do
  let vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARMPtr = pVkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM is null" Nothing Nothing
  let vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM' = mkVkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARMPtr
  pQueueFamilyDataGraphProperties <- ContT $ withCStruct (queueFamilyDataGraphProperties)
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM" (vkGetPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM'
                                                                                                        (physicalDeviceHandle (physicalDevice))
                                                                                                        (queueFamilyIndex)
                                                                                                        pQueueFamilyDataGraphProperties
                                                                                                        (properties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkDataGraphTOSANameQualityARM - Structure describing the name and
-- quality level of a TOSA profile or extension
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_instruction_set_tosa VK_ARM_data_graph_instruction_set_tosa>,
-- 'DataGraphTOSAQualityFlagsARM', 'QueueFamilyDataGraphTOSAPropertiesARM'
data DataGraphTOSANameQualityARM = DataGraphTOSANameQualityARM
  { -- | @name@ is a pointer to a null-terminated UTF-8 string specifying the
    -- name of the TOSA profile or extension.
    name :: ByteString
  , -- No documentation found for Nested "VkDataGraphTOSANameQualityARM" "qualityFlags"
    qualityFlags :: DataGraphTOSAQualityFlagsARM
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DataGraphTOSANameQualityARM)
#endif
deriving instance Show DataGraphTOSANameQualityARM

instance ToCStruct DataGraphTOSANameQualityARM where
  withCStruct x f = allocaBytes 132 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DataGraphTOSANameQualityARM{..} f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM CChar))) (name)
    poke ((p `plusPtr` 128 :: Ptr DataGraphTOSAQualityFlagsARM)) (qualityFlags)
    f
  cStructSize = 132
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM CChar))) (mempty)
    poke ((p `plusPtr` 128 :: Ptr DataGraphTOSAQualityFlagsARM)) (zero)
    f

instance FromCStruct DataGraphTOSANameQualityARM where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM CChar))))
    qualityFlags <- peek @DataGraphTOSAQualityFlagsARM ((p `plusPtr` 128 :: Ptr DataGraphTOSAQualityFlagsARM))
    pure $ DataGraphTOSANameQualityARM
             name qualityFlags

instance Storable DataGraphTOSANameQualityARM where
  sizeOf ~_ = 132
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DataGraphTOSANameQualityARM where
  zero = DataGraphTOSANameQualityARM
           mempty
           zero


-- | VkQueueFamilyDataGraphTOSAPropertiesARM - Structure describing the TOSA
-- properties of a processing engine and operation set for a specific queue
-- family of a physical device
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_instruction_set_tosa VK_ARM_data_graph_instruction_set_tosa>,
-- 'DataGraphTOSALevelARM', 'DataGraphTOSANameQualityARM',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data QueueFamilyDataGraphTOSAPropertiesARM = QueueFamilyDataGraphTOSAPropertiesARM
  { -- | @pProfiles@ is a pointer to an array of 'DataGraphTOSANameQualityARM'
    -- structures describing the TOSA profiles supported.
    profiles :: Vector DataGraphTOSANameQualityARM
  , -- | @pExtensions@ is a pointer to an array of 'DataGraphTOSANameQualityARM'
    -- structures describing the TOSA extensions supported.
    extensions :: Vector DataGraphTOSANameQualityARM
  , -- | @level@ is a 'DataGraphTOSALevelARM' describing the TOSA level
    -- supported.
    level :: DataGraphTOSALevelARM
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (QueueFamilyDataGraphTOSAPropertiesARM)
#endif
deriving instance Show QueueFamilyDataGraphTOSAPropertiesARM

instance ToCStruct QueueFamilyDataGraphTOSAPropertiesARM where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p QueueFamilyDataGraphTOSAPropertiesARM{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_TOSA_PROPERTIES_ARM)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (profiles)) :: Word32))
    pPProfiles' <- ContT $ allocaBytes @DataGraphTOSANameQualityARM ((Data.Vector.length (profiles)) * 132)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPProfiles' `plusPtr` (132 * (i)) :: Ptr DataGraphTOSANameQualityARM) (e)) (profiles)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DataGraphTOSANameQualityARM))) (pPProfiles')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (extensions)) :: Word32))
    pPExtensions' <- ContT $ allocaBytes @DataGraphTOSANameQualityARM ((Data.Vector.length (extensions)) * 132)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPExtensions' `plusPtr` (132 * (i)) :: Ptr DataGraphTOSANameQualityARM) (e)) (extensions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr DataGraphTOSANameQualityARM))) (pPExtensions')
    lift $ poke ((p `plusPtr` 48 :: Ptr DataGraphTOSALevelARM)) (level)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_TOSA_PROPERTIES_ARM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 48 :: Ptr DataGraphTOSALevelARM)) (zero)
    f

instance FromCStruct QueueFamilyDataGraphTOSAPropertiesARM where
  peekCStruct p = do
    profileCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pProfiles <- peek @(Ptr DataGraphTOSANameQualityARM) ((p `plusPtr` 24 :: Ptr (Ptr DataGraphTOSANameQualityARM)))
    pProfiles' <- generateM (fromIntegral profileCount) (\i -> peekCStruct @DataGraphTOSANameQualityARM ((pProfiles `advancePtrBytes` (132 * (i)) :: Ptr DataGraphTOSANameQualityARM)))
    extensionCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pExtensions <- peek @(Ptr DataGraphTOSANameQualityARM) ((p `plusPtr` 40 :: Ptr (Ptr DataGraphTOSANameQualityARM)))
    pExtensions' <- generateM (fromIntegral extensionCount) (\i -> peekCStruct @DataGraphTOSANameQualityARM ((pExtensions `advancePtrBytes` (132 * (i)) :: Ptr DataGraphTOSANameQualityARM)))
    level <- peek @DataGraphTOSALevelARM ((p `plusPtr` 48 :: Ptr DataGraphTOSALevelARM))
    pure $ QueueFamilyDataGraphTOSAPropertiesARM
             pProfiles' pExtensions' level

instance Zero QueueFamilyDataGraphTOSAPropertiesARM where
  zero = QueueFamilyDataGraphTOSAPropertiesARM
           mempty
           mempty
           zero


type DataGraphTOSAQualityFlagsARM = DataGraphTOSAQualityFlagBitsARM

-- | VkDataGraphTOSAQualityFlagBitsARM - Bits specifying quality properties
-- for a TOSA profile or extension
--
-- = Description
--
-- -   'DATA_GRAPH_TOSA_QUALITY_ACCELERATED_ARM' specifies that the
--     implementation of the TOSA profile or extension is accelerated.
--
-- -   'DATA_GRAPH_TOSA_QUALITY_CONFORMANT_ARM' specifies that the
--     implementation of the TOSA profile or extension is conformant.
--
-- -   'DATA_GRAPH_TOSA_QUALITY_EXPERIMENTAL_ARM' specifies that the TOSA
--     profile or extension is experimental.
--
-- -   'DATA_GRAPH_TOSA_QUALITY_DEPRECATED_ARM' specifies that the TOSA
--     profile or extension is deprecated.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_instruction_set_tosa VK_ARM_data_graph_instruction_set_tosa>,
-- 'DataGraphTOSAQualityFlagsARM'
newtype DataGraphTOSAQualityFlagBitsARM = DataGraphTOSAQualityFlagBitsARM Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDataGraphTOSAQualityFlagBitsARM" "VK_DATA_GRAPH_TOSA_QUALITY_ACCELERATED_ARM"
pattern DATA_GRAPH_TOSA_QUALITY_ACCELERATED_ARM = DataGraphTOSAQualityFlagBitsARM 0x00000001

-- No documentation found for Nested "VkDataGraphTOSAQualityFlagBitsARM" "VK_DATA_GRAPH_TOSA_QUALITY_CONFORMANT_ARM"
pattern DATA_GRAPH_TOSA_QUALITY_CONFORMANT_ARM = DataGraphTOSAQualityFlagBitsARM 0x00000002

-- No documentation found for Nested "VkDataGraphTOSAQualityFlagBitsARM" "VK_DATA_GRAPH_TOSA_QUALITY_EXPERIMENTAL_ARM"
pattern DATA_GRAPH_TOSA_QUALITY_EXPERIMENTAL_ARM = DataGraphTOSAQualityFlagBitsARM 0x00000004

-- No documentation found for Nested "VkDataGraphTOSAQualityFlagBitsARM" "VK_DATA_GRAPH_TOSA_QUALITY_DEPRECATED_ARM"
pattern DATA_GRAPH_TOSA_QUALITY_DEPRECATED_ARM = DataGraphTOSAQualityFlagBitsARM 0x00000008

conNameDataGraphTOSAQualityFlagBitsARM :: String
conNameDataGraphTOSAQualityFlagBitsARM = "DataGraphTOSAQualityFlagBitsARM"

enumPrefixDataGraphTOSAQualityFlagBitsARM :: String
enumPrefixDataGraphTOSAQualityFlagBitsARM = "DATA_GRAPH_TOSA_QUALITY_"

showTableDataGraphTOSAQualityFlagBitsARM :: [(DataGraphTOSAQualityFlagBitsARM, String)]
showTableDataGraphTOSAQualityFlagBitsARM =
  [
    ( DATA_GRAPH_TOSA_QUALITY_ACCELERATED_ARM
    , "ACCELERATED_ARM"
    )
  ,
    ( DATA_GRAPH_TOSA_QUALITY_CONFORMANT_ARM
    , "CONFORMANT_ARM"
    )
  ,
    ( DATA_GRAPH_TOSA_QUALITY_EXPERIMENTAL_ARM
    , "EXPERIMENTAL_ARM"
    )
  ,
    ( DATA_GRAPH_TOSA_QUALITY_DEPRECATED_ARM
    , "DEPRECATED_ARM"
    )
  ]

instance Show DataGraphTOSAQualityFlagBitsARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphTOSAQualityFlagBitsARM
      showTableDataGraphTOSAQualityFlagBitsARM
      conNameDataGraphTOSAQualityFlagBitsARM
      (\(DataGraphTOSAQualityFlagBitsARM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DataGraphTOSAQualityFlagBitsARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphTOSAQualityFlagBitsARM
      showTableDataGraphTOSAQualityFlagBitsARM
      conNameDataGraphTOSAQualityFlagBitsARM
      DataGraphTOSAQualityFlagBitsARM

-- | VkDataGraphTOSALevelARM - Enumeration describing a data graph TOSA level
--
-- = Description
--
-- -   'DATA_GRAPH_TOSA_LEVEL_NONE_ARM' corresponds to the none TOSA level,
--     as described in the TOSA specification.
--
-- -   'DATA_GRAPH_TOSA_LEVEL_8K_ARM' corresponds to the 8K TOSA level, as
--     described in the TOSA specification.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph_instruction_set_tosa VK_ARM_data_graph_instruction_set_tosa>,
-- 'QueueFamilyDataGraphTOSAPropertiesARM'
newtype DataGraphTOSALevelARM = DataGraphTOSALevelARM Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDataGraphTOSALevelARM" "VK_DATA_GRAPH_TOSA_LEVEL_NONE_ARM"
pattern DATA_GRAPH_TOSA_LEVEL_NONE_ARM = DataGraphTOSALevelARM 0

-- No documentation found for Nested "VkDataGraphTOSALevelARM" "VK_DATA_GRAPH_TOSA_LEVEL_8K_ARM"
pattern DATA_GRAPH_TOSA_LEVEL_8K_ARM = DataGraphTOSALevelARM 1

{-# COMPLETE
  DATA_GRAPH_TOSA_LEVEL_NONE_ARM
  , DATA_GRAPH_TOSA_LEVEL_8K_ARM ::
    DataGraphTOSALevelARM
  #-}

conNameDataGraphTOSALevelARM :: String
conNameDataGraphTOSALevelARM = "DataGraphTOSALevelARM"

enumPrefixDataGraphTOSALevelARM :: String
enumPrefixDataGraphTOSALevelARM = "DATA_GRAPH_TOSA_LEVEL_"

showTableDataGraphTOSALevelARM :: [(DataGraphTOSALevelARM, String)]
showTableDataGraphTOSALevelARM =
  [ (DATA_GRAPH_TOSA_LEVEL_NONE_ARM, "NONE_ARM")
  , (DATA_GRAPH_TOSA_LEVEL_8K_ARM, "8K_ARM")
  ]

instance Show DataGraphTOSALevelARM where
  showsPrec =
    enumShowsPrec
      enumPrefixDataGraphTOSALevelARM
      showTableDataGraphTOSALevelARM
      conNameDataGraphTOSALevelARM
      (\(DataGraphTOSALevelARM x) -> x)
      (showsPrec 11)

instance Read DataGraphTOSALevelARM where
  readPrec =
    enumReadPrec
      enumPrefixDataGraphTOSALevelARM
      showTableDataGraphTOSALevelARM
      conNameDataGraphTOSALevelARM
      DataGraphTOSALevelARM

type ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_SPEC_VERSION"
pattern ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_SPEC_VERSION :: forall a . Integral a => a
pattern ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_SPEC_VERSION = 1


type ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_EXTENSION_NAME = "VK_ARM_data_graph_instruction_set_tosa"

-- No documentation found for TopLevel "VK_ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_EXTENSION_NAME"
pattern ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_EXTENSION_NAME = "VK_ARM_data_graph_instruction_set_tosa"

