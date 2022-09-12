{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_properties - device extension
--
-- == VK_EXT_pipeline_properties
--
-- [__Name String__]
--     @VK_EXT_pipeline_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     373
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
-- [__Contact__]
--
--     -   Mukund Keshava
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_pipeline_properties] @mkeshavanv%0A*Here describe the issue or question you have about the VK_EXT_pipeline_properties extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-04-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mukund Keshava, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Mark Bellamy, Arm
--
-- == Description
--
-- Vulkan SC requires offline compilation of pipelines. In order to support
-- this, the pipeline state is represented in a
-- <https://github.com/KhronosGroup/VulkanSC-Docs/wiki/JSON-schema JSON schema>
-- that is read by an offline tool for compilation.
--
-- One method of developing a Vulkan SC application is to author a Vulkan
-- application and use a layer to record and serialize the pipeline state
-- and shaders for offline compilation. Each pipeline is represented by a
-- separate JSON file, and can be identified with a @pipelineIdentifier@.
--
-- Once the pipelines have been compiled by the offline pipeline cache
-- compiler, the Vulkan SC application can then use this
-- @pipelineIdentifier@ for identifying the pipeline via Vulkan SC’s
-- @VkPipelineIdentifierInfo@ structure.
--
-- This extension allows the Vulkan application to query the
-- @pipelineIdentifier@ associated with each pipeline so that the
-- application can store this with its pipeline metadata and the Vulkan SC
-- application will then use to map the same state to an entry in the
-- Vulkan SC pipeline cache.
--
-- It is expected that this extension will initially be implemented in the
-- json generation layer, although we can envision that there might be
-- future uses for it in native Vulkan drivers as well.
--
-- == New Commands
--
-- -   'getPipelinePropertiesEXT'
--
-- == New Structures
--
-- -   'PipelineInfoEXT'
--
-- -   'PipelinePropertiesIdentifierEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelinePropertiesFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_PROPERTIES_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROPERTIES_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PIPELINE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_PROPERTIES_IDENTIFIER_EXT'
--
-- == Issues
--
-- (1) This extension does not make sense on a strict Vulkan SC
-- implementation. It may however be of potential use in a non-strict
-- Vulkan SC implementation. Should this extension be enabled as part of
-- Vulkan SC as well?
--
-- __RESOLVED__: No. This extension will not be enabled for Vulkan SC.
--
-- (2) This is intended to be a general pipeline properties query, but is
-- currently only retrieving the pipeline identifier. Should the pipeline
-- identifier query be mandatory for this extension and for all queries
-- using this entry point?
--
-- __RESOLVED__: Use 'Vulkan.CStruct.Extends.BaseOutStructure' for the
-- return parameter. Currently this is required to actually be a
-- 'PipelinePropertiesIdentifierEXT' structure, but that could be relaxed
-- in the future to allow other structure types or to allow other
-- structures to be chained in along with this one.
--
-- (3) Should there be a feature structure? Should it be required?
--
-- __RESOLVED__: Add a feature structure, and a feature for querying
-- pipeline identifier, but allow it to be optional so that this extension
-- can be used as the basis for other pipeline property queries without
-- requiring the pipeline identifier to be supported.
--
-- == Version History
--
-- -   Revision 1, 2022-04-19 (Mukund Keshava, Daniel Koch)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDevicePipelinePropertiesFeaturesEXT', 'PipelineInfoEXT',
-- 'PipelinePropertiesIdentifierEXT', 'getPipelinePropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_properties  ( getPipelinePropertiesEXT
                                                     , pattern STRUCTURE_TYPE_PIPELINE_INFO_EXT
                                                     , PipelinePropertiesIdentifierEXT(..)
                                                     , PhysicalDevicePipelinePropertiesFeaturesEXT(..)
                                                     , PipelineInfoEXT
                                                     , EXT_PIPELINE_PROPERTIES_SPEC_VERSION
                                                     , pattern EXT_PIPELINE_PROPERTIES_SPEC_VERSION
                                                     , EXT_PIPELINE_PROPERTIES_EXTENSION_NAME
                                                     , pattern EXT_PIPELINE_PROPERTIES_EXTENSION_NAME
                                                     , PipelineInfoKHR(..)
                                                     ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (BaseOutStructure)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelinePropertiesEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineInfoKHR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROPERTIES_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_PROPERTIES_IDENTIFIER_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineInfoKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelinePropertiesEXT
  :: FunPtr (Ptr Device_T -> Ptr PipelineInfoEXT -> Ptr BaseOutStructure -> IO Result) -> Ptr Device_T -> Ptr PipelineInfoEXT -> Ptr BaseOutStructure -> IO Result

-- | vkGetPipelinePropertiesEXT - Query pipeline properties
--
-- = Description
--
-- To query a pipeline’s @pipelineIdentifier@ pass a
-- 'PipelinePropertiesIdentifierEXT' structure in @pPipelineProperties@.
-- Each pipeline is associated with a @pipelineIdentifier@ and the
-- identifier is implementation specific.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPipelinePropertiesEXT-pipeline-06738# The @pipeline@
--     member of @pPipelineInfo@ must have been created with @device@
--
-- -   #VUID-vkGetPipelinePropertiesEXT-pPipelineProperties-06739#
--     @pPipelineProperties@ /must/ be a valid pointer to a
--     'PipelinePropertiesIdentifierEXT' structure
--
-- -   #VUID-vkGetPipelinePropertiesEXT-None-06766# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-pipelinePropertiesIdentifier pipelinePropertiesIdentifier>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPipelinePropertiesEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetPipelinePropertiesEXT-pPipelineInfo-parameter#
--     @pPipelineInfo@ /must/ be a valid pointer to a valid
--     'PipelineInfoEXT' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_properties VK_EXT_pipeline_properties>,
-- 'Vulkan.CStruct.Extends.BaseOutStructure',
-- 'Vulkan.Core10.Handles.Device', 'PipelineInfoEXT'
getPipelinePropertiesEXT :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that created the pipeline.
                            Device
                         -> -- | @pPipelineInfo@ is a pointer to a 'PipelineInfoEXT' structure which
                            -- describes the pipeline being queried.
                            PipelineInfoEXT
                         -> -- | @pPipelineProperties@ is a pointer to a
                            -- 'Vulkan.CStruct.Extends.BaseOutStructure' structure in which the
                            -- pipeline properties will be written.
                            ("pipelineProperties" ::: Ptr BaseOutStructure)
                         -> io ()
getPipelinePropertiesEXT device pipelineInfo pipelineProperties = liftIO . evalContT $ do
  let vkGetPipelinePropertiesEXTPtr = pVkGetPipelinePropertiesEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPipelinePropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelinePropertiesEXT is null" Nothing Nothing
  let vkGetPipelinePropertiesEXT' = mkVkGetPipelinePropertiesEXT vkGetPipelinePropertiesEXTPtr
  pPipelineInfo <- ContT $ withCStruct (pipelineInfo)
  r <- lift $ traceAroundEvent "vkGetPipelinePropertiesEXT" (vkGetPipelinePropertiesEXT' (deviceHandle (device)) pPipelineInfo (pipelineProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_INFO_EXT = STRUCTURE_TYPE_PIPELINE_INFO_KHR


-- | VkPipelinePropertiesIdentifierEXT - Structure used to retrieve pipeline
-- properties
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_properties VK_EXT_pipeline_properties>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelinePropertiesIdentifierEXT = PipelinePropertiesIdentifierEXT
  { -- | @pipelineIdentifier@ is an array of
    -- 'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values into which the
    -- pipeline identifier will be written.
    pipelineIdentifier :: ByteString }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelinePropertiesIdentifierEXT)
#endif
deriving instance Show PipelinePropertiesIdentifierEXT

instance ToCStruct PipelinePropertiesIdentifierEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelinePropertiesIdentifierEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_PROPERTIES_IDENTIFIER_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (pipelineIdentifier)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_PROPERTIES_IDENTIFIER_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    f

instance FromCStruct PipelinePropertiesIdentifierEXT where
  peekCStruct p = do
    pipelineIdentifier <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    pure $ PipelinePropertiesIdentifierEXT
             pipelineIdentifier

instance Storable PipelinePropertiesIdentifierEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelinePropertiesIdentifierEXT where
  zero = PipelinePropertiesIdentifierEXT
           mempty


-- | VkPhysicalDevicePipelinePropertiesFeaturesEXT - Structure describing
-- what pipeline properties are supported
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePipelinePropertiesFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePipelinePropertiesFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_properties VK_EXT_pipeline_properties>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelinePropertiesFeaturesEXT = PhysicalDevicePipelinePropertiesFeaturesEXT
  { -- | #features-pipelinePropertiesIdentifier# @pipelinePropertiesIdentifier@
    -- indicates that the implementation supports querying a unique pipeline
    -- identifier.
    pipelinePropertiesIdentifier :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelinePropertiesFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePipelinePropertiesFeaturesEXT

instance ToCStruct PhysicalDevicePipelinePropertiesFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelinePropertiesFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROPERTIES_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelinePropertiesIdentifier))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_PROPERTIES_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelinePropertiesFeaturesEXT where
  peekCStruct p = do
    pipelinePropertiesIdentifier <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelinePropertiesFeaturesEXT
             (bool32ToBool pipelinePropertiesIdentifier)

instance Storable PhysicalDevicePipelinePropertiesFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelinePropertiesFeaturesEXT where
  zero = PhysicalDevicePipelinePropertiesFeaturesEXT
           zero


-- No documentation found for TopLevel "VkPipelineInfoEXT"
type PipelineInfoEXT = PipelineInfoKHR


type EXT_PIPELINE_PROPERTIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PIPELINE_PROPERTIES_SPEC_VERSION"
pattern EXT_PIPELINE_PROPERTIES_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_PROPERTIES_SPEC_VERSION = 1


type EXT_PIPELINE_PROPERTIES_EXTENSION_NAME = "VK_EXT_pipeline_properties"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_PROPERTIES_EXTENSION_NAME"
pattern EXT_PIPELINE_PROPERTIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_PROPERTIES_EXTENSION_NAME = "VK_EXT_pipeline_properties"

