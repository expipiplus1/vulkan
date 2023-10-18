{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_module_identifier - device extension
--
-- == VK_EXT_shader_module_identifier
--
-- [__Name String__]
--     @VK_EXT_shader_module_identifier@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     463
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_cache_control VK_EXT_pipeline_creation_cache_control>
--
-- [__Contact__]
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_module_identifier] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_EXT_shader_module_identifier extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_module_identifier.adoc VK_EXT_shader_module_identifier>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Ricardo Garcia, Igalia
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Tom Olson, Arm
--
--     -   Faith Ekstrand, Collabora
--
-- == Description
--
-- Some applications generate SPIR-V code at runtime. When pipeline caches
-- are primed, either explicitly through e.g.
-- 'Vulkan.Core10.Handles.PipelineCache' mechanisms, or implicitly through
-- driver managed caches, having to re-generate SPIR-V modules is
-- redundant. SPIR-V modules could be cached on disk by an application, but
-- the extra disk size requirement might be prohibitive in some use cases.
--
-- This extension adds the ability for an application to query a small
-- identifier associated with a 'Vulkan.Core10.Handles.ShaderModule'. On
-- subsequent runs of the application, the same identifier /can/ be
-- provided in lieu of a 'Vulkan.Core10.Handles.ShaderModule' object. A
-- pipeline creation call with such a module /may/ succeed if a pipeline
-- could be created without invoking compilation, and information inside
-- the SPIR-V module is not required by the implementation.
--
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
-- /must/ be used if only the identifier is provided, and this use case is
-- intended to work like a non-blocking, speculative compile. Applications
-- /can/ fallback as necessary.
--
-- The main motivation for identifying the module itself and not the entire
-- pipeline is that pipeline identifiers change when a driver is updated,
-- but module identifiers are expected to be stable for any particular
-- driver implementation. This approach is helpful for shader
-- pre-compilation systems which can prime pipeline caches ahead of time.
-- When on-disk pipeline caches are updated, the same shader identifiers
-- could lead to a pipeline cache hit.
--
-- == New Commands
--
-- -   'getShaderModuleCreateInfoIdentifierEXT'
--
-- -   'getShaderModuleIdentifierEXT'
--
-- == New Structures
--
-- -   'ShaderModuleIdentifierEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderModuleIdentifierFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderModuleIdentifierPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo':
--
--     -   'PipelineShaderStageModuleIdentifierCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_MODULE_IDENTIFIER_EXTENSION_NAME'
--
-- -   'EXT_SHADER_MODULE_IDENTIFIER_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_MODULE_IDENTIFIER_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-03-16 (Hans-Kristian Arntzen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT',
-- 'PhysicalDeviceShaderModuleIdentifierFeaturesEXT',
-- 'PhysicalDeviceShaderModuleIdentifierPropertiesEXT',
-- 'PipelineShaderStageModuleIdentifierCreateInfoEXT',
-- 'ShaderModuleIdentifierEXT', 'getShaderModuleCreateInfoIdentifierEXT',
-- 'getShaderModuleIdentifierEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_module_identifier Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_module_identifier  ( getShaderModuleIdentifierEXT
                                                          , getShaderModuleCreateInfoIdentifierEXT
                                                          , PhysicalDeviceShaderModuleIdentifierFeaturesEXT(..)
                                                          , PhysicalDeviceShaderModuleIdentifierPropertiesEXT(..)
                                                          , PipelineShaderStageModuleIdentifierCreateInfoEXT(..)
                                                          , ShaderModuleIdentifierEXT(..)
                                                          , EXT_SHADER_MODULE_IDENTIFIER_SPEC_VERSION
                                                          , pattern EXT_SHADER_MODULE_IDENTIFIER_SPEC_VERSION
                                                          , EXT_SHADER_MODULE_IDENTIFIER_EXTENSION_NAME
                                                          , pattern EXT_SHADER_MODULE_IDENTIFIER_EXTENSION_NAME
                                                          , MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT
                                                          , pattern MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT
                                                          ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Word (Word32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetShaderModuleCreateInfoIdentifierEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetShaderModuleIdentifierEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.Core10.APIConstants (MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.Core10.Handles (ShaderModule)
import Vulkan.Core10.Handles (ShaderModule(..))
import Vulkan.Core10.Shader (ShaderModuleCreateInfo)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SHADER_MODULE_IDENTIFIER_EXT))
import Vulkan.Core10.APIConstants (MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT)
import Vulkan.Core10.APIConstants (pattern MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetShaderModuleIdentifierEXT
  :: FunPtr (Ptr Device_T -> ShaderModule -> Ptr ShaderModuleIdentifierEXT -> IO ()) -> Ptr Device_T -> ShaderModule -> Ptr ShaderModuleIdentifierEXT -> IO ()

-- | vkGetShaderModuleIdentifierEXT - Query a unique identifier for a shader
-- module
--
-- = Description
--
-- The identifier returned by the implementation /must/ only depend on
-- @shaderIdentifierAlgorithmUUID@ and information provided in the
-- 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' which created
-- @shaderModule@. The implementation /may/ return equal identifiers for
-- two different 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structures
-- if the difference does not affect pipeline compilation. Identifiers are
-- only meaningful on different 'Vulkan.Core10.Handles.Device' objects if
-- the device the identifier was queried from had the same
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-shaderModuleIdentifierAlgorithmUUID shaderModuleIdentifierAlgorithmUUID>
-- as the device consuming the identifier.
--
-- == Valid Usage
--
-- -   #VUID-vkGetShaderModuleIdentifierEXT-shaderModuleIdentifier-06884#
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderModuleIdentifier shaderModuleIdentifier>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetShaderModuleIdentifierEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetShaderModuleIdentifierEXT-shaderModule-parameter#
--     @shaderModule@ /must/ be a valid
--     'Vulkan.Core10.Handles.ShaderModule' handle
--
-- -   #VUID-vkGetShaderModuleIdentifierEXT-pIdentifier-parameter#
--     @pIdentifier@ /must/ be a valid pointer to a
--     'ShaderModuleIdentifierEXT' structure
--
-- -   #VUID-vkGetShaderModuleIdentifierEXT-shaderModule-parent#
--     @shaderModule@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_module_identifier VK_EXT_shader_module_identifier>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.ShaderModule',
-- 'ShaderModuleIdentifierEXT'
getShaderModuleIdentifierEXT :: forall io
                              . (MonadIO io)
                             => -- | @device@ is the logical device that created the shader module.
                                Device
                             -> -- | @shaderModule@ is the handle of the shader module.
                                ShaderModule
                             -> io (ShaderModuleIdentifierEXT)
getShaderModuleIdentifierEXT device shaderModule = liftIO . evalContT $ do
  let vkGetShaderModuleIdentifierEXTPtr = pVkGetShaderModuleIdentifierEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetShaderModuleIdentifierEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetShaderModuleIdentifierEXT is null" Nothing Nothing
  let vkGetShaderModuleIdentifierEXT' = mkVkGetShaderModuleIdentifierEXT vkGetShaderModuleIdentifierEXTPtr
  pPIdentifier <- ContT (withZeroCStruct @ShaderModuleIdentifierEXT)
  lift $ traceAroundEvent "vkGetShaderModuleIdentifierEXT" (vkGetShaderModuleIdentifierEXT'
                                                              (deviceHandle (device))
                                                              (shaderModule)
                                                              (pPIdentifier))
  pIdentifier <- lift $ peekCStruct @ShaderModuleIdentifierEXT pPIdentifier
  pure $ (pIdentifier)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetShaderModuleCreateInfoIdentifierEXT
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct ShaderModuleCreateInfo) -> Ptr ShaderModuleIdentifierEXT -> IO ()) -> Ptr Device_T -> Ptr (SomeStruct ShaderModuleCreateInfo) -> Ptr ShaderModuleIdentifierEXT -> IO ()

-- | vkGetShaderModuleCreateInfoIdentifierEXT - Query a unique identifier for
-- a shader module create info
--
-- = Description
--
-- The identifier returned by implementation /must/ only depend on
-- @shaderIdentifierAlgorithmUUID@ and information provided in the
-- 'Vulkan.Core10.Shader.ShaderModuleCreateInfo'. The implementation /may/
-- return equal identifiers for two different
-- 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structures if the
-- difference does not affect pipeline compilation. Identifiers are only
-- meaningful on different 'Vulkan.Core10.Handles.Device' objects if the
-- device the identifier was queried from had the same
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-shaderModuleIdentifierAlgorithmUUID shaderModuleIdentifierAlgorithmUUID>
-- as the device consuming the identifier.
--
-- The identifier returned by the implementation in
-- 'getShaderModuleCreateInfoIdentifierEXT' /must/ be equal to the
-- identifier returned by 'getShaderModuleIdentifierEXT' given equivalent
-- definitions of 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' and any
-- chained @pNext@ structures.
--
-- == Valid Usage
--
-- -   #VUID-vkGetShaderModuleCreateInfoIdentifierEXT-shaderModuleIdentifier-06885#
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderModuleIdentifier shaderModuleIdentifier>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetShaderModuleCreateInfoIdentifierEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetShaderModuleCreateInfoIdentifierEXT-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structure
--
-- -   #VUID-vkGetShaderModuleCreateInfoIdentifierEXT-pIdentifier-parameter#
--     @pIdentifier@ /must/ be a valid pointer to a
--     'ShaderModuleIdentifierEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_module_identifier VK_EXT_shader_module_identifier>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Shader.ShaderModuleCreateInfo',
-- 'ShaderModuleIdentifierEXT'
getShaderModuleCreateInfoIdentifierEXT :: forall a io
                                        . ( Extendss ShaderModuleCreateInfo a
                                          , PokeChain a
                                          , MonadIO io )
                                       => -- | @device@ is the logical device that /can/ create a
                                          -- 'Vulkan.Core10.Handles.ShaderModule' from @pCreateInfo@.
                                          Device
                                       -> -- | @pCreateInfo@ is a pointer to a
                                          -- 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structure.
                                          (ShaderModuleCreateInfo a)
                                       -> io (ShaderModuleIdentifierEXT)
getShaderModuleCreateInfoIdentifierEXT device
                                         createInfo = liftIO . evalContT $ do
  let vkGetShaderModuleCreateInfoIdentifierEXTPtr = pVkGetShaderModuleCreateInfoIdentifierEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetShaderModuleCreateInfoIdentifierEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetShaderModuleCreateInfoIdentifierEXT is null" Nothing Nothing
  let vkGetShaderModuleCreateInfoIdentifierEXT' = mkVkGetShaderModuleCreateInfoIdentifierEXT vkGetShaderModuleCreateInfoIdentifierEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pPIdentifier <- ContT (withZeroCStruct @ShaderModuleIdentifierEXT)
  lift $ traceAroundEvent "vkGetShaderModuleCreateInfoIdentifierEXT" (vkGetShaderModuleCreateInfoIdentifierEXT'
                                                                        (deviceHandle (device))
                                                                        (forgetExtensions pCreateInfo)
                                                                        (pPIdentifier))
  pIdentifier <- lift $ peekCStruct @ShaderModuleIdentifierEXT pPIdentifier
  pure $ (pIdentifier)


-- | VkPhysicalDeviceShaderModuleIdentifierFeaturesEXT - Structure describing
-- whether querying and providing an identifier of a shader module is
-- supported by the implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceShaderModuleIdentifierFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceShaderModuleIdentifierFeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_module_identifier VK_EXT_shader_module_identifier>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderModuleIdentifierFeaturesEXT = PhysicalDeviceShaderModuleIdentifierFeaturesEXT
  { -- | #features-shaderModuleIdentifier# @shaderModuleIdentifier@ indicates
    -- whether the implementation supports querying an identifier of a
    -- 'Vulkan.Core10.Handles.ShaderModule' or
    -- 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structure, and creating
    -- pipelines from identifiers only.
    shaderModuleIdentifier :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderModuleIdentifierFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceShaderModuleIdentifierFeaturesEXT

instance ToCStruct PhysicalDeviceShaderModuleIdentifierFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderModuleIdentifierFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (shaderModuleIdentifier))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderModuleIdentifierFeaturesEXT where
  peekCStruct p = do
    shaderModuleIdentifier <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderModuleIdentifierFeaturesEXT
             (bool32ToBool shaderModuleIdentifier)

instance Storable PhysicalDeviceShaderModuleIdentifierFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderModuleIdentifierFeaturesEXT where
  zero = PhysicalDeviceShaderModuleIdentifierFeaturesEXT
           zero


-- | VkPhysicalDeviceShaderModuleIdentifierPropertiesEXT - Structure
-- describing shader module identifier properties of an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceShaderModuleIdentifierPropertiesEXT'
-- structure describe the following:
--
-- = Description
--
-- Note
--
-- The algorithm UUID may be the same in different ICDs if the algorithms
-- are guaranteed to produce the same results. This may happen in driver
-- stacks which support different kinds of hardware with shared code.
--
-- Khronos\' conformance testing can not guarantee that
-- @shaderModuleIdentifierAlgorithmUUID@ values are actually unique, so
-- implementors should make their own best efforts to ensure that their
-- UUID is unlikely to conflict with other implementations which may use a
-- different algorithm. In particular, hard-coded values which easily
-- conflict, such as all-@0@ bits, /should/ never be used. Hard-coded
-- values are acceptable if best effort is ensured that the value will not
-- accidentally conflict.
--
-- If the 'PhysicalDeviceShaderModuleIdentifierPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_module_identifier VK_EXT_shader_module_identifier>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceShaderModuleIdentifierPropertiesEXT = PhysicalDeviceShaderModuleIdentifierPropertiesEXT
  { -- | #limits-shaderModuleIdentifierAlgorithmUUID#
    -- @shaderModuleIdentifierAlgorithmUUID@ is an array of
    -- 'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values which uniquely
    -- represents the algorithm used to compute an identifier in
    -- 'getShaderModuleIdentifierEXT' and
    -- 'getShaderModuleCreateInfoIdentifierEXT'. Implementations /should/ not
    -- change this value in different driver versions if the algorithm used to
    -- compute an identifier is the same.
    shaderModuleIdentifierAlgorithmUUID :: ByteString }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderModuleIdentifierPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceShaderModuleIdentifierPropertiesEXT

instance ToCStruct PhysicalDeviceShaderModuleIdentifierPropertiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderModuleIdentifierPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (shaderModuleIdentifierAlgorithmUUID)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    f

instance FromCStruct PhysicalDeviceShaderModuleIdentifierPropertiesEXT where
  peekCStruct p = do
    shaderModuleIdentifierAlgorithmUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    pure $ PhysicalDeviceShaderModuleIdentifierPropertiesEXT
             shaderModuleIdentifierAlgorithmUUID

instance Storable PhysicalDeviceShaderModuleIdentifierPropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderModuleIdentifierPropertiesEXT where
  zero = PhysicalDeviceShaderModuleIdentifierPropertiesEXT
           mempty


-- | VkPipelineShaderStageModuleIdentifierCreateInfoEXT - Structure
-- specifying an identifier for a shader module
--
-- = Description
--
-- Any identifier /can/ be used. If the pipeline being created with
-- identifier requires compilation to complete the pipeline creation call,
-- pipeline compilation /must/ fail as defined by
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'.
--
-- @pIdentifier@ and @identifierSize@ /can/ be obtained from an
-- 'ShaderModuleIdentifierEXT' queried earlier.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineShaderStageModuleIdentifierCreateInfoEXT-pNext-06850#
--     If this structure is included in a @pNext@ chain and
--     @identifierSize@ is not equal to 0, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-shaderModuleIdentifier shaderModuleIdentifier>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineShaderStageModuleIdentifierCreateInfoEXT-pNext-06851#
--     If this struct is included in a @pNext@ chain of
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' and
--     @identifierSize@ is not equal to 0, the pipeline /must/ be created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
--     flag set
--
-- -   #VUID-VkPipelineShaderStageModuleIdentifierCreateInfoEXT-identifierSize-06852#
--     @identifierSize@ /must/ be less-or-equal to
--     'Vulkan.Core10.APIConstants.MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineShaderStageModuleIdentifierCreateInfoEXT-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT'
--
-- -   #VUID-VkPipelineShaderStageModuleIdentifierCreateInfoEXT-pIdentifier-parameter#
--     If @identifierSize@ is not @0@, @pIdentifier@ /must/ be a valid
--     pointer to an array of @identifierSize@ @uint8_t@ values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_module_identifier VK_EXT_shader_module_identifier>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineShaderStageModuleIdentifierCreateInfoEXT = PipelineShaderStageModuleIdentifierCreateInfoEXT
  { -- | @pIdentifier@ is a pointer to a buffer of opaque data specifying an
    -- identifier.
    identifier :: Vector Word8 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineShaderStageModuleIdentifierCreateInfoEXT)
#endif
deriving instance Show PipelineShaderStageModuleIdentifierCreateInfoEXT

instance ToCStruct PipelineShaderStageModuleIdentifierCreateInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineShaderStageModuleIdentifierCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (identifier)) :: Word32))
    pPIdentifier' <- ContT $ allocaBytes @Word8 (Data.Vector.length (identifier))
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIdentifier' `plusPtr` (1 * (i)) :: Ptr Word8) (e)) (identifier)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word8))) (pPIdentifier')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineShaderStageModuleIdentifierCreateInfoEXT where
  peekCStruct p = do
    identifierSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pIdentifier <- peek @(Ptr Word8) ((p `plusPtr` 24 :: Ptr (Ptr Word8)))
    pIdentifier' <- generateM (fromIntegral identifierSize) (\i -> peek @Word8 ((pIdentifier `advancePtrBytes` (1 * (i)) :: Ptr Word8)))
    pure $ PipelineShaderStageModuleIdentifierCreateInfoEXT
             pIdentifier'

instance Zero PipelineShaderStageModuleIdentifierCreateInfoEXT where
  zero = PipelineShaderStageModuleIdentifierCreateInfoEXT
           mempty


-- | VkShaderModuleIdentifierEXT - A unique identifier for a shader module
--
-- = Description
--
-- Any returned values beyond the first @identifierSize@ bytes are
-- undefined. Implementations /must/ return an @identifierSize@ greater
-- than 0, and less-or-equal to
-- 'Vulkan.Core10.APIConstants.MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT'.
--
-- Two identifiers are considered equal if @identifierSize@ is equal and
-- the first @identifierSize@ bytes of @identifier@ compare equal.
--
-- Implementations /may/ return a different @identifierSize@ for different
-- modules. Implementations /should/ ensure that @identifierSize@ is large
-- enough to uniquely define a shader module.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_module_identifier VK_EXT_shader_module_identifier>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getShaderModuleCreateInfoIdentifierEXT', 'getShaderModuleIdentifierEXT'
data ShaderModuleIdentifierEXT = ShaderModuleIdentifierEXT
  { -- | @identifierSize@ is the size, in bytes, of valid data returned in
    -- @identifier@.
    identifierSize :: Word32
  , -- | @identifier@ is a buffer of opaque data specifying an identifier.
    identifier :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ShaderModuleIdentifierEXT)
#endif
deriving instance Show ShaderModuleIdentifierEXT

instance ToCStruct ShaderModuleIdentifierEXT where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ShaderModuleIdentifierEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_MODULE_IDENTIFIER_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (identifierSize)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT Word8))) (identifier)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SHADER_MODULE_IDENTIFIER_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT Word8))) (mempty)
    f

instance FromCStruct ShaderModuleIdentifierEXT where
  peekCStruct p = do
    identifierSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    identifier <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT Word8)))
    pure $ ShaderModuleIdentifierEXT
             identifierSize identifier

instance Storable ShaderModuleIdentifierEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ShaderModuleIdentifierEXT where
  zero = ShaderModuleIdentifierEXT
           zero
           mempty


type EXT_SHADER_MODULE_IDENTIFIER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_MODULE_IDENTIFIER_SPEC_VERSION"
pattern EXT_SHADER_MODULE_IDENTIFIER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_MODULE_IDENTIFIER_SPEC_VERSION = 1


type EXT_SHADER_MODULE_IDENTIFIER_EXTENSION_NAME = "VK_EXT_shader_module_identifier"

-- No documentation found for TopLevel "VK_EXT_SHADER_MODULE_IDENTIFIER_EXTENSION_NAME"
pattern EXT_SHADER_MODULE_IDENTIFIER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_MODULE_IDENTIFIER_EXTENSION_NAME = "VK_EXT_shader_module_identifier"

