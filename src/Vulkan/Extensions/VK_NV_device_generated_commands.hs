{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_device_generated_commands"
module Vulkan.Extensions.VK_NV_device_generated_commands  ( cmdExecuteGeneratedCommandsNV
                                                          , cmdPreprocessGeneratedCommandsNV
                                                          , cmdBindPipelineShaderGroupNV
                                                          , getGeneratedCommandsMemoryRequirementsNV
                                                          , createIndirectCommandsLayoutNV
                                                          , withIndirectCommandsLayoutNV
                                                          , destroyIndirectCommandsLayoutNV
                                                          , PhysicalDeviceDeviceGeneratedCommandsFeaturesNV(..)
                                                          , PhysicalDeviceDeviceGeneratedCommandsPropertiesNV(..)
                                                          , GraphicsShaderGroupCreateInfoNV(..)
                                                          , GraphicsPipelineShaderGroupsCreateInfoNV(..)
                                                          , BindShaderGroupIndirectCommandNV(..)
                                                          , BindIndexBufferIndirectCommandNV(..)
                                                          , BindVertexBufferIndirectCommandNV(..)
                                                          , SetStateFlagsIndirectCommandNV(..)
                                                          , IndirectCommandsStreamNV(..)
                                                          , IndirectCommandsLayoutTokenNV(..)
                                                          , IndirectCommandsLayoutCreateInfoNV(..)
                                                          , GeneratedCommandsInfoNV(..)
                                                          , GeneratedCommandsMemoryRequirementsInfoNV(..)
                                                          , IndirectCommandsLayoutUsageFlagsNV
                                                          , IndirectCommandsLayoutUsageFlagBitsNV( INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV
                                                                                                 , INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV
                                                                                                 , INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV
                                                                                                 , ..
                                                                                                 )
                                                          , IndirectStateFlagsNV
                                                          , IndirectStateFlagBitsNV( INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV
                                                                                   , ..
                                                                                   )
                                                          , IndirectCommandsTokenTypeNV( INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV
                                                                                       , INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV
                                                                                       , ..
                                                                                       )
                                                          , NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                          , pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION
                                                          , NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                          , pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME
                                                          , IndirectCommandsLayoutNV(..)
                                                          ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import Foreign.Ptr (FunPtr)
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindPipelineShaderGroupNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdExecuteGeneratedCommandsNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPreprocessGeneratedCommandsNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateIndirectCommandsLayoutNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyIndirectCommandsLayoutNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetGeneratedCommandsMemoryRequirementsNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV)
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV(..))
import Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.Core10.Pipeline (PipelineTessellationStateCreateInfo)
import Vulkan.Core10.Pipeline (PipelineVertexInputStateCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (IndirectCommandsLayoutNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdExecuteGeneratedCommandsNV
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> Ptr GeneratedCommandsInfoNV -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> Ptr GeneratedCommandsInfoNV -> IO ()

-- No documentation found for TopLevel "vkCmdExecuteGeneratedCommandsNV"
cmdExecuteGeneratedCommandsNV :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkCmdExecuteGeneratedCommandsNV" "commandBuffer"
                                 CommandBuffer
                              -> -- No documentation found for Nested "vkCmdExecuteGeneratedCommandsNV" "isPreprocessed"
                                 ("isPreprocessed" ::: Bool)
                              -> -- No documentation found for Nested "vkCmdExecuteGeneratedCommandsNV" "pGeneratedCommandsInfo"
                                 GeneratedCommandsInfoNV
                              -> io ()
cmdExecuteGeneratedCommandsNV commandBuffer isPreprocessed generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdExecuteGeneratedCommandsNVPtr = pVkCmdExecuteGeneratedCommandsNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdExecuteGeneratedCommandsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdExecuteGeneratedCommandsNV is null" Nothing Nothing
  let vkCmdExecuteGeneratedCommandsNV' = mkVkCmdExecuteGeneratedCommandsNV vkCmdExecuteGeneratedCommandsNVPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ vkCmdExecuteGeneratedCommandsNV' (commandBufferHandle (commandBuffer)) (boolToBool32 (isPreprocessed)) pGeneratedCommandsInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPreprocessGeneratedCommandsNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr GeneratedCommandsInfoNV -> IO ()) -> Ptr CommandBuffer_T -> Ptr GeneratedCommandsInfoNV -> IO ()

-- No documentation found for TopLevel "vkCmdPreprocessGeneratedCommandsNV"
cmdPreprocessGeneratedCommandsNV :: forall io
                                  . (MonadIO io)
                                 => -- No documentation found for Nested "vkCmdPreprocessGeneratedCommandsNV" "commandBuffer"
                                    CommandBuffer
                                 -> -- No documentation found for Nested "vkCmdPreprocessGeneratedCommandsNV" "pGeneratedCommandsInfo"
                                    GeneratedCommandsInfoNV
                                 -> io ()
cmdPreprocessGeneratedCommandsNV commandBuffer generatedCommandsInfo = liftIO . evalContT $ do
  let vkCmdPreprocessGeneratedCommandsNVPtr = pVkCmdPreprocessGeneratedCommandsNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdPreprocessGeneratedCommandsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPreprocessGeneratedCommandsNV is null" Nothing Nothing
  let vkCmdPreprocessGeneratedCommandsNV' = mkVkCmdPreprocessGeneratedCommandsNV vkCmdPreprocessGeneratedCommandsNVPtr
  pGeneratedCommandsInfo <- ContT $ withCStruct (generatedCommandsInfo)
  lift $ vkCmdPreprocessGeneratedCommandsNV' (commandBufferHandle (commandBuffer)) pGeneratedCommandsInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindPipelineShaderGroupNV
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdBindPipelineShaderGroupNV"
cmdBindPipelineShaderGroupNV :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCmdBindPipelineShaderGroupNV" "commandBuffer"
                                CommandBuffer
                             -> -- No documentation found for Nested "vkCmdBindPipelineShaderGroupNV" "pipelineBindPoint"
                                PipelineBindPoint
                             -> -- No documentation found for Nested "vkCmdBindPipelineShaderGroupNV" "pipeline"
                                Pipeline
                             -> -- No documentation found for Nested "vkCmdBindPipelineShaderGroupNV" "groupIndex"
                                ("groupIndex" ::: Word32)
                             -> io ()
cmdBindPipelineShaderGroupNV commandBuffer pipelineBindPoint pipeline groupIndex = liftIO $ do
  let vkCmdBindPipelineShaderGroupNVPtr = pVkCmdBindPipelineShaderGroupNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdBindPipelineShaderGroupNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindPipelineShaderGroupNV is null" Nothing Nothing
  let vkCmdBindPipelineShaderGroupNV' = mkVkCmdBindPipelineShaderGroupNV vkCmdBindPipelineShaderGroupNVPtr
  vkCmdBindPipelineShaderGroupNV' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (pipeline) (groupIndex)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetGeneratedCommandsMemoryRequirementsNV
  :: FunPtr (Ptr Device_T -> Ptr GeneratedCommandsMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2) -> IO ()) -> Ptr Device_T -> Ptr GeneratedCommandsMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2) -> IO ()

-- No documentation found for TopLevel "vkGetGeneratedCommandsMemoryRequirementsNV"
getGeneratedCommandsMemoryRequirementsNV :: forall a io
                                          . (Extendss MemoryRequirements2 a, PokeChain a, PeekChain a, MonadIO io)
                                         => -- No documentation found for Nested "vkGetGeneratedCommandsMemoryRequirementsNV" "device"
                                            Device
                                         -> -- No documentation found for Nested "vkGetGeneratedCommandsMemoryRequirementsNV" "pInfo"
                                            GeneratedCommandsMemoryRequirementsInfoNV
                                         -> io (MemoryRequirements2 a)
getGeneratedCommandsMemoryRequirementsNV device info = liftIO . evalContT $ do
  let vkGetGeneratedCommandsMemoryRequirementsNVPtr = pVkGetGeneratedCommandsMemoryRequirementsNV (deviceCmds (device :: Device))
  lift $ unless (vkGetGeneratedCommandsMemoryRequirementsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetGeneratedCommandsMemoryRequirementsNV is null" Nothing Nothing
  let vkGetGeneratedCommandsMemoryRequirementsNV' = mkVkGetGeneratedCommandsMemoryRequirementsNV vkGetGeneratedCommandsMemoryRequirementsNVPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2 _))
  lift $ vkGetGeneratedCommandsMemoryRequirementsNV' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2 _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIndirectCommandsLayoutNV
  :: FunPtr (Ptr Device_T -> Ptr IndirectCommandsLayoutCreateInfoNV -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutNV -> IO Result) -> Ptr Device_T -> Ptr IndirectCommandsLayoutCreateInfoNV -> Ptr AllocationCallbacks -> Ptr IndirectCommandsLayoutNV -> IO Result

-- No documentation found for TopLevel "vkCreateIndirectCommandsLayoutNV"
createIndirectCommandsLayoutNV :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkCreateIndirectCommandsLayoutNV" "device"
                                  Device
                               -> -- No documentation found for Nested "vkCreateIndirectCommandsLayoutNV" "pCreateInfo"
                                  IndirectCommandsLayoutCreateInfoNV
                               -> -- No documentation found for Nested "vkCreateIndirectCommandsLayoutNV" "pAllocator"
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io (IndirectCommandsLayoutNV)
createIndirectCommandsLayoutNV device createInfo allocator = liftIO . evalContT $ do
  let vkCreateIndirectCommandsLayoutNVPtr = pVkCreateIndirectCommandsLayoutNV (deviceCmds (device :: Device))
  lift $ unless (vkCreateIndirectCommandsLayoutNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateIndirectCommandsLayoutNV is null" Nothing Nothing
  let vkCreateIndirectCommandsLayoutNV' = mkVkCreateIndirectCommandsLayoutNV vkCreateIndirectCommandsLayoutNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPIndirectCommandsLayout <- ContT $ bracket (callocBytes @IndirectCommandsLayoutNV 8) free
  r <- lift $ vkCreateIndirectCommandsLayoutNV' (deviceHandle (device)) pCreateInfo pAllocator (pPIndirectCommandsLayout)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pIndirectCommandsLayout <- lift $ peek @IndirectCommandsLayoutNV pPIndirectCommandsLayout
  pure $ (pIndirectCommandsLayout)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createIndirectCommandsLayoutNV' and 'destroyIndirectCommandsLayoutNV'
--
-- To ensure that 'destroyIndirectCommandsLayoutNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withIndirectCommandsLayoutNV :: forall io r . MonadIO io => Device -> IndirectCommandsLayoutCreateInfoNV -> Maybe AllocationCallbacks -> (io IndirectCommandsLayoutNV -> (IndirectCommandsLayoutNV -> io ()) -> r) -> r
withIndirectCommandsLayoutNV device pCreateInfo pAllocator b =
  b (createIndirectCommandsLayoutNV device pCreateInfo pAllocator)
    (\(o0) -> destroyIndirectCommandsLayoutNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyIndirectCommandsLayoutNV
  :: FunPtr (Ptr Device_T -> IndirectCommandsLayoutNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> IndirectCommandsLayoutNV -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyIndirectCommandsLayoutNV"
destroyIndirectCommandsLayoutNV :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkDestroyIndirectCommandsLayoutNV" "device"
                                   Device
                                -> -- No documentation found for Nested "vkDestroyIndirectCommandsLayoutNV" "indirectCommandsLayout"
                                   IndirectCommandsLayoutNV
                                -> -- No documentation found for Nested "vkDestroyIndirectCommandsLayoutNV" "pAllocator"
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io ()
destroyIndirectCommandsLayoutNV device indirectCommandsLayout allocator = liftIO . evalContT $ do
  let vkDestroyIndirectCommandsLayoutNVPtr = pVkDestroyIndirectCommandsLayoutNV (deviceCmds (device :: Device))
  lift $ unless (vkDestroyIndirectCommandsLayoutNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyIndirectCommandsLayoutNV is null" Nothing Nothing
  let vkDestroyIndirectCommandsLayoutNV' = mkVkDestroyIndirectCommandsLayoutNV vkDestroyIndirectCommandsLayoutNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyIndirectCommandsLayoutNV' (deviceHandle (device)) (indirectCommandsLayout) pAllocator
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV"
data PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsFeaturesNV" "deviceGeneratedCommands"
    deviceGeneratedCommands :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsFeaturesNV)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsFeaturesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceGeneratedCommands))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  peekCStruct p = do
    deviceGeneratedCommands <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
             (bool32ToBool deviceGeneratedCommands)


instance Storable PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsFeaturesNV where
  zero = PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV"
data PhysicalDeviceDeviceGeneratedCommandsPropertiesNV = PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "maxGraphicsShaderGroupCount"
    maxGraphicsShaderGroupCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "maxIndirectSequenceCount"
    maxIndirectSequenceCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "maxIndirectCommandsTokenCount"
    maxIndirectCommandsTokenCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "maxIndirectCommandsStreamCount"
    maxIndirectCommandsStreamCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "maxIndirectCommandsTokenOffset"
    maxIndirectCommandsTokenOffset :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "maxIndirectCommandsStreamStride"
    maxIndirectCommandsStreamStride :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "minSequencesCountBufferOffsetAlignment"
    minSequencesCountBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "minSequencesIndexBufferOffsetAlignment"
    minSequencesIndexBufferOffsetAlignment :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDeviceGeneratedCommandsPropertiesNV" "minIndirectCommandsBufferOffsetAlignment"
    minIndirectCommandsBufferOffsetAlignment :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceDeviceGeneratedCommandsPropertiesNV)
#endif
deriving instance Show PhysicalDeviceDeviceGeneratedCommandsPropertiesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceDeviceGeneratedCommandsPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxGraphicsShaderGroupCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxIndirectSequenceCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxIndirectCommandsTokenCount)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxIndirectCommandsStreamCount)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxIndirectCommandsTokenOffset)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxIndirectCommandsStreamStride)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (minSequencesCountBufferOffsetAlignment)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (minSequencesIndexBufferOffsetAlignment)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (minIndirectCommandsBufferOffsetAlignment)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  peekCStruct p = do
    maxGraphicsShaderGroupCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxIndirectSequenceCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxIndirectCommandsTokenCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxIndirectCommandsStreamCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxIndirectCommandsTokenOffset <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxIndirectCommandsStreamStride <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    minSequencesCountBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    minSequencesIndexBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    minIndirectCommandsBufferOffsetAlignment <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pure $ PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
             maxGraphicsShaderGroupCount maxIndirectSequenceCount maxIndirectCommandsTokenCount maxIndirectCommandsStreamCount maxIndirectCommandsTokenOffset maxIndirectCommandsStreamStride minSequencesCountBufferOffsetAlignment minSequencesIndexBufferOffsetAlignment minIndirectCommandsBufferOffsetAlignment


instance Storable PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceDeviceGeneratedCommandsPropertiesNV where
  zero = PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkGraphicsShaderGroupCreateInfoNV"
data GraphicsShaderGroupCreateInfoNV = GraphicsShaderGroupCreateInfoNV
  { -- No documentation found for Nested "VkGraphicsShaderGroupCreateInfoNV" "pStages"
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- No documentation found for Nested "VkGraphicsShaderGroupCreateInfoNV" "pVertexInputState"
    vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
  , -- No documentation found for Nested "VkGraphicsShaderGroupCreateInfoNV" "pTessellationState"
    tessellationState :: Maybe (SomeStruct PipelineTessellationStateCreateInfo)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsShaderGroupCreateInfoNV)
#endif
deriving instance Show GraphicsShaderGroupCreateInfoNV

instance ToCStruct GraphicsShaderGroupCreateInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsShaderGroupCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    pVertexInputState'' <- case (vertexInputState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineVertexInputStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineVertexInputStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo _)))) pVertexInputState''
    pTessellationState'' <- case (tessellationState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineTessellationStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineTessellationStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (PipelineTessellationStateCreateInfo _)))) pTessellationState''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    lift $ f

instance FromCStruct GraphicsShaderGroupCreateInfoNV where
  peekCStruct p = do
    stageCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo a))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    pVertexInputState <- peek @(Ptr (PipelineVertexInputStateCreateInfo _)) ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo a))))
    pVertexInputState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pVertexInputState
    pTessellationState <- peek @(Ptr (PipelineTessellationStateCreateInfo _)) ((p `plusPtr` 40 :: Ptr (Ptr (PipelineTessellationStateCreateInfo a))))
    pTessellationState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pTessellationState
    pure $ GraphicsShaderGroupCreateInfoNV
             pStages' pVertexInputState' pTessellationState'

instance Zero GraphicsShaderGroupCreateInfoNV where
  zero = GraphicsShaderGroupCreateInfoNV
           mempty
           Nothing
           Nothing



-- No documentation found for TopLevel "VkGraphicsPipelineShaderGroupsCreateInfoNV"
data GraphicsPipelineShaderGroupsCreateInfoNV = GraphicsPipelineShaderGroupsCreateInfoNV
  { -- No documentation found for Nested "VkGraphicsPipelineShaderGroupsCreateInfoNV" "pGroups"
    groups :: Vector GraphicsShaderGroupCreateInfoNV
  , -- No documentation found for Nested "VkGraphicsPipelineShaderGroupsCreateInfoNV" "pPipelines"
    pipelines :: Vector Pipeline
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsPipelineShaderGroupsCreateInfoNV)
#endif
deriving instance Show GraphicsPipelineShaderGroupsCreateInfoNV

instance ToCStruct GraphicsPipelineShaderGroupsCreateInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsPipelineShaderGroupsCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytesAligned @GraphicsShaderGroupCreateInfoNV ((Data.Vector.length (groups)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV) (e) . ($ ())) (groups)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pipelines)) :: Word32))
    pPPipelines' <- ContT $ allocaBytesAligned @Pipeline ((Data.Vector.length (pipelines)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelines' `plusPtr` (8 * (i)) :: Ptr Pipeline) (e)) (pipelines)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Pipeline))) (pPPipelines')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPGroups' <- ContT $ allocaBytesAligned @GraphicsShaderGroupCreateInfoNV ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPGroups' `plusPtr` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV))) (pPGroups')
    pPPipelines' <- ContT $ allocaBytesAligned @Pipeline ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPipelines' `plusPtr` (8 * (i)) :: Ptr Pipeline) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Pipeline))) (pPPipelines')
    lift $ f

instance FromCStruct GraphicsPipelineShaderGroupsCreateInfoNV where
  peekCStruct p = do
    groupCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pGroups <- peek @(Ptr GraphicsShaderGroupCreateInfoNV) ((p `plusPtr` 24 :: Ptr (Ptr GraphicsShaderGroupCreateInfoNV)))
    pGroups' <- generateM (fromIntegral groupCount) (\i -> peekCStruct @GraphicsShaderGroupCreateInfoNV ((pGroups `advancePtrBytes` (48 * (i)) :: Ptr GraphicsShaderGroupCreateInfoNV)))
    pipelineCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPipelines <- peek @(Ptr Pipeline) ((p `plusPtr` 40 :: Ptr (Ptr Pipeline)))
    pPipelines' <- generateM (fromIntegral pipelineCount) (\i -> peek @Pipeline ((pPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
    pure $ GraphicsPipelineShaderGroupsCreateInfoNV
             pGroups' pPipelines'

instance Zero GraphicsPipelineShaderGroupsCreateInfoNV where
  zero = GraphicsPipelineShaderGroupsCreateInfoNV
           mempty
           mempty



-- No documentation found for TopLevel "VkBindShaderGroupIndirectCommandNV"
data BindShaderGroupIndirectCommandNV = BindShaderGroupIndirectCommandNV
  { -- No documentation found for Nested "VkBindShaderGroupIndirectCommandNV" "groupIndex"
    groupIndex :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindShaderGroupIndirectCommandNV)
#endif
deriving instance Show BindShaderGroupIndirectCommandNV

instance ToCStruct BindShaderGroupIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 4 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindShaderGroupIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (groupIndex)
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct BindShaderGroupIndirectCommandNV where
  peekCStruct p = do
    groupIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pure $ BindShaderGroupIndirectCommandNV
             groupIndex


instance Storable BindShaderGroupIndirectCommandNV where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindShaderGroupIndirectCommandNV where
  zero = BindShaderGroupIndirectCommandNV
           zero



-- No documentation found for TopLevel "VkBindIndexBufferIndirectCommandNV"
data BindIndexBufferIndirectCommandNV = BindIndexBufferIndirectCommandNV
  { -- No documentation found for Nested "VkBindIndexBufferIndirectCommandNV" "bufferAddress"
    bufferAddress :: DeviceAddress
  , -- No documentation found for Nested "VkBindIndexBufferIndirectCommandNV" "size"
    size :: Word32
  , -- No documentation found for Nested "VkBindIndexBufferIndirectCommandNV" "indexType"
    indexType :: IndexType
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindIndexBufferIndirectCommandNV)
#endif
deriving instance Show BindIndexBufferIndirectCommandNV

instance ToCStruct BindIndexBufferIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindIndexBufferIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (bufferAddress)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (size)
    poke ((p `plusPtr` 12 :: Ptr IndexType)) (indexType)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr IndexType)) (zero)
    f

instance FromCStruct BindIndexBufferIndirectCommandNV where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    indexType <- peek @IndexType ((p `plusPtr` 12 :: Ptr IndexType))
    pure $ BindIndexBufferIndirectCommandNV
             bufferAddress size indexType


instance Storable BindIndexBufferIndirectCommandNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindIndexBufferIndirectCommandNV where
  zero = BindIndexBufferIndirectCommandNV
           zero
           zero
           zero



-- No documentation found for TopLevel "VkBindVertexBufferIndirectCommandNV"
data BindVertexBufferIndirectCommandNV = BindVertexBufferIndirectCommandNV
  { -- No documentation found for Nested "VkBindVertexBufferIndirectCommandNV" "bufferAddress"
    bufferAddress :: DeviceAddress
  , -- No documentation found for Nested "VkBindVertexBufferIndirectCommandNV" "size"
    size :: Word32
  , -- No documentation found for Nested "VkBindVertexBufferIndirectCommandNV" "stride"
    stride :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindVertexBufferIndirectCommandNV)
#endif
deriving instance Show BindVertexBufferIndirectCommandNV

instance ToCStruct BindVertexBufferIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindVertexBufferIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (bufferAddress)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (size)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (stride)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct BindVertexBufferIndirectCommandNV where
  peekCStruct p = do
    bufferAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ BindVertexBufferIndirectCommandNV
             bufferAddress size stride


instance Storable BindVertexBufferIndirectCommandNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BindVertexBufferIndirectCommandNV where
  zero = BindVertexBufferIndirectCommandNV
           zero
           zero
           zero



-- No documentation found for TopLevel "VkSetStateFlagsIndirectCommandNV"
data SetStateFlagsIndirectCommandNV = SetStateFlagsIndirectCommandNV
  { -- No documentation found for Nested "VkSetStateFlagsIndirectCommandNV" "data"
    data' :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SetStateFlagsIndirectCommandNV)
#endif
deriving instance Show SetStateFlagsIndirectCommandNV

instance ToCStruct SetStateFlagsIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 4 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SetStateFlagsIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (data')
    f
  cStructSize = 4
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    f

instance FromCStruct SetStateFlagsIndirectCommandNV where
  peekCStruct p = do
    data' <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pure $ SetStateFlagsIndirectCommandNV
             data'


instance Storable SetStateFlagsIndirectCommandNV where
  sizeOf ~_ = 4
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SetStateFlagsIndirectCommandNV where
  zero = SetStateFlagsIndirectCommandNV
           zero



-- No documentation found for TopLevel "VkIndirectCommandsStreamNV"
data IndirectCommandsStreamNV = IndirectCommandsStreamNV
  { -- No documentation found for Nested "VkIndirectCommandsStreamNV" "buffer"
    buffer :: Buffer
  , -- No documentation found for Nested "VkIndirectCommandsStreamNV" "offset"
    offset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsStreamNV)
#endif
deriving instance Show IndirectCommandsStreamNV

instance ToCStruct IndirectCommandsStreamNV where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsStreamNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (offset)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct IndirectCommandsStreamNV where
  peekCStruct p = do
    buffer <- peek @Buffer ((p `plusPtr` 0 :: Ptr Buffer))
    offset <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    pure $ IndirectCommandsStreamNV
             buffer offset


instance Storable IndirectCommandsStreamNV where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero IndirectCommandsStreamNV where
  zero = IndirectCommandsStreamNV
           zero
           zero



-- No documentation found for TopLevel "VkIndirectCommandsLayoutTokenNV"
data IndirectCommandsLayoutTokenNV = IndirectCommandsLayoutTokenNV
  { -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "tokenType"
    tokenType :: IndirectCommandsTokenTypeNV
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "stream"
    stream :: Word32
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "offset"
    offset :: Word32
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "vertexBindingUnit"
    vertexBindingUnit :: Word32
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "vertexDynamicStride"
    vertexDynamicStride :: Bool
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pushconstantPipelineLayout"
    pushconstantPipelineLayout :: PipelineLayout
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pushconstantShaderStageFlags"
    pushconstantShaderStageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pushconstantOffset"
    pushconstantOffset :: Word32
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pushconstantSize"
    pushconstantSize :: Word32
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "indirectStateFlags"
    indirectStateFlags :: IndirectStateFlagsNV
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pIndexTypes"
    indexTypes :: Vector IndexType
  , -- No documentation found for Nested "VkIndirectCommandsLayoutTokenNV" "pIndexTypeValues"
    indexTypeValues :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsLayoutTokenNV)
#endif
deriving instance Show IndirectCommandsLayoutTokenNV

instance ToCStruct IndirectCommandsLayoutTokenNV where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutTokenNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV)) (tokenType)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (stream)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (offset)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (vertexBindingUnit)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (vertexDynamicStride))
    lift $ poke ((p `plusPtr` 40 :: Ptr PipelineLayout)) (pushconstantPipelineLayout)
    lift $ poke ((p `plusPtr` 48 :: Ptr ShaderStageFlags)) (pushconstantShaderStageFlags)
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (pushconstantOffset)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (pushconstantSize)
    lift $ poke ((p `plusPtr` 60 :: Ptr IndirectStateFlagsNV)) (indirectStateFlags)
    let pIndexTypesLength = Data.Vector.length $ (indexTypes)
    lift $ unless ((Data.Vector.length $ (indexTypeValues)) == pIndexTypesLength) $
      throwIO $ IOError Nothing InvalidArgument "" "pIndexTypeValues and pIndexTypes must have the same length" Nothing Nothing
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral pIndexTypesLength :: Word32))
    pPIndexTypes' <- ContT $ allocaBytesAligned @IndexType ((Data.Vector.length (indexTypes)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypes' `plusPtr` (4 * (i)) :: Ptr IndexType) (e)) (indexTypes)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr IndexType))) (pPIndexTypes')
    pPIndexTypeValues' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (indexTypeValues)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypeValues' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (indexTypeValues)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPIndexTypeValues')
    lift $ f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    pPIndexTypes' <- ContT $ allocaBytesAligned @IndexType ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypes' `plusPtr` (4 * (i)) :: Ptr IndexType) (e)) (mempty)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr IndexType))) (pPIndexTypes')
    pPIndexTypeValues' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPIndexTypeValues' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr Word32))) (pPIndexTypeValues')
    lift $ f

instance FromCStruct IndirectCommandsLayoutTokenNV where
  peekCStruct p = do
    tokenType <- peek @IndirectCommandsTokenTypeNV ((p `plusPtr` 16 :: Ptr IndirectCommandsTokenTypeNV))
    stream <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    offset <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    vertexBindingUnit <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    vertexDynamicStride <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pushconstantPipelineLayout <- peek @PipelineLayout ((p `plusPtr` 40 :: Ptr PipelineLayout))
    pushconstantShaderStageFlags <- peek @ShaderStageFlags ((p `plusPtr` 48 :: Ptr ShaderStageFlags))
    pushconstantOffset <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    pushconstantSize <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    indirectStateFlags <- peek @IndirectStateFlagsNV ((p `plusPtr` 60 :: Ptr IndirectStateFlagsNV))
    indexTypeCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pIndexTypes <- peek @(Ptr IndexType) ((p `plusPtr` 72 :: Ptr (Ptr IndexType)))
    pIndexTypes' <- generateM (fromIntegral indexTypeCount) (\i -> peek @IndexType ((pIndexTypes `advancePtrBytes` (4 * (i)) :: Ptr IndexType)))
    pIndexTypeValues <- peek @(Ptr Word32) ((p `plusPtr` 80 :: Ptr (Ptr Word32)))
    pIndexTypeValues' <- generateM (fromIntegral indexTypeCount) (\i -> peek @Word32 ((pIndexTypeValues `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ IndirectCommandsLayoutTokenNV
             tokenType stream offset vertexBindingUnit (bool32ToBool vertexDynamicStride) pushconstantPipelineLayout pushconstantShaderStageFlags pushconstantOffset pushconstantSize indirectStateFlags pIndexTypes' pIndexTypeValues'

instance Zero IndirectCommandsLayoutTokenNV where
  zero = IndirectCommandsLayoutTokenNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           mempty
           mempty



-- No documentation found for TopLevel "VkIndirectCommandsLayoutCreateInfoNV"
data IndirectCommandsLayoutCreateInfoNV = IndirectCommandsLayoutCreateInfoNV
  { -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNV" "flags"
    flags :: IndirectCommandsLayoutUsageFlagsNV
  , -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNV" "pipelineBindPoint"
    pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNV" "pTokens"
    tokens :: Vector IndirectCommandsLayoutTokenNV
  , -- No documentation found for Nested "VkIndirectCommandsLayoutCreateInfoNV" "pStreamStrides"
    streamStrides :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (IndirectCommandsLayoutCreateInfoNV)
#endif
deriving instance Show IndirectCommandsLayoutCreateInfoNV

instance ToCStruct IndirectCommandsLayoutCreateInfoNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p IndirectCommandsLayoutCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (tokens)) :: Word32))
    pPTokens' <- ContT $ allocaBytesAligned @IndirectCommandsLayoutTokenNV ((Data.Vector.length (tokens)) * 88) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTokens' `plusPtr` (88 * (i)) :: Ptr IndirectCommandsLayoutTokenNV) (e) . ($ ())) (tokens)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNV))) (pPTokens')
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (streamStrides)) :: Word32))
    pPStreamStrides' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (streamStrides)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreamStrides' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (streamStrides)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPStreamStrides')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr PipelineBindPoint)) (zero)
    pPTokens' <- ContT $ allocaBytesAligned @IndirectCommandsLayoutTokenNV ((Data.Vector.length (mempty)) * 88) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPTokens' `plusPtr` (88 * (i)) :: Ptr IndirectCommandsLayoutTokenNV) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNV))) (pPTokens')
    pPStreamStrides' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreamStrides' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPStreamStrides')
    lift $ f

instance FromCStruct IndirectCommandsLayoutCreateInfoNV where
  peekCStruct p = do
    flags <- peek @IndirectCommandsLayoutUsageFlagsNV ((p `plusPtr` 16 :: Ptr IndirectCommandsLayoutUsageFlagsNV))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 20 :: Ptr PipelineBindPoint))
    tokenCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pTokens <- peek @(Ptr IndirectCommandsLayoutTokenNV) ((p `plusPtr` 32 :: Ptr (Ptr IndirectCommandsLayoutTokenNV)))
    pTokens' <- generateM (fromIntegral tokenCount) (\i -> peekCStruct @IndirectCommandsLayoutTokenNV ((pTokens `advancePtrBytes` (88 * (i)) :: Ptr IndirectCommandsLayoutTokenNV)))
    streamCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pStreamStrides <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pStreamStrides' <- generateM (fromIntegral streamCount) (\i -> peek @Word32 ((pStreamStrides `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ IndirectCommandsLayoutCreateInfoNV
             flags pipelineBindPoint pTokens' pStreamStrides'

instance Zero IndirectCommandsLayoutCreateInfoNV where
  zero = IndirectCommandsLayoutCreateInfoNV
           zero
           zero
           mempty
           mempty



-- No documentation found for TopLevel "VkGeneratedCommandsInfoNV"
data GeneratedCommandsInfoNV = GeneratedCommandsInfoNV
  { -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "pipelineBindPoint"
    pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "pipeline"
    pipeline :: Pipeline
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "indirectCommandsLayout"
    indirectCommandsLayout :: IndirectCommandsLayoutNV
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "pStreams"
    streams :: Vector IndirectCommandsStreamNV
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "sequencesCount"
    sequencesCount :: Word32
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "preprocessBuffer"
    preprocessBuffer :: Buffer
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "preprocessOffset"
    preprocessOffset :: DeviceSize
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "preprocessSize"
    preprocessSize :: DeviceSize
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "sequencesCountBuffer"
    sequencesCountBuffer :: Buffer
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "sequencesCountOffset"
    sequencesCountOffset :: DeviceSize
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "sequencesIndexBuffer"
    sequencesIndexBuffer :: Buffer
  , -- No documentation found for Nested "VkGeneratedCommandsInfoNV" "sequencesIndexOffset"
    sequencesIndexOffset :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeneratedCommandsInfoNV)
#endif
deriving instance Show GeneratedCommandsInfoNV

instance ToCStruct GeneratedCommandsInfoNV where
  withCStruct x f = allocaBytesAligned 120 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    lift $ poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (indirectCommandsLayout)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (streams)) :: Word32))
    pPStreams' <- ContT $ allocaBytesAligned @IndirectCommandsStreamNV ((Data.Vector.length (streams)) * 16) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreams' `plusPtr` (16 * (i)) :: Ptr IndirectCommandsStreamNV) (e)) (streams)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsStreamNV))) (pPStreams')
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (sequencesCount)
    lift $ poke ((p `plusPtr` 64 :: Ptr Buffer)) (preprocessBuffer)
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (preprocessOffset)
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (preprocessSize)
    lift $ poke ((p `plusPtr` 88 :: Ptr Buffer)) (sequencesCountBuffer)
    lift $ poke ((p `plusPtr` 96 :: Ptr DeviceSize)) (sequencesCountOffset)
    lift $ poke ((p `plusPtr` 104 :: Ptr Buffer)) (sequencesIndexBuffer)
    lift $ poke ((p `plusPtr` 112 :: Ptr DeviceSize)) (sequencesIndexOffset)
    lift $ f
  cStructSize = 120
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (zero)
    pPStreams' <- ContT $ allocaBytesAligned @IndirectCommandsStreamNV ((Data.Vector.length (mempty)) * 16) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPStreams' `plusPtr` (16 * (i)) :: Ptr IndirectCommandsStreamNV) (e)) (mempty)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsStreamNV))) (pPStreams')
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr Buffer)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 80 :: Ptr DeviceSize)) (zero)
    lift $ f

instance FromCStruct GeneratedCommandsInfoNV where
  peekCStruct p = do
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 16 :: Ptr PipelineBindPoint))
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutNV ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV))
    streamCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pStreams <- peek @(Ptr IndirectCommandsStreamNV) ((p `plusPtr` 48 :: Ptr (Ptr IndirectCommandsStreamNV)))
    pStreams' <- generateM (fromIntegral streamCount) (\i -> peekCStruct @IndirectCommandsStreamNV ((pStreams `advancePtrBytes` (16 * (i)) :: Ptr IndirectCommandsStreamNV)))
    sequencesCount <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    preprocessBuffer <- peek @Buffer ((p `plusPtr` 64 :: Ptr Buffer))
    preprocessOffset <- peek @DeviceSize ((p `plusPtr` 72 :: Ptr DeviceSize))
    preprocessSize <- peek @DeviceSize ((p `plusPtr` 80 :: Ptr DeviceSize))
    sequencesCountBuffer <- peek @Buffer ((p `plusPtr` 88 :: Ptr Buffer))
    sequencesCountOffset <- peek @DeviceSize ((p `plusPtr` 96 :: Ptr DeviceSize))
    sequencesIndexBuffer <- peek @Buffer ((p `plusPtr` 104 :: Ptr Buffer))
    sequencesIndexOffset <- peek @DeviceSize ((p `plusPtr` 112 :: Ptr DeviceSize))
    pure $ GeneratedCommandsInfoNV
             pipelineBindPoint pipeline indirectCommandsLayout pStreams' sequencesCount preprocessBuffer preprocessOffset preprocessSize sequencesCountBuffer sequencesCountOffset sequencesIndexBuffer sequencesIndexOffset

instance Zero GeneratedCommandsInfoNV where
  zero = GeneratedCommandsInfoNV
           zero
           zero
           zero
           mempty
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkGeneratedCommandsMemoryRequirementsInfoNV"
data GeneratedCommandsMemoryRequirementsInfoNV = GeneratedCommandsMemoryRequirementsInfoNV
  { -- No documentation found for Nested "VkGeneratedCommandsMemoryRequirementsInfoNV" "pipelineBindPoint"
    pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "VkGeneratedCommandsMemoryRequirementsInfoNV" "pipeline"
    pipeline :: Pipeline
  , -- No documentation found for Nested "VkGeneratedCommandsMemoryRequirementsInfoNV" "indirectCommandsLayout"
    indirectCommandsLayout :: IndirectCommandsLayoutNV
  , -- No documentation found for Nested "VkGeneratedCommandsMemoryRequirementsInfoNV" "maxSequencesCount"
    maxSequencesCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeneratedCommandsMemoryRequirementsInfoNV)
#endif
deriving instance Show GeneratedCommandsMemoryRequirementsInfoNV

instance ToCStruct GeneratedCommandsMemoryRequirementsInfoNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeneratedCommandsMemoryRequirementsInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (pipeline)
    poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (indirectCommandsLayout)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxSequencesCount)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineBindPoint)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Pipeline)) (zero)
    poke ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    f

instance FromCStruct GeneratedCommandsMemoryRequirementsInfoNV where
  peekCStruct p = do
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 16 :: Ptr PipelineBindPoint))
    pipeline <- peek @Pipeline ((p `plusPtr` 24 :: Ptr Pipeline))
    indirectCommandsLayout <- peek @IndirectCommandsLayoutNV ((p `plusPtr` 32 :: Ptr IndirectCommandsLayoutNV))
    maxSequencesCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pure $ GeneratedCommandsMemoryRequirementsInfoNV
             pipelineBindPoint pipeline indirectCommandsLayout maxSequencesCount


instance Storable GeneratedCommandsMemoryRequirementsInfoNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeneratedCommandsMemoryRequirementsInfoNV where
  zero = GeneratedCommandsMemoryRequirementsInfoNV
           zero
           zero
           zero
           zero


type IndirectCommandsLayoutUsageFlagsNV = IndirectCommandsLayoutUsageFlagBitsNV

-- No documentation found for TopLevel "VkIndirectCommandsLayoutUsageFlagBitsNV"
newtype IndirectCommandsLayoutUsageFlagBitsNV = IndirectCommandsLayoutUsageFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNV" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000001
-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNV" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV   = IndirectCommandsLayoutUsageFlagBitsNV 0x00000002
-- No documentation found for Nested "VkIndirectCommandsLayoutUsageFlagBitsNV" "VK_INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV"
pattern INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV = IndirectCommandsLayoutUsageFlagBitsNV 0x00000004

conNameIndirectCommandsLayoutUsageFlagBitsNV :: String
conNameIndirectCommandsLayoutUsageFlagBitsNV = "IndirectCommandsLayoutUsageFlagBitsNV"

enumPrefixIndirectCommandsLayoutUsageFlagBitsNV :: String
enumPrefixIndirectCommandsLayoutUsageFlagBitsNV = "INDIRECT_COMMANDS_LAYOUT_USAGE_"

showTableIndirectCommandsLayoutUsageFlagBitsNV :: [(IndirectCommandsLayoutUsageFlagBitsNV, String)]
showTableIndirectCommandsLayoutUsageFlagBitsNV =
  [ (INDIRECT_COMMANDS_LAYOUT_USAGE_EXPLICIT_PREPROCESS_BIT_NV, "EXPLICIT_PREPROCESS_BIT_NV")
  , (INDIRECT_COMMANDS_LAYOUT_USAGE_INDEXED_SEQUENCES_BIT_NV  , "INDEXED_SEQUENCES_BIT_NV")
  , (INDIRECT_COMMANDS_LAYOUT_USAGE_UNORDERED_SEQUENCES_BIT_NV, "UNORDERED_SEQUENCES_BIT_NV")
  ]


instance Show IndirectCommandsLayoutUsageFlagBitsNV where
showsPrec = enumShowsPrec enumPrefixIndirectCommandsLayoutUsageFlagBitsNV
                          showTableIndirectCommandsLayoutUsageFlagBitsNV
                          conNameIndirectCommandsLayoutUsageFlagBitsNV
                          (\(IndirectCommandsLayoutUsageFlagBitsNV x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read IndirectCommandsLayoutUsageFlagBitsNV where
  readPrec = enumReadPrec enumPrefixIndirectCommandsLayoutUsageFlagBitsNV
                          showTableIndirectCommandsLayoutUsageFlagBitsNV
                          conNameIndirectCommandsLayoutUsageFlagBitsNV
                          IndirectCommandsLayoutUsageFlagBitsNV


type IndirectStateFlagsNV = IndirectStateFlagBitsNV

-- No documentation found for TopLevel "VkIndirectStateFlagBitsNV"
newtype IndirectStateFlagBitsNV = IndirectStateFlagBitsNV Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkIndirectStateFlagBitsNV" "VK_INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV"
pattern INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV = IndirectStateFlagBitsNV 0x00000001

conNameIndirectStateFlagBitsNV :: String
conNameIndirectStateFlagBitsNV = "IndirectStateFlagBitsNV"

enumPrefixIndirectStateFlagBitsNV :: String
enumPrefixIndirectStateFlagBitsNV = "INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV"

showTableIndirectStateFlagBitsNV :: [(IndirectStateFlagBitsNV, String)]
showTableIndirectStateFlagBitsNV = [(INDIRECT_STATE_FLAG_FRONTFACE_BIT_NV, "")]


instance Show IndirectStateFlagBitsNV where
showsPrec = enumShowsPrec enumPrefixIndirectStateFlagBitsNV
                          showTableIndirectStateFlagBitsNV
                          conNameIndirectStateFlagBitsNV
                          (\(IndirectStateFlagBitsNV x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read IndirectStateFlagBitsNV where
  readPrec = enumReadPrec enumPrefixIndirectStateFlagBitsNV
                          showTableIndirectStateFlagBitsNV
                          conNameIndirectStateFlagBitsNV
                          IndirectStateFlagBitsNV


-- No documentation found for TopLevel "VkIndirectCommandsTokenTypeNV"
newtype IndirectCommandsTokenTypeNV = IndirectCommandsTokenTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV  = IndirectCommandsTokenTypeNV 0
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV   = IndirectCommandsTokenTypeNV 1
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV  = IndirectCommandsTokenTypeNV 2
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV = IndirectCommandsTokenTypeNV 3
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV = IndirectCommandsTokenTypeNV 4
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV  = IndirectCommandsTokenTypeNV 5
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV          = IndirectCommandsTokenTypeNV 6
-- No documentation found for Nested "VkIndirectCommandsTokenTypeNV" "VK_INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV"
pattern INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV    = IndirectCommandsTokenTypeNV 7
{-# complete INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV,
             INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV :: IndirectCommandsTokenTypeNV #-}

conNameIndirectCommandsTokenTypeNV :: String
conNameIndirectCommandsTokenTypeNV = "IndirectCommandsTokenTypeNV"

enumPrefixIndirectCommandsTokenTypeNV :: String
enumPrefixIndirectCommandsTokenTypeNV = "INDIRECT_COMMANDS_TOKEN_TYPE_"

showTableIndirectCommandsTokenTypeNV :: [(IndirectCommandsTokenTypeNV, String)]
showTableIndirectCommandsTokenTypeNV =
  [ (INDIRECT_COMMANDS_TOKEN_TYPE_SHADER_GROUP_NV , "SHADER_GROUP_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_STATE_FLAGS_NV  , "STATE_FLAGS_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_INDEX_BUFFER_NV , "INDEX_BUFFER_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_VERTEX_BUFFER_NV, "VERTEX_BUFFER_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_PUSH_CONSTANT_NV, "PUSH_CONSTANT_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_INDEXED_NV , "DRAW_INDEXED_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_NV         , "DRAW_NV")
  , (INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_TASKS_NV   , "DRAW_TASKS_NV")
  ]


instance Show IndirectCommandsTokenTypeNV where
showsPrec = enumShowsPrec enumPrefixIndirectCommandsTokenTypeNV
                          showTableIndirectCommandsTokenTypeNV
                          conNameIndirectCommandsTokenTypeNV
                          (\(IndirectCommandsTokenTypeNV x) -> x)
                          (showsPrec 11)


instance Read IndirectCommandsTokenTypeNV where
  readPrec = enumReadPrec enumPrefixIndirectCommandsTokenTypeNV
                          showTableIndirectCommandsTokenTypeNV
                          conNameIndirectCommandsTokenTypeNV
                          IndirectCommandsTokenTypeNV


type NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION"
pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION :: forall a . Integral a => a
pattern NV_DEVICE_GENERATED_COMMANDS_SPEC_VERSION = 3


type NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NV_device_generated_commands"

-- No documentation found for TopLevel "VK_NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME"
pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_DEVICE_GENERATED_COMMANDS_EXTENSION_NAME = "VK_NV_device_generated_commands"

