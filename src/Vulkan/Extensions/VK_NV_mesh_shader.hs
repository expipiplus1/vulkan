{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_mesh_shader"
module Vulkan.Extensions.VK_NV_mesh_shader  ( cmdDrawMeshTasksNV
                                            , cmdDrawMeshTasksIndirectNV
                                            , cmdDrawMeshTasksIndirectCountNV
                                            , PhysicalDeviceMeshShaderFeaturesNV(..)
                                            , PhysicalDeviceMeshShaderPropertiesNV(..)
                                            , DrawMeshTasksIndirectCommandNV(..)
                                            , NV_MESH_SHADER_SPEC_VERSION
                                            , pattern NV_MESH_SHADER_SPEC_VERSION
                                            , NV_MESH_SHADER_EXTENSION_NAME
                                            , pattern NV_MESH_SHADER_EXTENSION_NAME
                                            ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Data.Kind (Type)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksIndirectCountNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksIndirectNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDrawMeshTasksNV))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawMeshTasksNV"
cmdDrawMeshTasksNV :: forall io
                    . (MonadIO io)
                   => -- No documentation found for Nested "vkCmdDrawMeshTasksNV" "commandBuffer"
                      CommandBuffer
                   -> -- No documentation found for Nested "vkCmdDrawMeshTasksNV" "taskCount"
                      ("taskCount" ::: Word32)
                   -> -- No documentation found for Nested "vkCmdDrawMeshTasksNV" "firstTask"
                      ("firstTask" ::: Word32)
                   -> io ()
cmdDrawMeshTasksNV commandBuffer taskCount firstTask = liftIO $ do
  let vkCmdDrawMeshTasksNVPtr = pVkCmdDrawMeshTasksNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawMeshTasksNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawMeshTasksNV is null" Nothing Nothing
  let vkCmdDrawMeshTasksNV' = mkVkCmdDrawMeshTasksNV vkCmdDrawMeshTasksNVPtr
  vkCmdDrawMeshTasksNV' (commandBufferHandle (commandBuffer)) (taskCount) (firstTask)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksIndirectNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawMeshTasksIndirectNV"
cmdDrawMeshTasksIndirectNV :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectNV" "commandBuffer"
                              CommandBuffer
                           -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectNV" "buffer"
                              Buffer
                           -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectNV" "offset"
                              ("offset" ::: DeviceSize)
                           -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectNV" "drawCount"
                              ("drawCount" ::: Word32)
                           -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectNV" "stride"
                              ("stride" ::: Word32)
                           -> io ()
cmdDrawMeshTasksIndirectNV commandBuffer buffer offset drawCount stride = liftIO $ do
  let vkCmdDrawMeshTasksIndirectNVPtr = pVkCmdDrawMeshTasksIndirectNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawMeshTasksIndirectNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawMeshTasksIndirectNV is null" Nothing Nothing
  let vkCmdDrawMeshTasksIndirectNV' = mkVkCmdDrawMeshTasksIndirectNV vkCmdDrawMeshTasksIndirectNVPtr
  vkCmdDrawMeshTasksIndirectNV' (commandBufferHandle (commandBuffer)) (buffer) (offset) (drawCount) (stride)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksIndirectCountNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> Word32 -> Word32 -> IO ()

-- No documentation found for TopLevel "vkCmdDrawMeshTasksIndirectCountNV"
cmdDrawMeshTasksIndirectCountNV :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectCountNV" "commandBuffer"
                                   CommandBuffer
                                -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectCountNV" "buffer"
                                   Buffer
                                -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectCountNV" "offset"
                                   ("offset" ::: DeviceSize)
                                -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectCountNV" "countBuffer"
                                   ("countBuffer" ::: Buffer)
                                -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectCountNV" "countBufferOffset"
                                   ("countBufferOffset" ::: DeviceSize)
                                -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectCountNV" "maxDrawCount"
                                   ("maxDrawCount" ::: Word32)
                                -> -- No documentation found for Nested "vkCmdDrawMeshTasksIndirectCountNV" "stride"
                                   ("stride" ::: Word32)
                                -> io ()
cmdDrawMeshTasksIndirectCountNV commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride = liftIO $ do
  let vkCmdDrawMeshTasksIndirectCountNVPtr = pVkCmdDrawMeshTasksIndirectCountNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdDrawMeshTasksIndirectCountNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDrawMeshTasksIndirectCountNV is null" Nothing Nothing
  let vkCmdDrawMeshTasksIndirectCountNV' = mkVkCmdDrawMeshTasksIndirectCountNV vkCmdDrawMeshTasksIndirectCountNVPtr
  vkCmdDrawMeshTasksIndirectCountNV' (commandBufferHandle (commandBuffer)) (buffer) (offset) (countBuffer) (countBufferOffset) (maxDrawCount) (stride)
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceMeshShaderFeaturesNV"
data PhysicalDeviceMeshShaderFeaturesNV = PhysicalDeviceMeshShaderFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceMeshShaderFeaturesNV" "taskShader"
    taskShader :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderFeaturesNV" "meshShader"
    meshShader :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMeshShaderFeaturesNV)
#endif
deriving instance Show PhysicalDeviceMeshShaderFeaturesNV

instance ToCStruct PhysicalDeviceMeshShaderFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMeshShaderFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (taskShader))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (meshShader))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMeshShaderFeaturesNV where
  peekCStruct p = do
    taskShader <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    meshShader <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceMeshShaderFeaturesNV
             (bool32ToBool taskShader) (bool32ToBool meshShader)


instance Storable PhysicalDeviceMeshShaderFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMeshShaderFeaturesNV where
  zero = PhysicalDeviceMeshShaderFeaturesNV
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceMeshShaderPropertiesNV"
data PhysicalDeviceMeshShaderPropertiesNV = PhysicalDeviceMeshShaderPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxDrawMeshTasksCount"
    maxDrawMeshTasksCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxTaskWorkGroupInvocations"
    maxTaskWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxTaskWorkGroupSize"
    maxTaskWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxTaskTotalMemorySize"
    maxTaskTotalMemorySize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxTaskOutputCount"
    maxTaskOutputCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshWorkGroupInvocations"
    maxMeshWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshWorkGroupSize"
    maxMeshWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshTotalMemorySize"
    maxMeshTotalMemorySize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshOutputVertices"
    maxMeshOutputVertices :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshOutputPrimitives"
    maxMeshOutputPrimitives :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "maxMeshMultiviewViewCount"
    maxMeshMultiviewViewCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "meshOutputPerVertexGranularity"
    meshOutputPerVertexGranularity :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMeshShaderPropertiesNV" "meshOutputPerPrimitiveGranularity"
    meshOutputPerPrimitiveGranularity :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMeshShaderPropertiesNV)
#endif
deriving instance Show PhysicalDeviceMeshShaderPropertiesNV

instance ToCStruct PhysicalDeviceMeshShaderPropertiesNV where
  withCStruct x f = allocaBytesAligned 88 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMeshShaderPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxDrawMeshTasksCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxTaskWorkGroupInvocations)
    let pMaxTaskWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 3 Word32)))
    case (maxTaskWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pMaxTaskWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxTaskTotalMemorySize)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (maxTaskOutputCount)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxMeshWorkGroupInvocations)
    let pMaxMeshWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 48 :: Ptr (FixedArray 3 Word32)))
    case (maxMeshWorkGroupSize) of
      (e0, e1, e2) -> do
        poke (pMaxMeshWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (maxMeshTotalMemorySize)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (maxMeshOutputVertices)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (maxMeshOutputPrimitives)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (maxMeshMultiviewViewCount)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (meshOutputPerVertexGranularity)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (meshOutputPerPrimitiveGranularity)
    f
  cStructSize = 88
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    let pMaxTaskWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pMaxTaskWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxTaskWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    let pMaxMeshWorkGroupSize' = lowerArrayPtr ((p `plusPtr` 48 :: Ptr (FixedArray 3 Word32)))
    case ((zero, zero, zero)) of
      (e0, e1, e2) -> do
        poke (pMaxMeshWorkGroupSize' :: Ptr Word32) (e0)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 4 :: Ptr Word32) (e1)
        poke (pMaxMeshWorkGroupSize' `plusPtr` 8 :: Ptr Word32) (e2)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 80 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceMeshShaderPropertiesNV where
  peekCStruct p = do
    maxDrawMeshTasksCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxTaskWorkGroupInvocations <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    let pmaxTaskWorkGroupSize = lowerArrayPtr @Word32 ((p `plusPtr` 24 :: Ptr (FixedArray 3 Word32)))
    maxTaskWorkGroupSize0 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 0 :: Ptr Word32))
    maxTaskWorkGroupSize1 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 4 :: Ptr Word32))
    maxTaskWorkGroupSize2 <- peek @Word32 ((pmaxTaskWorkGroupSize `advancePtrBytes` 8 :: Ptr Word32))
    maxTaskTotalMemorySize <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    maxTaskOutputCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxMeshWorkGroupInvocations <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    let pmaxMeshWorkGroupSize = lowerArrayPtr @Word32 ((p `plusPtr` 48 :: Ptr (FixedArray 3 Word32)))
    maxMeshWorkGroupSize0 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 0 :: Ptr Word32))
    maxMeshWorkGroupSize1 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 4 :: Ptr Word32))
    maxMeshWorkGroupSize2 <- peek @Word32 ((pmaxMeshWorkGroupSize `advancePtrBytes` 8 :: Ptr Word32))
    maxMeshTotalMemorySize <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxMeshOutputVertices <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    maxMeshOutputPrimitives <- peek @Word32 ((p `plusPtr` 68 :: Ptr Word32))
    maxMeshMultiviewViewCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    meshOutputPerVertexGranularity <- peek @Word32 ((p `plusPtr` 76 :: Ptr Word32))
    meshOutputPerPrimitiveGranularity <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    pure $ PhysicalDeviceMeshShaderPropertiesNV
             maxDrawMeshTasksCount maxTaskWorkGroupInvocations ((maxTaskWorkGroupSize0, maxTaskWorkGroupSize1, maxTaskWorkGroupSize2)) maxTaskTotalMemorySize maxTaskOutputCount maxMeshWorkGroupInvocations ((maxMeshWorkGroupSize0, maxMeshWorkGroupSize1, maxMeshWorkGroupSize2)) maxMeshTotalMemorySize maxMeshOutputVertices maxMeshOutputPrimitives maxMeshMultiviewViewCount meshOutputPerVertexGranularity meshOutputPerPrimitiveGranularity


instance Storable PhysicalDeviceMeshShaderPropertiesNV where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMeshShaderPropertiesNV where
  zero = PhysicalDeviceMeshShaderPropertiesNV
           zero
           zero
           (zero, zero, zero)
           zero
           zero
           zero
           (zero, zero, zero)
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDrawMeshTasksIndirectCommandNV"
data DrawMeshTasksIndirectCommandNV = DrawMeshTasksIndirectCommandNV
  { -- No documentation found for Nested "VkDrawMeshTasksIndirectCommandNV" "taskCount"
    taskCount :: Word32
  , -- No documentation found for Nested "VkDrawMeshTasksIndirectCommandNV" "firstTask"
    firstTask :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawMeshTasksIndirectCommandNV)
#endif
deriving instance Show DrawMeshTasksIndirectCommandNV

instance ToCStruct DrawMeshTasksIndirectCommandNV where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawMeshTasksIndirectCommandNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (taskCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (firstTask)
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawMeshTasksIndirectCommandNV where
  peekCStruct p = do
    taskCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    firstTask <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    pure $ DrawMeshTasksIndirectCommandNV
             taskCount firstTask


instance Storable DrawMeshTasksIndirectCommandNV where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawMeshTasksIndirectCommandNV where
  zero = DrawMeshTasksIndirectCommandNV
           zero
           zero


type NV_MESH_SHADER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_SPEC_VERSION"
pattern NV_MESH_SHADER_SPEC_VERSION :: forall a . Integral a => a
pattern NV_MESH_SHADER_SPEC_VERSION = 1


type NV_MESH_SHADER_EXTENSION_NAME = "VK_NV_mesh_shader"

-- No documentation found for TopLevel "VK_NV_MESH_SHADER_EXTENSION_NAME"
pattern NV_MESH_SHADER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_MESH_SHADER_EXTENSION_NAME = "VK_NV_mesh_shader"

