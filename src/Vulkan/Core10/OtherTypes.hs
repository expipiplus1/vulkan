{-# language CPP #-}
-- No documentation found for Chapter "OtherTypes"
module Vulkan.Core10.OtherTypes  ( PipelineCacheHeaderVersionOne(..)
                                 , DrawIndirectCommand(..)
                                 , DrawIndexedIndirectCommand(..)
                                 , DispatchIndirectCommand(..)
                                 , BaseOutStructure(..)
                                 , BaseInStructure(..)
                                 , PipelineCacheHeaderVersion(..)
                                 , ObjectType(..)
                                 , VendorId(..)
                                 ) where

import Vulkan.CStruct.Utils (FixedArray)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.Core10.Enums.PipelineCacheHeaderVersion (PipelineCacheHeaderVersion)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.CStruct.Extends (BaseInStructure(..))
import Vulkan.CStruct.Extends (BaseOutStructure(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core10.Enums.PipelineCacheHeaderVersion (PipelineCacheHeaderVersion(..))
import Vulkan.Core10.Enums.VendorId (VendorId(..))
-- | VkPipelineCacheHeaderVersionOne - Structure describing the layout of the
-- pipeline cache header
--
-- = Description
--
-- Unlike most structures declared by the Vulkan API, all fields of this
-- structure are written with the least significant byte first, regardless
-- of host byte-order.
--
-- The C language specification does not define the packing of structure
-- members. This layout assumes tight structure member packing, with
-- members laid out in the order listed in the structure, and the intended
-- size of the structure is 32 bytes. If a compiler produces code that
-- diverges from that pattern, applications /must/ employ another method to
-- set values at the correct offsets.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
data PipelineCacheHeaderVersionOne = PipelineCacheHeaderVersionOne
  { -- | @headerSize@ is the length in bytes of the pipeline cache header.
    --
    -- #VUID-VkPipelineCacheHeaderVersionOne-headerSize-04967# @headerSize@
    -- /must/ be 32
    --
    -- #VUID-VkPipelineCacheHeaderVersionOne-headerSize-08990# @headerSize@
    -- /must/ not exceed the size of the pipeline cache
    headerSize :: Word32
  , -- | @headerVersion@ is a
    -- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
    -- value specifying the version of the header. A consumer of the pipeline
    -- cache /should/ use the cache version to interpret the remainder of the
    -- cache header. @headerVersion@ /must/ be written as exactly 4 bytes.
    --
    -- #VUID-VkPipelineCacheHeaderVersionOne-headerVersion-04968#
    -- @headerVersion@ /must/ be
    -- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PIPELINE_CACHE_HEADER_VERSION_ONE'
    --
    -- #VUID-VkPipelineCacheHeaderVersionOne-headerVersion-parameter#
    -- @headerVersion@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion'
    -- value
    headerVersion :: PipelineCacheHeaderVersion
  , -- | @vendorID@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@vendorID@
    -- of the implementation.
    vendorID :: Word32
  , -- | @deviceID@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@deviceID@
    -- of the implementation.
    deviceID :: Word32
  , -- | @pipelineCacheUUID@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@pipelineCacheUUID@
    -- of the implementation.
    pipelineCacheUUID :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCacheHeaderVersionOne)
#endif
deriving instance Show PipelineCacheHeaderVersionOne

instance ToCStruct PipelineCacheHeaderVersionOne where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCacheHeaderVersionOne{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (headerSize)
    poke ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion)) (headerVersion)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (vendorID)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (deviceID)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (pipelineCacheUUID)
    f
  cStructSize = 32
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    f

instance FromCStruct PipelineCacheHeaderVersionOne where
  peekCStruct p = do
    headerSize <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    headerVersion <- peek @PipelineCacheHeaderVersion ((p `plusPtr` 4 :: Ptr PipelineCacheHeaderVersion))
    vendorID <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    deviceID <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pipelineCacheUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 16 :: Ptr (FixedArray UUID_SIZE Word8)))
    pure $ PipelineCacheHeaderVersionOne
             headerSize headerVersion vendorID deviceID pipelineCacheUUID

instance Storable PipelineCacheHeaderVersionOne where
  sizeOf ~_ = 32
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCacheHeaderVersionOne where
  zero = PipelineCacheHeaderVersionOne
           zero
           zero
           zero
           zero
           mempty


-- | VkDrawIndirectCommand - Structure specifying an indirect drawing command
--
-- = Description
--
-- The members of 'DrawIndirectCommand' have the same meaning as the
-- similarly named parameters of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDraw'.
--
-- == Valid Usage
--
-- -   #VUID-VkDrawIndirectCommand-pNext-09461# If the bound graphics
--     pipeline state was created with
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PipelineVertexInputDivisorStateCreateInfo'
--     in the @pNext@ chain of
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@pVertexInputState@,
--     any member of
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PipelineVertexInputDivisorStateCreateInfo'::@pVertexBindingDivisors@
--     has a value other than @1@ in @divisor@, and
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PhysicalDeviceVertexAttributeDivisorProperties'::@supportsNonZeroFirstInstance@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then @firstInstance@
--     /must/ be @0@
--
-- -   #VUID-VkDrawIndirectCommand-None-09462# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-objects shader objects>
--     are used for drawing or the bound graphics pipeline state was
--     created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, any member of the
--     @pVertexBindingDescriptions@ parameter to the
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     call that sets this dynamic state has a value other than @1@ in
--     @divisor@, and
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PhysicalDeviceVertexAttributeDivisorProperties'::@supportsNonZeroFirstInstance@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then @firstInstance@
--     /must/ be @0@
--
-- -   #VUID-VkDrawIndirectCommand-firstInstance-00501# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect'
data DrawIndirectCommand = DrawIndirectCommand
  { -- | @vertexCount@ is the number of vertices to draw.
    vertexCount :: Word32
  , -- | @instanceCount@ is the number of instances to draw.
    instanceCount :: Word32
  , -- | @firstVertex@ is the index of the first vertex to draw.
    firstVertex :: Word32
  , -- | @firstInstance@ is the instance ID of the first instance to draw.
    firstInstance :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawIndirectCommand)
#endif
deriving instance Show DrawIndirectCommand

instance ToCStruct DrawIndirectCommand where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (vertexCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (instanceCount)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstVertex)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (firstInstance)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawIndirectCommand where
  peekCStruct p = do
    vertexCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    instanceCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstVertex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    firstInstance <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ DrawIndirectCommand
             vertexCount instanceCount firstVertex firstInstance

instance Storable DrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawIndirectCommand where
  zero = DrawIndirectCommand
           zero
           zero
           zero
           zero


-- | VkDrawIndexedIndirectCommand - Structure specifying an indexed indirect
-- drawing command
--
-- = Description
--
-- The members of 'DrawIndexedIndirectCommand' have the same meaning as the
-- similarly named parameters of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexed'.
--
-- == Valid Usage
--
-- -   #VUID-VkDrawIndexedIndirectCommand-pNext-09461# If the bound
--     graphics pipeline state was created with
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PipelineVertexInputDivisorStateCreateInfo'
--     in the @pNext@ chain of
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'::@pVertexInputState@,
--     any member of
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PipelineVertexInputDivisorStateCreateInfo'::@pVertexBindingDivisors@
--     has a value other than @1@ in @divisor@, and
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PhysicalDeviceVertexAttributeDivisorProperties'::@supportsNonZeroFirstInstance@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then @firstInstance@
--     /must/ be @0@
--
-- -   #VUID-VkDrawIndexedIndirectCommand-None-09462# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-objects shader objects>
--     are used for drawing or the bound graphics pipeline state was
--     created with the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--     dynamic state enabled, any member of the
--     @pVertexBindingDescriptions@ parameter to the
--     'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.cmdSetVertexInputEXT'
--     call that sets this dynamic state has a value other than @1@ in
--     @divisor@, and
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PhysicalDeviceVertexAttributeDivisorProperties'::@supportsNonZeroFirstInstance@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then @firstInstance@
--     /must/ be @0@
--
-- -   #VUID-VkDrawIndexedIndirectCommand-robustBufferAccess2-08798# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, (@indexSize@ × (@firstIndex@ +
--     @indexCount@)) /must/ be less than or equal to the size of the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#index-buffer-range bound index buffer range>,
--     with @indexSize@ being based on the type specified by @indexType@,
--     and the other parameters sourced from this command
--
-- -   #VUID-VkDrawIndexedIndirectCommand-firstInstance-00554# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'
data DrawIndexedIndirectCommand = DrawIndexedIndirectCommand
  { -- | @indexCount@ is the number of vertices to draw.
    indexCount :: Word32
  , -- | @instanceCount@ is the number of instances to draw.
    instanceCount :: Word32
  , -- | @firstIndex@ is the base index within the index buffer.
    firstIndex :: Word32
  , -- | @vertexOffset@ is the value added to the vertex index before indexing
    -- into the vertex buffer.
    vertexOffset :: Int32
  , -- | @firstInstance@ is the instance ID of the first instance to draw.
    firstInstance :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DrawIndexedIndirectCommand)
#endif
deriving instance Show DrawIndexedIndirectCommand

instance ToCStruct DrawIndexedIndirectCommand where
  withCStruct x f = allocaBytes 20 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DrawIndexedIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (indexCount)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (instanceCount)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (firstIndex)
    poke ((p `plusPtr` 12 :: Ptr Int32)) (vertexOffset)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (firstInstance)
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Int32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct DrawIndexedIndirectCommand where
  peekCStruct p = do
    indexCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    instanceCount <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    firstIndex <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    vertexOffset <- peek @Int32 ((p `plusPtr` 12 :: Ptr Int32))
    firstInstance <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ DrawIndexedIndirectCommand
             indexCount instanceCount firstIndex vertexOffset firstInstance

instance Storable DrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DrawIndexedIndirectCommand where
  zero = DrawIndexedIndirectCommand
           zero
           zero
           zero
           zero
           zero


-- | VkDispatchIndirectCommand - Structure specifying an indirect dispatching
-- command
--
-- = Description
--
-- The members of 'DispatchIndirectCommand' have the same meaning as the
-- corresponding parameters of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatch'.
--
-- == Valid Usage
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect'
data DispatchIndirectCommand = DispatchIndirectCommand
  { -- | @x@ is the number of local workgroups to dispatch in the X dimension.
    --
    -- #VUID-VkDispatchIndirectCommand-x-00417# @x@ /must/ be less than or
    -- equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
    x :: Word32
  , -- | @y@ is the number of local workgroups to dispatch in the Y dimension.
    --
    -- #VUID-VkDispatchIndirectCommand-y-00418# @y@ /must/ be less than or
    -- equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
    y :: Word32
  , -- | @z@ is the number of local workgroups to dispatch in the Z dimension.
    --
    -- #VUID-VkDispatchIndirectCommand-z-00419# @z@ /must/ be less than or
    -- equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
    z :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DispatchIndirectCommand)
#endif
deriving instance Show DispatchIndirectCommand

instance ToCStruct DispatchIndirectCommand where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DispatchIndirectCommand{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (x)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (y)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (z)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct DispatchIndirectCommand where
  peekCStruct p = do
    x <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    y <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    z <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ DispatchIndirectCommand
             x y z

instance Storable DispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DispatchIndirectCommand where
  zero = DispatchIndirectCommand
           zero
           zero
           zero

