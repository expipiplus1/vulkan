{-# language CPP #-}
module Graphics.Vulkan.Core10.BufferView  ( createBufferView
                                          , withBufferView
                                          , destroyBufferView
                                          , BufferViewCreateInfo(..)
                                          ) where

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (BufferView)
import Graphics.Vulkan.Core10.Handles (BufferView(..))
import Graphics.Vulkan.Core10.Enums.BufferViewCreateFlags (BufferViewCreateFlags)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateBufferView))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyBufferView))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBufferView
  :: FunPtr (Ptr Device_T -> Ptr BufferViewCreateInfo -> Ptr AllocationCallbacks -> Ptr BufferView -> IO Result) -> Ptr Device_T -> Ptr BufferViewCreateInfo -> Ptr AllocationCallbacks -> Ptr BufferView -> IO Result

-- | vkCreateBufferView - Create a new buffer view object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     creates the buffer view.
--
-- -   @pCreateInfo@ is a pointer to a 'BufferViewCreateInfo' structure
--     containing parameters to be used to create the buffer.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pView@ is a pointer to a
--     'Graphics.Vulkan.Core10.Handles.BufferView' handle in which the
--     resulting buffer view object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'BufferViewCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pView@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.BufferView' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.BufferView', 'BufferViewCreateInfo',
-- 'Graphics.Vulkan.Core10.Handles.Device'
createBufferView :: Device -> BufferViewCreateInfo -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (BufferView)
createBufferView device createInfo allocator = evalContT $ do
  let vkCreateBufferView' = mkVkCreateBufferView (pVkCreateBufferView (deviceCmds (device :: Device)))
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPView <- ContT $ bracket (callocBytes @BufferView 8) free
  r <- lift $ vkCreateBufferView' (deviceHandle (device)) pCreateInfo pAllocator (pPView)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pView <- lift $ peek @BufferView pPView
  pure $ (pView)

-- | A safe wrapper for 'createBufferView' and 'destroyBufferView' using
-- 'bracket'
--
-- The allocated value must not be returned from the provided computation
withBufferView :: Device -> BufferViewCreateInfo -> Maybe AllocationCallbacks -> (BufferView -> IO r) -> IO r
withBufferView device bufferViewCreateInfo allocationCallbacks =
  bracket
    (createBufferView device bufferViewCreateInfo allocationCallbacks)
    (\o -> destroyBufferView device o allocationCallbacks)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBufferView
  :: FunPtr (Ptr Device_T -> BufferView -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> BufferView -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyBufferView - Destroy a buffer view object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     destroys the buffer view.
--
-- -   'Graphics.Vulkan.Core10.Handles.BufferView' is the buffer view to
--     destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to
--     'Graphics.Vulkan.Core10.Handles.BufferView' /must/ have completed
--     execution
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when 'Graphics.Vulkan.Core10.Handles.BufferView' was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when 'Graphics.Vulkan.Core10.Handles.BufferView' was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   If 'Graphics.Vulkan.Core10.Handles.BufferView' is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Graphics.Vulkan.Core10.Handles.BufferView' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.BufferView' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If 'Graphics.Vulkan.Core10.Handles.BufferView' is a valid handle, it
--     /must/ have been created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.BufferView' /must/ be
--     externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.BufferView',
-- 'Graphics.Vulkan.Core10.Handles.Device'
destroyBufferView :: Device -> BufferView -> ("allocator" ::: Maybe AllocationCallbacks) -> IO ()
destroyBufferView device bufferView allocator = evalContT $ do
  let vkDestroyBufferView' = mkVkDestroyBufferView (pVkDestroyBufferView (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyBufferView' (deviceHandle (device)) (bufferView) pAllocator
  pure $ ()


-- | VkBufferViewCreateInfo - Structure specifying parameters of a newly
-- created buffer view
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', @range@ /must/ be
--     greater than @0@
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', @range@ /must/ be
--     an integer multiple of the texel block size of
--     'Graphics.Vulkan.Core10.Enums.Format.Format'
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', @range@ divided by
--     the texel block size of
--     'Graphics.Vulkan.Core10.Enums.Format.Format', multiplied by the
--     number of texels per texel block for that format (as defined in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatibility Compatible Formats>
--     table), /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTexelBufferElements@
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', the sum of
--     @offset@ and @range@ /must/ be less than or equal to the size of
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ have been created
--     with a @usage@ value containing at least one of
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with @usage@
--     containing
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT',
--     'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be supported for
--     uniform texel buffers, as specified by the
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT'
--     flag in
--     'Graphics.Vulkan.Core10.DeviceInitialization.FormatProperties'::@bufferFeatures@
--     returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with @usage@
--     containing
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT',
--     'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be supported for
--     storage texel buffers, as specified by the
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT'
--     flag in
--     'Graphics.Vulkan.Core10.DeviceInitialization.FormatProperties'::@bufferFeatures@
--     returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' is non-sparse then it
--     /must/ be bound completely and contiguously to a single
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-texelBufferAlignment texelBufferAlignment>
--     feature is not enabled, @offset@ /must/ be a multiple of
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minTexelBufferOffsetAlignment@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-texelBufferAlignment texelBufferAlignment>
--     feature is enabled and if 'Graphics.Vulkan.Core10.Handles.Buffer'
--     was created with @usage@ containing
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT',
--     @offset@ /must/ be a multiple of the lesser of
--     'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT'::@storageTexelBufferOffsetAlignmentBytes@
--     or, if
--     'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT'::@storageTexelBufferOffsetSingleTexelAlignment@
--     is 'Graphics.Vulkan.Core10.BaseType.TRUE', the size of a texel of
--     the requested 'Graphics.Vulkan.Core10.Enums.Format.Format'. If the
--     size of a texel is a multiple of three bytes, then the size of a
--     single component of 'Graphics.Vulkan.Core10.Enums.Format.Format' is
--     used instead
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-texelBufferAlignment texelBufferAlignment>
--     feature is enabled and if 'Graphics.Vulkan.Core10.Handles.Buffer'
--     was created with @usage@ containing
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT',
--     @offset@ /must/ be a multiple of the lesser of
--     'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT'::@uniformTexelBufferOffsetAlignmentBytes@
--     or, if
--     'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT'::@uniformTexelBufferOffsetSingleTexelAlignment@
--     is 'Graphics.Vulkan.Core10.BaseType.TRUE', the size of a texel of
--     the requested 'Graphics.Vulkan.Core10.Enums.Format.Format'. If the
--     size of a texel is a multiple of three bytes, then the size of a
--     single component of 'Graphics.Vulkan.Core10.Enums.Format.Format' is
--     used instead
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   'Graphics.Vulkan.Core10.BaseType.Flags' /must/ be @0@
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   'Graphics.Vulkan.Core10.Enums.Format.Format' /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Enums.BufferViewCreateFlags.BufferViewCreateFlags',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Enums.Format.Format',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createBufferView'
data BufferViewCreateInfo = BufferViewCreateInfo
  { -- | 'Graphics.Vulkan.Core10.BaseType.Flags' is reserved for future use.
    flags :: BufferViewCreateFlags
  , -- | 'Graphics.Vulkan.Core10.Handles.Buffer' is a
    -- 'Graphics.Vulkan.Core10.Handles.Buffer' on which the view will be
    -- created.
    buffer :: Buffer
  , -- | 'Graphics.Vulkan.Core10.Enums.Format.Format' is a
    -- 'Graphics.Vulkan.Core10.Enums.Format.Format' describing the format of
    -- the data elements in the buffer.
    format :: Format
  , -- | @offset@ is an offset in bytes from the base address of the buffer.
    -- Accesses to the buffer view from shaders use addressing that is relative
    -- to this starting offset.
    offset :: DeviceSize
  , -- | @range@ is a size in bytes of the buffer view. If @range@ is equal to
    -- 'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE', the range from
    -- @offset@ to the end of the buffer is used. If
    -- 'Graphics.Vulkan.Core10.APIConstants.WHOLE_SIZE' is used and the
    -- remaining size of the buffer is not a multiple of the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#texel-block-size texel block size>
    -- of 'Graphics.Vulkan.Core10.Enums.Format.Format', the nearest smaller
    -- multiple is used.
    range :: DeviceSize
  }
  deriving (Typeable)
deriving instance Show BufferViewCreateInfo

instance ToCStruct BufferViewCreateInfo where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferViewCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferViewCreateFlags)) (flags)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (buffer)
    poke ((p `plusPtr` 32 :: Ptr Format)) (format)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (offset)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (range)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BufferViewCreateInfo where
  peekCStruct p = do
    flags <- peek @BufferViewCreateFlags ((p `plusPtr` 16 :: Ptr BufferViewCreateFlags))
    buffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    format <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    offset <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    range <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    pure $ BufferViewCreateInfo
             flags buffer format offset range

instance Storable BufferViewCreateInfo where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferViewCreateInfo where
  zero = BufferViewCreateInfo
           zero
           zero
           zero
           zero
           zero

