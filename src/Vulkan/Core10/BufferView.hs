{-# language CPP #-}
-- No documentation found for Chapter "BufferView"
module Vulkan.Core10.BufferView  ( createBufferView
                                 , withBufferView
                                 , destroyBufferView
                                 , BufferViewCreateInfo(..)
                                 , BufferView(..)
                                 , BufferViewCreateFlags(..)
                                 ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
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
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (BufferView)
import Vulkan.Core10.Handles (BufferView(..))
import Vulkan.Core10.Enums.BufferViewCreateFlags (BufferViewCreateFlags)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateBufferView))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyBufferView))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalObjectCreateInfoEXT)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (BufferView(..))
import Vulkan.Core10.Enums.BufferViewCreateFlags (BufferViewCreateFlags(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBufferView
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct BufferViewCreateInfo) -> Ptr AllocationCallbacks -> Ptr BufferView -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct BufferViewCreateInfo) -> Ptr AllocationCallbacks -> Ptr BufferView -> IO Result

-- | vkCreateBufferView - Create a new buffer view object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateBufferView-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateBufferView-pCreateInfo-parameter# @pCreateInfo@ /must/
--     be a valid pointer to a valid 'BufferViewCreateInfo' structure
--
-- -   #VUID-vkCreateBufferView-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateBufferView-pView-parameter# @pView@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.BufferView' handle
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.BufferView', 'BufferViewCreateInfo',
-- 'Vulkan.Core10.Handles.Device'
createBufferView :: forall a io
                  . (Extendss BufferViewCreateInfo a, PokeChain a, MonadIO io)
                 => -- | @device@ is the logical device that creates the buffer view.
                    Device
                 -> -- | @pCreateInfo@ is a pointer to a 'BufferViewCreateInfo' structure
                    -- containing parameters to be used to create the buffer view.
                    (BufferViewCreateInfo a)
                 -> -- | @pAllocator@ controls host memory allocation as described in the
                    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                    -- chapter.
                    ("allocator" ::: Maybe AllocationCallbacks)
                 -> io (BufferView)
createBufferView device createInfo allocator = liftIO . evalContT $ do
  let vkCreateBufferViewPtr = pVkCreateBufferView (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateBufferViewPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateBufferView is null" Nothing Nothing
  let vkCreateBufferView' = mkVkCreateBufferView vkCreateBufferViewPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPView <- ContT $ bracket (callocBytes @BufferView 8) free
  r <- lift $ traceAroundEvent "vkCreateBufferView" (vkCreateBufferView'
                                                       (deviceHandle (device))
                                                       (forgetExtensions pCreateInfo)
                                                       pAllocator
                                                       (pPView))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pView <- lift $ peek @BufferView pPView
  pure $ (pView)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createBufferView' and 'destroyBufferView'
--
-- To ensure that 'destroyBufferView' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withBufferView :: forall a io r . (Extendss BufferViewCreateInfo a, PokeChain a, MonadIO io) => Device -> BufferViewCreateInfo a -> Maybe AllocationCallbacks -> (io BufferView -> (BufferView -> io ()) -> r) -> r
withBufferView device pCreateInfo pAllocator b =
  b (createBufferView device pCreateInfo pAllocator)
    (\(o0) -> destroyBufferView device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBufferView
  :: FunPtr (Ptr Device_T -> BufferView -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> BufferView -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyBufferView - Destroy a buffer view object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyBufferView-bufferView-00936# All submitted commands
--     that refer to @bufferView@ /must/ have completed execution
--
-- -   #VUID-vkDestroyBufferView-bufferView-00937# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @bufferView@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyBufferView-bufferView-00938# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @bufferView@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyBufferView-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyBufferView-bufferView-parameter# If @bufferView@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @bufferView@ /must/ be
--     a valid 'Vulkan.Core10.Handles.BufferView' handle
--
-- -   #VUID-vkDestroyBufferView-pAllocator-parameter# If @pAllocator@ is
--     not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyBufferView-bufferView-parent# If @bufferView@ is a
--     valid handle, it /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Host Synchronization
--
-- -   Host access to @bufferView@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.BufferView', 'Vulkan.Core10.Handles.Device'
destroyBufferView :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device that destroys the buffer view.
                     Device
                  -> -- | @bufferView@ is the buffer view to destroy.
                     BufferView
                  -> -- | @pAllocator@ controls host memory allocation as described in the
                     -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                     -- chapter.
                     ("allocator" ::: Maybe AllocationCallbacks)
                  -> io ()
destroyBufferView device bufferView allocator = liftIO . evalContT $ do
  let vkDestroyBufferViewPtr = pVkDestroyBufferView (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyBufferViewPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyBufferView is null" Nothing Nothing
  let vkDestroyBufferView' = mkVkDestroyBufferView vkDestroyBufferViewPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyBufferView" (vkDestroyBufferView'
                                                   (deviceHandle (device))
                                                   (bufferView)
                                                   pAllocator)
  pure $ ()


-- | VkBufferViewCreateInfo - Structure specifying parameters of a newly
-- created buffer view
--
-- == Valid Usage
--
-- -   #VUID-VkBufferViewCreateInfo-offset-00925# @offset@ /must/ be less
--     than the size of @buffer@
--
-- -   #VUID-VkBufferViewCreateInfo-range-00928# If @range@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @range@ /must/ be greater
--     than @0@
--
-- -   #VUID-VkBufferViewCreateInfo-range-00929# If @range@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @range@ /must/ be an
--     integer multiple of the texel block size of @format@
--
-- -   #VUID-VkBufferViewCreateInfo-range-00930# If @range@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', the number of texel buffer
--     elements given by (⌊@range@ \/ (texel block size)⌋ × (texels per
--     block)) where texel block size and texels per block are as defined
--     in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility Compatible Formats>
--     table for @format@, /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTexelBufferElements@
--
-- -   #VUID-VkBufferViewCreateInfo-offset-00931# If @range@ is not equal
--     to 'Vulkan.Core10.APIConstants.WHOLE_SIZE', the sum of @offset@ and
--     @range@ /must/ be less than or equal to the size of @buffer@
--
-- -   #VUID-VkBufferViewCreateInfo-range-04059# If @range@ is equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', the number of texel buffer
--     elements given by (⌊(size - @offset@) \/ (texel block size)⌋ ×
--     (texels per block)) where size is the size of @buffer@, and texel
--     block size and texels per block are as defined in the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#formats-compatibility Compatible Formats>
--     table for @format@, /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTexelBufferElements@
--
-- -   #VUID-VkBufferViewCreateInfo-buffer-00932# @buffer@ /must/ have been
--     created with a @usage@ value containing at least one of
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
--     or
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
--
-- -   #VUID-VkBufferViewCreateInfo-buffer-00933# If @buffer@ was created
--     with @usage@ containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT',
--     then
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     of @format@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT'
--
-- -   #VUID-VkBufferViewCreateInfo-buffer-00934# If @buffer@ was created
--     with @usage@ containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT',
--     then
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     of @format@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT'
--
-- -   #VUID-VkBufferViewCreateInfo-buffer-00935# If @buffer@ is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-VkBufferViewCreateInfo-offset-02749# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-texelBufferAlignment texelBufferAlignment>
--     feature is not enabled, @offset@ /must/ be a multiple of
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@minTexelBufferOffsetAlignment@
--
-- -   #VUID-VkBufferViewCreateInfo-buffer-02750# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-texelBufferAlignment texelBufferAlignment>
--     feature is enabled and if @buffer@ was created with @usage@
--     containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT',
--     @offset@ /must/ be a multiple of the lesser of
--     'Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentProperties'::@storageTexelBufferOffsetAlignmentBytes@
--     or, if
--     'Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentProperties'::@storageTexelBufferOffsetSingleTexelAlignment@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', the size of a texel of the
--     requested @format@. If the size of a texel is a multiple of three
--     bytes, then the size of a single component of @format@ is used
--     instead
--
-- -   #VUID-VkBufferViewCreateInfo-buffer-02751# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-texelBufferAlignment texelBufferAlignment>
--     feature is enabled and if @buffer@ was created with @usage@
--     containing
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT',
--     @offset@ /must/ be a multiple of the lesser of
--     'Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentProperties'::@uniformTexelBufferOffsetAlignmentBytes@
--     or, if
--     'Vulkan.Core13.Promoted_From_VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentProperties'::@uniformTexelBufferOffsetSingleTexelAlignment@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', the size of a texel of the
--     requested @format@. If the size of a texel is a multiple of three
--     bytes, then the size of a single component of @format@ is used
--     instead
--
-- -   #VUID-VkBufferViewCreateInfo-pNext-06782# If the @pNext@ chain
--     includes a
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--     structure, its @exportObjectType@ member /must/ be
--     'Vulkan.Extensions.VK_EXT_metal_objects.EXPORT_METAL_OBJECT_TYPE_METAL_TEXTURE_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferViewCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO'
--
-- -   #VUID-VkBufferViewCreateInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--
-- -   #VUID-VkBufferViewCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique, with the exception of
--     structures of type
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--
-- -   #VUID-VkBufferViewCreateInfo-flags-zerobitmask# @flags@ /must/ be
--     @0@
--
-- -   #VUID-VkBufferViewCreateInfo-buffer-parameter# @buffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkBufferViewCreateInfo-format-parameter# @format@ /must/ be a
--     valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.Enums.BufferViewCreateFlags.BufferViewCreateFlags',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createBufferView'
data BufferViewCreateInfo (es :: [Type]) = BufferViewCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: BufferViewCreateFlags
  , -- | @buffer@ is a 'Vulkan.Core10.Handles.Buffer' on which the view will be
    -- created.
    buffer :: Buffer
  , -- | @format@ is a 'Vulkan.Core10.Enums.Format.Format' describing the format
    -- of the data elements in the buffer.
    format :: Format
  , -- | @offset@ is an offset in bytes from the base address of the buffer.
    -- Accesses to the buffer view from shaders use addressing that is relative
    -- to this starting offset.
    offset :: DeviceSize
  , -- | @range@ is a size in bytes of the buffer view. If @range@ is equal to
    -- 'Vulkan.Core10.APIConstants.WHOLE_SIZE', the range from @offset@ to the
    -- end of the buffer is used. If 'Vulkan.Core10.APIConstants.WHOLE_SIZE' is
    -- used and the remaining size of the buffer is not a multiple of the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#texel-block-size texel block size>
    -- of @format@, the nearest smaller multiple is used.
    range :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferViewCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BufferViewCreateInfo es)

instance Extensible BufferViewCreateInfo where
  extensibleTypeName = "BufferViewCreateInfo"
  setNext BufferViewCreateInfo{..} next' = BufferViewCreateInfo{next = next', ..}
  getNext BufferViewCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BufferViewCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ExportMetalObjectCreateInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss BufferViewCreateInfo es
         , PokeChain es ) => ToCStruct (BufferViewCreateInfo es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferViewCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr BufferViewCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Buffer)) (buffer)
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (format)
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (offset)
    lift $ poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (range)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr Buffer)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr DeviceSize)) (zero)
    lift $ f

instance ( Extendss BufferViewCreateInfo es
         , PeekChain es ) => FromCStruct (BufferViewCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @BufferViewCreateFlags ((p `plusPtr` 16 :: Ptr BufferViewCreateFlags))
    buffer <- peek @Buffer ((p `plusPtr` 24 :: Ptr Buffer))
    format <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    offset <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    range <- peek @DeviceSize ((p `plusPtr` 48 :: Ptr DeviceSize))
    pure $ BufferViewCreateInfo
             next flags buffer format offset range

instance es ~ '[] => Zero (BufferViewCreateInfo es) where
  zero = BufferViewCreateInfo
           ()
           zero
           zero
           zero
           zero
           zero

