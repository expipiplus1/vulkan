{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_map_memory2Roadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_map_memory2Roadmap  ( mapMemory2
                                                              , unmapMemory2
                                                              , MemoryMapInfo(..)
                                                              , MemoryUnmapInfo(..)
                                                              , StructureType(..)
                                                              , MemoryUnmapFlagBits(..)
                                                              , MemoryUnmapFlags
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
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkMapMemory2))
import Vulkan.Dynamic (DeviceCmds(pVkUnmapMemory2))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.MemoryMapFlagBits (MemoryMapFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_map_memory_placed (MemoryMapPlacedInfoEXT)
import Vulkan.Core14.Enums.MemoryUnmapFlagBits (MemoryUnmapFlags)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_MAP_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_UNMAP_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core14.Enums.MemoryUnmapFlagBits (MemoryUnmapFlagBits(..))
import Vulkan.Core14.Enums.MemoryUnmapFlagBits (MemoryUnmapFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMapMemory2
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct MemoryMapInfo) -> Ptr (Ptr ()) -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct MemoryMapInfo) -> Ptr (Ptr ()) -> IO Result

-- | vkMapMemory2 - Map a memory object into application address space
--
-- = Description
--
-- This function behaves identically to 'Vulkan.Core10.Memory.mapMemory'
-- except that it gets its parameters via an extensible structure pointer
-- rather than directly as function arguments.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.Device', 'MemoryMapInfo'
mapMemory2 :: forall a io
            . (Extendss MemoryMapInfo a, PokeChain a, MonadIO io)
           => -- | @device@ is the logical device that owns the memory.
              --
              -- #VUID-vkMapMemory2-device-parameter# @device@ /must/ be a valid
              -- 'Vulkan.Core10.Handles.Device' handle
              Device
           -> -- | @pMemoryMapInfo@ is a pointer to a 'MemoryMapInfo' structure describing
              -- parameters of the map.
              --
              -- #VUID-vkMapMemory2-pMemoryMapInfo-parameter# @pMemoryMapInfo@ /must/ be
              -- a valid pointer to a valid 'MemoryMapInfo' structure
              (MemoryMapInfo a)
           -> io (("data" ::: Ptr ()))
mapMemory2 device memoryMapInfo = liftIO . evalContT $ do
  let vkMapMemory2Ptr = pVkMapMemory2 (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkMapMemory2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkMapMemory2 is null" Nothing Nothing
  let vkMapMemory2' = mkVkMapMemory2 vkMapMemory2Ptr
  pMemoryMapInfo <- ContT $ withCStruct (memoryMapInfo)
  pPpData <- ContT $ bracket (callocBytes @(Ptr ()) 8) free
  r <- lift $ traceAroundEvent "vkMapMemory2" (vkMapMemory2'
                                                 (deviceHandle (device))
                                                 (forgetExtensions pMemoryMapInfo)
                                                 (pPpData))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  ppData <- lift $ peek @(Ptr ()) pPpData
  pure $ (ppData)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnmapMemory2
  :: FunPtr (Ptr Device_T -> Ptr MemoryUnmapInfo -> IO Result) -> Ptr Device_T -> Ptr MemoryUnmapInfo -> IO Result

-- | vkUnmapMemory2 - Unmap a previously mapped memory object
--
-- = Description
--
-- This function behaves identically to 'Vulkan.Core10.Memory.unmapMemory'
-- except that it gets its parameters via an extensible structure pointer
-- rather than directly as function arguments.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.Device', 'MemoryUnmapInfo'
unmapMemory2 :: forall io
              . (MonadIO io)
             => -- | @device@ is the logical device that owns the memory.
                --
                -- #VUID-vkUnmapMemory2-device-parameter# @device@ /must/ be a valid
                -- 'Vulkan.Core10.Handles.Device' handle
                Device
             -> -- | @pMemoryUnmapInfo@ is a pointer to a 'MemoryUnmapInfo' structure
                -- describing parameters of the unmap.
                --
                -- #VUID-vkUnmapMemory2-pMemoryUnmapInfo-parameter# @pMemoryUnmapInfo@
                -- /must/ be a valid pointer to a valid 'MemoryUnmapInfo' structure
                MemoryUnmapInfo
             -> io ()
unmapMemory2 device memoryUnmapInfo = liftIO . evalContT $ do
  let vkUnmapMemory2Ptr = pVkUnmapMemory2 (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkUnmapMemory2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUnmapMemory2 is null" Nothing Nothing
  let vkUnmapMemory2' = mkVkUnmapMemory2 vkUnmapMemory2Ptr
  pMemoryUnmapInfo <- ContT $ withCStruct (memoryUnmapInfo)
  r <- lift $ traceAroundEvent "vkUnmapMemory2" (vkUnmapMemory2'
                                                   (deviceHandle (device))
                                                   pMemoryUnmapInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkMemoryMapInfo - Structure containing parameters of a memory map
-- operation
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryMapInfo-memory-07958# @memory@ /must/ not be currently
--     host mapped
--
-- -   #VUID-VkMemoryMapInfo-offset-07959# @offset@ /must/ be less than the
--     size of @memory@
--
-- -   #VUID-VkMemoryMapInfo-size-07960# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be greater
--     than @0@
--
-- -   #VUID-VkMemoryMapInfo-size-07961# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be less than
--     or equal to the size of the @memory@ minus @offset@
--
-- -   #VUID-VkMemoryMapInfo-memory-07962# @memory@ /must/ have been
--     created with a memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--
-- -   #VUID-VkMemoryMapInfo-memory-07963# @memory@ /must/ not have been
--     allocated with multiple instances
--
-- -   #VUID-VkMemoryMapInfo-flags-09569# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@, the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryMapPlaced memoryMapPlaced>
--     feature /must/ be enabled
--
-- -   #VUID-VkMemoryMapInfo-flags-09570# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@, the @pNext@ chain /must/ include a
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.MemoryMapPlacedInfoEXT'
--     structure and
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.MemoryMapPlacedInfoEXT'::@pPlacedAddress@
--     /must/ not be @NULL@
--
-- -   #VUID-VkMemoryMapInfo-flags-09571# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryMapRangePlaced memoryMapRangePlaced>
--     feature is not enabled, @offset@ /must/ be zero
--
-- -   #VUID-VkMemoryMapInfo-flags-09572# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryMapRangePlaced memoryMapRangePlaced>
--     feature is not enabled, @size@ /must/ be
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE' or
--     'Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@
--
-- -   #VUID-VkMemoryMapInfo-flags-09573# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryMapRangePlaced memoryMapRangePlaced>
--     feature is enabled, @offset@ /must/ be aligned to an integer
--     multiple of
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.PhysicalDeviceMapMemoryPlacedPropertiesEXT'::@minPlacedMemoryMapAlignment@
--
-- -   #VUID-VkMemoryMapInfo-flags-09574# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and @size@ is not
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be aligned to
--     an integer multiple of
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.PhysicalDeviceMapMemoryPlacedPropertiesEXT'::@minPlacedMemoryMapAlignment@
--
-- -   #VUID-VkMemoryMapInfo-flags-09651# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and @size@ is
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE',
--     'Vulkan.Core10.Memory.MemoryAllocateInfo'::@allocationSize@ /must/
--     be aligned to an integer multiple of
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.PhysicalDeviceMapMemoryPlacedPropertiesEXT'::@minPlacedMemoryMapAlignment@
--
-- -   #VUID-VkMemoryMapInfo-flags-09575# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@, the memory object /must/ not have been imported from
--     a handle type of
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryMapInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_MAP_INFO'
--
-- -   #VUID-VkMemoryMapInfo-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.MemoryMapPlacedInfoEXT'
--
-- -   #VUID-VkMemoryMapInfo-sType-unique# The @sType@ value of each
--     structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkMemoryMapInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MemoryMapFlagBits' values
--
-- -   #VUID-VkMemoryMapInfo-memory-parameter# @memory@ /must/ be a valid
--     'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- == Host Synchronization
--
-- -   Host access to @memory@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.MemoryMapFlagBits.MemoryMapFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'mapMemory2',
-- 'mapMemory2'
data MemoryMapInfo (es :: [Type]) = MemoryMapInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.MemoryMapFlagBits.MemoryMapFlagBits' specifying
    -- additional parameters of the memory map operation.
    flags :: MemoryMapFlags
  , -- | @memory@ is the 'Vulkan.Core10.Handles.DeviceMemory' object to be
    -- mapped.
    memory :: DeviceMemory
  , -- | @offset@ is a zero-based byte offset from the beginning of the memory
    -- object.
    offset :: DeviceSize
  , -- | @size@ is the size of the memory range to map, or
    -- 'Vulkan.Core10.APIConstants.WHOLE_SIZE' to map from @offset@ to the end
    -- of the allocation.
    size :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryMapInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (MemoryMapInfo es)

instance Extensible MemoryMapInfo where
  extensibleTypeName = "MemoryMapInfo"
  setNext MemoryMapInfo{..} next' = MemoryMapInfo{next = next', ..}
  getNext MemoryMapInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends MemoryMapInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MemoryMapPlacedInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss MemoryMapInfo es
         , PokeChain es ) => ToCStruct (MemoryMapInfo es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryMapInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_MAP_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr MemoryMapFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (offset)
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (size)
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_MAP_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    lift $ f

instance ( Extendss MemoryMapInfo es
         , PeekChain es ) => FromCStruct (MemoryMapInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @MemoryMapFlags ((p `plusPtr` 16 :: Ptr MemoryMapFlags))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    offset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    pure $ MemoryMapInfo
             next flags memory offset size

instance es ~ '[] => Zero (MemoryMapInfo es) where
  zero = MemoryMapInfo
           ()
           zero
           zero
           zero
           zero


-- | VkMemoryUnmapInfo - Structure containing parameters of a memory unmap
-- operation
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryUnmapInfo-memory-07964# @memory@ /must/ be currently
--     host mapped
--
-- -   #VUID-VkMemoryUnmapInfo-flags-09579# If
--     'Vulkan.Core14.Enums.MemoryUnmapFlagBits.MEMORY_UNMAP_RESERVE_BIT_EXT'
--     is set in @flags@, the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-memoryUnmapReserve memoryUnmapReserve>
--     /must/ be enabled
--
-- -   #VUID-VkMemoryUnmapInfo-flags-09580# If
--     'Vulkan.Core14.Enums.MemoryUnmapFlagBits.MEMORY_UNMAP_RESERVE_BIT_EXT'
--     is set in @flags@, the memory object /must/ not have been imported
--     from a handle type of
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryUnmapInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_UNMAP_INFO'
--
-- -   #VUID-VkMemoryUnmapInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkMemoryUnmapInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core14.Enums.MemoryUnmapFlagBits.MemoryUnmapFlagBits' values
--
-- -   #VUID-VkMemoryUnmapInfo-memory-parameter# @memory@ /must/ be a valid
--     'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- == Host Synchronization
--
-- -   Host access to @memory@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_4 VK_VERSION_1_4>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core14.Enums.MemoryUnmapFlagBits.MemoryUnmapFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'unmapMemory2',
-- 'unmapMemory2'
data MemoryUnmapInfo = MemoryUnmapInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core14.Enums.MemoryUnmapFlagBits.MemoryUnmapFlagBits' specifying
    -- additional parameters of the memory map operation.
    flags :: MemoryUnmapFlags
  , -- | @memory@ is the 'Vulkan.Core10.Handles.DeviceMemory' object to be
    -- unmapped.
    memory :: DeviceMemory
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryUnmapInfo)
#endif
deriving instance Show MemoryUnmapInfo

instance ToCStruct MemoryUnmapInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryUnmapInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_UNMAP_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryUnmapFlags)) (flags)
    poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_UNMAP_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    f

instance FromCStruct MemoryUnmapInfo where
  peekCStruct p = do
    flags <- peek @MemoryUnmapFlags ((p `plusPtr` 16 :: Ptr MemoryUnmapFlags))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    pure $ MemoryUnmapInfo
             flags memory

instance Storable MemoryUnmapInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryUnmapInfo where
  zero = MemoryUnmapInfo
           zero
           zero

