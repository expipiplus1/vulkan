{-# language CPP #-}
-- | = Name
--
-- VK_KHR_map_memory2 - device extension
--
-- = VK_KHR_map_memory2
--
-- [__Name String__]
--     @VK_KHR_map_memory2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     272
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Faith Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_map_memory2] @gfxstrand%0A*Here describe the issue or question you have about the VK_KHR_map_memory2 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_map_memory2.adoc VK_KHR_map_memory2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-03-14
--
-- [__Interactions and External Dependencies__]
--
--     -   None
--
-- [__Contributors__]
--
--     -   Faith Ekstrand, Collabora
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension provides extensible versions of the Vulkan memory map and
-- unmap entry points. The new entry points are functionally identical to
-- the core entry points, except that their parameters are specified using
-- extensible structures that can be used to pass extension-specific
-- information.
--
-- == New Commands
--
-- -   'mapMemory2KHR'
--
-- -   'unmapMemory2KHR'
--
-- == New Structures
--
-- -   'MemoryMapInfoKHR'
--
-- -   'MemoryUnmapInfoKHR'
--
-- == New Enums
--
-- -   'MemoryUnmapFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'MemoryUnmapFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAP_MEMORY_2_EXTENSION_NAME'
--
-- -   'KHR_MAP_MEMORY_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR'
--
-- == Version History
--
-- -   Revision 0, 2022-08-03 (Faith Ekstrand)
--
--     -   Internal revisions
--
-- -   Revision 1, 2023-03-14
--
--     -   Public release
--
-- == See Also
--
-- 'MemoryMapInfoKHR', 'MemoryUnmapFlagBitsKHR', 'MemoryUnmapFlagsKHR',
-- 'MemoryUnmapInfoKHR', 'mapMemory2KHR', 'unmapMemory2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_map_memory2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_map_memory2  ( mapMemory2KHR
                                             , unmapMemory2KHR
                                             , MemoryMapInfoKHR(..)
                                             , MemoryUnmapInfoKHR(..)
                                             , MemoryUnmapFlagsKHR
                                             , MemoryUnmapFlagBitsKHR( MEMORY_UNMAP_RESERVE_BIT_EXT
                                                                     , ..
                                                                     )
                                             , KHR_MAP_MEMORY_2_SPEC_VERSION
                                             , pattern KHR_MAP_MEMORY_2_SPEC_VERSION
                                             , KHR_MAP_MEMORY_2_EXTENSION_NAME
                                             , pattern KHR_MAP_MEMORY_2_EXTENSION_NAME
                                             ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkMapMemory2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkUnmapMemory2KHR))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.MemoryMapFlagBits (MemoryMapFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_map_memory_placed (MemoryMapPlacedInfoEXT)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMapMemory2KHR
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct MemoryMapInfoKHR) -> Ptr (Ptr ()) -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct MemoryMapInfoKHR) -> Ptr (Ptr ()) -> IO Result

-- | vkMapMemory2KHR - Map a memory object into application address space
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- 'Vulkan.Core10.Handles.Device', 'MemoryMapInfoKHR'
mapMemory2KHR :: forall a io
               . (Extendss MemoryMapInfoKHR a, PokeChain a, MonadIO io)
              => -- | @device@ is the logical device that owns the memory.
                 --
                 -- #VUID-vkMapMemory2KHR-device-parameter# @device@ /must/ be a valid
                 -- 'Vulkan.Core10.Handles.Device' handle
                 Device
              -> -- | @pMemoryMapInfo@ is a pointer to a 'MemoryMapInfoKHR' structure
                 -- describing parameters of the map.
                 --
                 -- #VUID-vkMapMemory2KHR-pMemoryMapInfo-parameter# @pMemoryMapInfo@ /must/
                 -- be a valid pointer to a valid 'MemoryMapInfoKHR' structure
                 (MemoryMapInfoKHR a)
              -> io (("data" ::: Ptr ()))
mapMemory2KHR device memoryMapInfo = liftIO . evalContT $ do
  let vkMapMemory2KHRPtr = pVkMapMemory2KHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkMapMemory2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkMapMemory2KHR is null" Nothing Nothing
  let vkMapMemory2KHR' = mkVkMapMemory2KHR vkMapMemory2KHRPtr
  pMemoryMapInfo <- ContT $ withCStruct (memoryMapInfo)
  pPpData <- ContT $ bracket (callocBytes @(Ptr ()) 8) free
  r <- lift $ traceAroundEvent "vkMapMemory2KHR" (vkMapMemory2KHR'
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
  "dynamic" mkVkUnmapMemory2KHR
  :: FunPtr (Ptr Device_T -> Ptr MemoryUnmapInfoKHR -> IO Result) -> Ptr Device_T -> Ptr MemoryUnmapInfoKHR -> IO Result

-- | vkUnmapMemory2KHR - Unmap a previously mapped memory object
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- 'Vulkan.Core10.Handles.Device', 'MemoryUnmapInfoKHR'
unmapMemory2KHR :: forall io
                 . (MonadIO io)
                => -- | @device@ is the logical device that owns the memory.
                   --
                   -- #VUID-vkUnmapMemory2KHR-device-parameter# @device@ /must/ be a valid
                   -- 'Vulkan.Core10.Handles.Device' handle
                   Device
                -> -- | @pMemoryUnmapInfo@ is a pointer to a 'MemoryUnmapInfoKHR' structure
                   -- describing parameters of the unmap.
                   --
                   -- #VUID-vkUnmapMemory2KHR-pMemoryUnmapInfo-parameter# @pMemoryUnmapInfo@
                   -- /must/ be a valid pointer to a valid 'MemoryUnmapInfoKHR' structure
                   MemoryUnmapInfoKHR
                -> io ()
unmapMemory2KHR device memoryUnmapInfo = liftIO . evalContT $ do
  let vkUnmapMemory2KHRPtr = pVkUnmapMemory2KHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkUnmapMemory2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUnmapMemory2KHR is null" Nothing Nothing
  let vkUnmapMemory2KHR' = mkVkUnmapMemory2KHR vkUnmapMemory2KHRPtr
  pMemoryUnmapInfo <- ContT $ withCStruct (memoryUnmapInfo)
  r <- lift $ traceAroundEvent "vkUnmapMemory2KHR" (vkUnmapMemory2KHR'
                                                      (deviceHandle (device))
                                                      pMemoryUnmapInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkMemoryMapInfoKHR - Structure containing parameters of a memory map
-- operation
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryMapInfoKHR-memory-07958# @memory@ /must/ not be
--     currently host mapped
--
-- -   #VUID-VkMemoryMapInfoKHR-offset-07959# @offset@ /must/ be less than
--     the size of @memory@
--
-- -   #VUID-VkMemoryMapInfoKHR-size-07960# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be greater
--     than @0@
--
-- -   #VUID-VkMemoryMapInfoKHR-size-07961# If @size@ is not equal to
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be less than
--     or equal to the size of the @memory@ minus @offset@
--
-- -   #VUID-VkMemoryMapInfoKHR-memory-07962# @memory@ /must/ have been
--     created with a memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--
-- -   #VUID-VkMemoryMapInfoKHR-memory-07963# @memory@ /must/ not have been
--     allocated with multiple instances
--
-- -   #VUID-VkMemoryMapInfoKHR-flags-09569# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-memoryMapPlaced memoryMapPlaced>
--     feature /must/ be enabled
--
-- -   #VUID-VkMemoryMapInfoKHR-flags-09570# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@, the @pNext@ chain /must/ include a
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.MemoryMapPlacedInfoEXT'
--     structure and
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.MemoryMapPlacedInfoEXT'::@pPlacedAddress@
--     /must/ not be @NULL@
--
-- -   #VUID-VkMemoryMapInfoKHR-flags-09571# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-memoryMapRangePlaced memoryMapRangePlaced>
--     feature is not enabled, @offset@ /must/ be zero
--
-- -   #VUID-VkMemoryMapInfoKHR-flags-09572# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-memoryMapRangePlaced memoryMapRangePlaced>
--     feature is not enabled, @size@ /must/ be
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE'
--
-- -   #VUID-VkMemoryMapInfoKHR-flags-09573# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-memoryMapRangePlaced memoryMapRangePlaced>
--     feature is enabled, @offset@ /must/ be aligned to an integer
--     multiple of
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.PhysicalDeviceMapMemoryPlacedPropertiesEXT'::@minPlacedMemoryMapAlignment@
--
-- -   #VUID-VkMemoryMapInfoKHR-flags-09574# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@ and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-memoryMapRangePlaced memoryMapRangePlaced>
--     feature is enabled, @size@ /must/ be
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE' or be aligned to an integer
--     multiple of
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.PhysicalDeviceMapMemoryPlacedPropertiesEXT'::@minPlacedMemoryMapAlignment@
--
-- -   #VUID-VkMemoryMapInfoKHR-flags-09575# If
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT' is
--     set in @flags@, the memory object /must/ not have been imported from
--     a handle type of
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryMapInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR'
--
-- -   #VUID-VkMemoryMapInfoKHR-pNext-pNext# @pNext@ /must/ be @NULL@ or a
--     pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_map_memory_placed.MemoryMapPlacedInfoEXT'
--
-- -   #VUID-VkMemoryMapInfoKHR-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkMemoryMapInfoKHR-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.MemoryMapFlagBits.MemoryMapFlagBits' values
--
-- -   #VUID-VkMemoryMapInfoKHR-memory-parameter# @memory@ /must/ be a
--     valid 'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- == Host Synchronization
--
-- -   Host access to @memory@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.MemoryMapFlagBits.MemoryMapFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'mapMemory2KHR'
data MemoryMapInfoKHR (es :: [Type]) = MemoryMapInfoKHR
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
deriving instance Generic (MemoryMapInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (MemoryMapInfoKHR es)

instance Extensible MemoryMapInfoKHR where
  extensibleTypeName = "MemoryMapInfoKHR"
  setNext MemoryMapInfoKHR{..} next' = MemoryMapInfoKHR{next = next', ..}
  getNext MemoryMapInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends MemoryMapInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @MemoryMapPlacedInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss MemoryMapInfoKHR es
         , PokeChain es ) => ToCStruct (MemoryMapInfoKHR es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryMapInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR)
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
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_MAP_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    lift $ f

instance ( Extendss MemoryMapInfoKHR es
         , PeekChain es ) => FromCStruct (MemoryMapInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @MemoryMapFlags ((p `plusPtr` 16 :: Ptr MemoryMapFlags))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    offset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    pure $ MemoryMapInfoKHR
             next flags memory offset size

instance es ~ '[] => Zero (MemoryMapInfoKHR es) where
  zero = MemoryMapInfoKHR
           ()
           zero
           zero
           zero
           zero


-- | VkMemoryUnmapInfoKHR - Structure containing parameters of a memory unmap
-- operation
--
-- == Valid Usage
--
-- -   #VUID-VkMemoryUnmapInfoKHR-memory-07964# @memory@ /must/ be
--     currently host mapped
--
-- -   #VUID-VkMemoryUnmapInfoKHR-flags-09579# If
--     'MEMORY_UNMAP_RESERVE_BIT_EXT' is set in @flags@, the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-memoryUnmapReserve memoryUnmapReserve>
--     /must/ be enabled
--
-- -   #VUID-VkMemoryUnmapInfoKHR-flags-09580# If
--     'MEMORY_UNMAP_RESERVE_BIT_EXT' is set in @flags@, the memory object
--     /must/ not have been imported from a handle type of
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_ALLOCATION_BIT_EXT'
--     or
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_HOST_MAPPED_FOREIGN_MEMORY_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkMemoryUnmapInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR'
--
-- -   #VUID-VkMemoryUnmapInfoKHR-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkMemoryUnmapInfoKHR-flags-parameter# @flags@ /must/ be a
--     valid combination of 'MemoryUnmapFlagBitsKHR' values
--
-- -   #VUID-VkMemoryUnmapInfoKHR-memory-parameter# @memory@ /must/ be a
--     valid 'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- == Host Synchronization
--
-- -   Host access to @memory@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- 'Vulkan.Core10.Handles.DeviceMemory', 'MemoryUnmapFlagsKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'unmapMemory2KHR'
data MemoryUnmapInfoKHR = MemoryUnmapInfoKHR
  { -- | @flags@ is a bitmask of 'MemoryUnmapFlagBitsKHR' specifying additional
    -- parameters of the memory map operation.
    flags :: MemoryUnmapFlagsKHR
  , -- | @memory@ is the 'Vulkan.Core10.Handles.DeviceMemory' object to be
    -- unmapped.
    memory :: DeviceMemory
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryUnmapInfoKHR)
#endif
deriving instance Show MemoryUnmapInfoKHR

instance ToCStruct MemoryUnmapInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryUnmapInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr MemoryUnmapFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_UNMAP_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    f

instance FromCStruct MemoryUnmapInfoKHR where
  peekCStruct p = do
    flags <- peek @MemoryUnmapFlagsKHR ((p `plusPtr` 16 :: Ptr MemoryUnmapFlagsKHR))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    pure $ MemoryUnmapInfoKHR
             flags memory

instance Storable MemoryUnmapInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryUnmapInfoKHR where
  zero = MemoryUnmapInfoKHR
           zero
           zero


type MemoryUnmapFlagsKHR = MemoryUnmapFlagBitsKHR

-- | VkMemoryUnmapFlagBitsKHR - Bitmask specifying additional parameters of a
-- memory unmap
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_map_memory2 VK_KHR_map_memory2>,
-- 'MemoryUnmapFlagsKHR'
newtype MemoryUnmapFlagBitsKHR = MemoryUnmapFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'MEMORY_UNMAP_RESERVE_BIT_EXT' requests that virtual address range
-- currently occupied by the memory map remain reserved after the
-- 'unmapMemory2KHR' call completes. Future system memory map operations or
-- calls to 'Vulkan.Core10.Memory.mapMemory' or 'mapMemory2KHR' will not
-- return addresses in that range unless the range has since been
-- unreserved by the client or the mapping is explicitly placed in that
-- range by calling 'mapMemory2KHR' with
-- 'Vulkan.Core10.Enums.MemoryMapFlagBits.MEMORY_MAP_PLACED_BIT_EXT', or
-- doing the system memory map equivalent. When
-- 'MEMORY_UNMAP_RESERVE_BIT_EXT' is set, the memory unmap operation /may/
-- fail, in which case the memory object will remain host mapped and
-- 'unmapMemory2KHR' will return
-- 'Vulkan.Core10.Enums.Result.ERROR_MEMORY_MAP_FAILED'.
pattern MEMORY_UNMAP_RESERVE_BIT_EXT = MemoryUnmapFlagBitsKHR 0x00000001

conNameMemoryUnmapFlagBitsKHR :: String
conNameMemoryUnmapFlagBitsKHR = "MemoryUnmapFlagBitsKHR"

enumPrefixMemoryUnmapFlagBitsKHR :: String
enumPrefixMemoryUnmapFlagBitsKHR = "MEMORY_UNMAP_RESERVE_BIT_EXT"

showTableMemoryUnmapFlagBitsKHR :: [(MemoryUnmapFlagBitsKHR, String)]
showTableMemoryUnmapFlagBitsKHR = [(MEMORY_UNMAP_RESERVE_BIT_EXT, "")]

instance Show MemoryUnmapFlagBitsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixMemoryUnmapFlagBitsKHR
      showTableMemoryUnmapFlagBitsKHR
      conNameMemoryUnmapFlagBitsKHR
      (\(MemoryUnmapFlagBitsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read MemoryUnmapFlagBitsKHR where
  readPrec =
    enumReadPrec
      enumPrefixMemoryUnmapFlagBitsKHR
      showTableMemoryUnmapFlagBitsKHR
      conNameMemoryUnmapFlagBitsKHR
      MemoryUnmapFlagBitsKHR

type KHR_MAP_MEMORY_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAP_MEMORY_2_SPEC_VERSION"
pattern KHR_MAP_MEMORY_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAP_MEMORY_2_SPEC_VERSION = 1


type KHR_MAP_MEMORY_2_EXTENSION_NAME = "VK_KHR_map_memory2"

-- No documentation found for TopLevel "VK_KHR_MAP_MEMORY_2_EXTENSION_NAME"
pattern KHR_MAP_MEMORY_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAP_MEMORY_2_EXTENSION_NAME = "VK_KHR_map_memory2"

