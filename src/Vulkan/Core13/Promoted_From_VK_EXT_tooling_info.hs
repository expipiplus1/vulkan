{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_tooling_info"
module Vulkan.Core13.Promoted_From_VK_EXT_tooling_info  ( getPhysicalDeviceToolProperties
                                                        , PhysicalDeviceToolProperties(..)
                                                        , StructureType(..)
                                                        , ToolPurposeFlagBits(..)
                                                        , ToolPurposeFlags
                                                        ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
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
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceToolProperties))
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.APIConstants (MAX_EXTENSION_NAME_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core13.Enums.ToolPurposeFlagBits (ToolPurposeFlags)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core13.Enums.ToolPurposeFlagBits (ToolPurposeFlagBits(..))
import Vulkan.Core13.Enums.ToolPurposeFlagBits (ToolPurposeFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceToolProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr PhysicalDeviceToolProperties -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr PhysicalDeviceToolProperties -> IO Result

-- | vkGetPhysicalDeviceToolProperties - Reports properties of tools active
-- on the specified physical device
--
-- = Description
--
-- If @pToolProperties@ is @NULL@, then the number of tools currently
-- active on @physicalDevice@ is returned in @pToolCount@. Otherwise,
-- @pToolCount@ /must/ point to a variable set by the user to the number of
-- elements in the @pToolProperties@ array, and on return the variable is
-- overwritten with the number of structures actually written to
-- @pToolProperties@. If @pToolCount@ is less than the number of currently
-- active tools, at most @pToolCount@ structures will be written.
--
-- The count and properties of active tools /may/ change in response to
-- events outside the scope of the specification. An application /should/
-- assume these properties might change at any given time.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceToolProperties-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceToolProperties-pToolCount-parameter#
--     @pToolCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceToolProperties-pToolProperties-parameter#
--     If the value referenced by @pToolCount@ is not @0@, and
--     @pToolProperties@ is not @NULL@, @pToolProperties@ /must/ be a valid
--     pointer to an array of @pToolCount@ 'PhysicalDeviceToolProperties'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_tooling_info VK_EXT_tooling_info>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Handles.PhysicalDevice', 'PhysicalDeviceToolProperties'
getPhysicalDeviceToolProperties :: forall io
                                 . (MonadIO io)
                                => -- | @physicalDevice@ is the handle to the physical device to query for
                                   -- active tools.
                                   PhysicalDevice
                                -> io (Result, ("toolProperties" ::: Vector PhysicalDeviceToolProperties))
getPhysicalDeviceToolProperties physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceToolPropertiesPtr = pVkGetPhysicalDeviceToolProperties (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceToolPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceToolProperties is null" Nothing Nothing
  let vkGetPhysicalDeviceToolProperties' = mkVkGetPhysicalDeviceToolProperties vkGetPhysicalDeviceToolPropertiesPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPToolCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceToolProperties" (vkGetPhysicalDeviceToolProperties' physicalDevice' (pPToolCount) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pToolCount <- lift $ peek @Word32 pPToolCount
  pPToolProperties <- ContT $ bracket (callocBytes @PhysicalDeviceToolProperties ((fromIntegral (pToolCount)) * 1048)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPToolProperties `advancePtrBytes` (i * 1048) :: Ptr PhysicalDeviceToolProperties) . ($ ())) [0..(fromIntegral (pToolCount)) - 1]
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceToolProperties" (vkGetPhysicalDeviceToolProperties' physicalDevice' (pPToolCount) ((pPToolProperties)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pToolCount' <- lift $ peek @Word32 pPToolCount
  pToolProperties' <- lift $ generateM (fromIntegral (pToolCount')) (\i -> peekCStruct @PhysicalDeviceToolProperties (((pPToolProperties) `advancePtrBytes` (1048 * (i)) :: Ptr PhysicalDeviceToolProperties)))
  pure $ ((r'), pToolProperties')


-- | VkPhysicalDeviceToolProperties - Structure providing information about
-- an active tool
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_tooling_info VK_EXT_tooling_info>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core13.Enums.ToolPurposeFlagBits.ToolPurposeFlags',
-- 'getPhysicalDeviceToolProperties',
-- 'Vulkan.Extensions.VK_EXT_tooling_info.getPhysicalDeviceToolPropertiesEXT'
data PhysicalDeviceToolProperties = PhysicalDeviceToolProperties
  { -- | @name@ is a null-terminated UTF-8 string containing the name of the
    -- tool.
    name :: ByteString
  , -- | @version@ is a null-terminated UTF-8 string containing the version of
    -- the tool.
    version :: ByteString
  , -- | @purposes@ is a bitmask of
    -- 'Vulkan.Core13.Enums.ToolPurposeFlagBits.ToolPurposeFlagBits' which is
    -- populated with purposes supported by the tool.
    purposes :: ToolPurposeFlags
  , -- | @description@ is a null-terminated UTF-8 string containing a description
    -- of the tool.
    description :: ByteString
  , -- | @layer@ is a null-terminated UTF-8 string containing the name of the
    -- layer implementing the tool, if the tool is implemented in a layer -
    -- otherwise it /may/ be an empty string.
    layer :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceToolProperties)
#endif
deriving instance Show PhysicalDeviceToolProperties

instance ToCStruct PhysicalDeviceToolProperties where
  withCStruct x f = allocaBytes 1048 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceToolProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (version)
    poke ((p `plusPtr` 528 :: Ptr ToolPurposeFlags)) (purposes)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 788 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (layer)
    f
  cStructSize = 1048
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 528 :: Ptr ToolPurposeFlags)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 788 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    f

instance FromCStruct PhysicalDeviceToolProperties where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    version <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    purposes <- peek @ToolPurposeFlags ((p `plusPtr` 528 :: Ptr ToolPurposeFlags))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 532 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    layer <- packCString (lowerArrayPtr ((p `plusPtr` 788 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    pure $ PhysicalDeviceToolProperties
             name version purposes description layer

instance Storable PhysicalDeviceToolProperties where
  sizeOf ~_ = 1048
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceToolProperties where
  zero = PhysicalDeviceToolProperties
           mempty
           mempty
           zero
           mempty
           mempty

