{-# language CPP #-}
module Vulkan.Core10.LayerDiscovery  ( enumerateInstanceLayerProperties
                                     , enumerateDeviceLayerProperties
                                     , LayerProperties(..)
                                     ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (castFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Ptr (Ptr(Ptr))
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Dynamic (getInstanceProcAddr')
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkEnumerateDeviceLayerProperties))
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.APIConstants (MAX_EXTENSION_NAME_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceLayerProperties
  :: FunPtr (Ptr Word32 -> Ptr LayerProperties -> IO Result) -> Ptr Word32 -> Ptr LayerProperties -> IO Result

-- | vkEnumerateInstanceLayerProperties - Returns up to requested number of
-- global layer properties
--
-- = Parameters
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     layer properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'LayerProperties' structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of layer properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of layer properties available,
-- at most @pPropertyCount@ structures will be written. If @pPropertyCount@
-- is smaller than the number of layers available,
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available layer properties were returned.
--
-- The list of available layers may change at any time due to actions
-- outside of the Vulkan implementation, so two calls to
-- 'enumerateInstanceLayerProperties' with the same parameters /may/ return
-- different results, or retrieve different @pPropertyCount@ values or
-- @pProperties@ contents. Once an instance has been created, the layers
-- enabled for that instance will continue to be enabled and valid for the
-- lifetime of that instance, even if some of them become unavailable for
-- future instances.
--
-- == Valid Usage (Implicit)
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'LayerProperties' structures
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'LayerProperties'
enumerateInstanceLayerProperties :: forall io . MonadIO io => io (Result, ("properties" ::: Vector LayerProperties))
enumerateInstanceLayerProperties  = liftIO . evalContT $ do
  vkEnumerateInstanceLayerProperties' <- lift $ mkVkEnumerateInstanceLayerProperties . castFunPtr @_ @(("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr LayerProperties) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "vkEnumerateInstanceLayerProperties"#)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateInstanceLayerProperties' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @LayerProperties ((fromIntegral (pPropertyCount)) * 520)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 520) :: Ptr LayerProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkEnumerateInstanceLayerProperties' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @LayerProperties (((pPProperties) `advancePtrBytes` (520 * (i)) :: Ptr LayerProperties)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceLayerProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr LayerProperties -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr LayerProperties -> IO Result

-- | vkEnumerateDeviceLayerProperties - Returns properties of available
-- physical device layers
--
-- = Parameters
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     layer properties available or queried.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'LayerProperties' structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of layer properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of layer properties available,
-- at most @pPropertyCount@ structures will be written. If @pPropertyCount@
-- is smaller than the number of layers available,
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available layer properties were returned.
--
-- The list of layers enumerated by 'enumerateDeviceLayerProperties' /must/
-- be exactly the sequence of layers enabled for the instance. The members
-- of 'LayerProperties' for each enumerated layer /must/ be the same as the
-- properties when the layer was enumerated by
-- 'enumerateInstanceLayerProperties'.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'LayerProperties' structures
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'LayerProperties', 'Vulkan.Core10.Handles.PhysicalDevice'
enumerateDeviceLayerProperties :: forall io . MonadIO io => PhysicalDevice -> io (Result, ("properties" ::: Vector LayerProperties))
enumerateDeviceLayerProperties physicalDevice = liftIO . evalContT $ do
  let vkEnumerateDeviceLayerProperties' = mkVkEnumerateDeviceLayerProperties (pVkEnumerateDeviceLayerProperties (instanceCmds (physicalDevice :: PhysicalDevice)))
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateDeviceLayerProperties' physicalDevice' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @LayerProperties ((fromIntegral (pPropertyCount)) * 520)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 520) :: Ptr LayerProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkEnumerateDeviceLayerProperties' physicalDevice' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @LayerProperties (((pPProperties) `advancePtrBytes` (520 * (i)) :: Ptr LayerProperties)))
  pure $ ((r'), pProperties')


-- | VkLayerProperties - Structure specifying layer properties
--
-- = See Also
--
-- 'enumerateDeviceLayerProperties', 'enumerateInstanceLayerProperties'
data LayerProperties = LayerProperties
  { -- | @layerName@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_EXTENSION_NAME_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is the name of the layer. Use this
    -- name in the @ppEnabledLayerNames@ array passed in the
    -- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure to
    -- enable this layer for an instance.
    layerName :: ByteString
  , -- | @specVersion@ is the Vulkan version the layer was written to, encoded as
    -- described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-coreversions-versionnumbers>.
    specVersion :: Word32
  , -- | @implementationVersion@ is the version of this layer. It is an integer,
    -- increasing with backward compatible changes.
    implementationVersion :: Word32
  , -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which provides additional details that
    -- /can/ be used by the application to identify the layer.
    description :: ByteString
  }
  deriving (Typeable)
deriving instance Show LayerProperties

instance ToCStruct LayerProperties where
  withCStruct x f = allocaBytesAligned 520 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LayerProperties{..} f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (layerName)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (specVersion)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (implementationVersion)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 264 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    f
  cStructSize = 520
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 260 :: Ptr Word32)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 264 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    f

instance FromCStruct LayerProperties where
  peekCStruct p = do
    layerName <- packCString (lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_EXTENSION_NAME_SIZE CChar))))
    specVersion <- peek @Word32 ((p `plusPtr` 256 :: Ptr Word32))
    implementationVersion <- peek @Word32 ((p `plusPtr` 260 :: Ptr Word32))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 264 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    pure $ LayerProperties
             layerName specVersion implementationVersion description

instance Storable LayerProperties where
  sizeOf ~_ = 520
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LayerProperties where
  zero = LayerProperties
           mempty
           zero
           zero
           mempty

