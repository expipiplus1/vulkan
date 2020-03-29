{-# language CPP #-}
module Graphics.Vulkan.Core10.ExtensionDiscovery  ( enumerateInstanceExtensionProperties
                                                  , enumerateDeviceExtensionProperties
                                                  , ExtensionProperties(..)
                                                  ) where

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (castFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CChar(..))
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
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Dynamic (getInstanceProcAddr')
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkEnumerateDeviceExtensionProperties))
import Graphics.Vulkan.Core10.APIConstants (MAX_EXTENSION_NAME_SIZE)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceExtensionProperties
  :: FunPtr (Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result) -> Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

-- | vkEnumerateInstanceExtensionProperties - Returns up to requested number
-- of global extension properties
--
-- = Parameters
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'ExtensionProperties' structures.
--
-- = Description
--
-- When @pLayerName@ parameter is @NULL@, only extensions provided by the
-- Vulkan implementation or by implicitly enabled layers are returned. When
-- @pLayerName@ is the name of a layer, the instance extensions provided by
-- that layer are returned.
--
-- If @pProperties@ is @NULL@, then the number of extensions properties
-- available is returned in @pPropertyCount@. Otherwise, @pPropertyCount@
-- /must/ point to a variable set by the user to the number of elements in
-- the @pProperties@ array, and on return the variable is overwritten with
-- the number of structures actually written to @pProperties@. If
-- @pPropertyCount@ is less than the number of extension properties
-- available, at most @pPropertyCount@ structures will be written. If
-- @pPropertyCount@ is smaller than the number of extensions available,
-- 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Graphics.Vulkan.Core10.Enums.Result.SUCCESS', to indicate
-- that not all the available properties were returned.
--
-- Because the list of available layers may change externally between calls
-- to 'enumerateInstanceExtensionProperties', two calls may retrieve
-- different results if a @pLayerName@ is available in one call but not in
-- another. The extensions supported by a layer may also change between two
-- calls, e.g. if the layer implementation is replaced by a different
-- version between those calls.
--
-- Implementations /must/ not advertise any pair of extensions that cannot
-- be enabled together due to behavioral differences, or any extension that
-- cannot be enabled against the advertised version.
--
-- == Valid Usage (Implicit)
--
-- -   If @pLayerName@ is not @NULL@, @pLayerName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'ExtensionProperties' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_LAYER_NOT_PRESENT'
--
-- = See Also
--
-- 'ExtensionProperties'
enumerateInstanceExtensionProperties :: ("layerName" ::: Maybe ByteString) -> IO (Result, ("properties" ::: Vector ExtensionProperties))
enumerateInstanceExtensionProperties layerName = evalContT $ do
  vkEnumerateInstanceExtensionProperties' <- lift $ mkVkEnumerateInstanceExtensionProperties . castFunPtr @_ @(("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr ExtensionProperties) -> IO Result) <$> getInstanceProcAddr' nullPtr (Ptr "vkEnumerateInstanceExtensionProperties\NUL"#)
  pLayerName <- case (layerName) of
    Nothing -> pure nullPtr
    Just j -> ContT $ useAsCString (j)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateInstanceExtensionProperties' pLayerName (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @ExtensionProperties ((fromIntegral (pPropertyCount)) * 260)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 260) :: Ptr ExtensionProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkEnumerateInstanceExtensionProperties' pLayerName (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @ExtensionProperties (((pPProperties) `advancePtrBytes` (260 * (i)) :: Ptr ExtensionProperties)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceExtensionProperties
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result) -> Ptr PhysicalDevice_T -> Ptr CChar -> Ptr Word32 -> Ptr ExtensionProperties -> IO Result

-- | vkEnumerateDeviceExtensionProperties - Returns properties of available
-- physical device extensions
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.PhysicalDevice' is the physical
--     device that will be queried.
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, and is treated in the
--     same fashion as the
--     'enumerateInstanceExtensionProperties'::@pPropertyCount@ parameter.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'ExtensionProperties' structures.
--
-- = Description
--
-- When @pLayerName@ parameter is @NULL@, only extensions provided by the
-- Vulkan implementation or by implicitly enabled layers are returned. When
-- @pLayerName@ is the name of a layer, the device extensions provided by
-- that layer are returned.
--
-- Implementations /must/ not advertise any pair of extensions that cannot
-- be enabled together due to behavioral differences, or any extension that
-- cannot be enabled against the advertised version.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.PhysicalDevice' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   If @pLayerName@ is not @NULL@, @pLayerName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'ExtensionProperties' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_LAYER_NOT_PRESENT'
--
-- = See Also
--
-- 'ExtensionProperties', 'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
enumerateDeviceExtensionProperties :: PhysicalDevice -> ("layerName" ::: Maybe ByteString) -> IO (Result, ("properties" ::: Vector ExtensionProperties))
enumerateDeviceExtensionProperties physicalDevice layerName = evalContT $ do
  let vkEnumerateDeviceExtensionProperties' = mkVkEnumerateDeviceExtensionProperties (pVkEnumerateDeviceExtensionProperties (instanceCmds (physicalDevice :: PhysicalDevice)))
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pLayerName <- case (layerName) of
    Nothing -> pure nullPtr
    Just j -> ContT $ useAsCString (j)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumerateDeviceExtensionProperties' physicalDevice' pLayerName (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @ExtensionProperties ((fromIntegral (pPropertyCount)) * 260)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 260) :: Ptr ExtensionProperties) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkEnumerateDeviceExtensionProperties' physicalDevice' pLayerName (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @ExtensionProperties (((pPProperties) `advancePtrBytes` (260 * (i)) :: Ptr ExtensionProperties)))
  pure $ ((r'), pProperties')


-- | VkExtensionProperties - Structure specifying an extension properties
--
-- = See Also
--
-- 'enumerateDeviceExtensionProperties',
-- 'enumerateInstanceExtensionProperties'
data ExtensionProperties = ExtensionProperties
  { -- | @extensionName@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_EXTENSION_NAME_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is the name of the
    -- extension.
    extensionName :: ByteString
  , -- | @specVersion@ is the version of this extension. It is an integer,
    -- incremented with backward compatible changes.
    specVersion :: Word32
  }
  deriving (Typeable)
deriving instance Show ExtensionProperties

instance ToCStruct ExtensionProperties where
  withCStruct x f = allocaBytesAligned 260 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ExtensionProperties{..} f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_EXTENSION_NAME_SIZE CChar))) (extensionName)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (specVersion)
    f
  cStructSize = 260
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_EXTENSION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 256 :: Ptr Word32)) (zero)
    f

instance FromCStruct ExtensionProperties where
  peekCStruct p = do
    extensionName <- packCString (lowerArrayPtr ((p `plusPtr` 0 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_EXTENSION_NAME_SIZE CChar))))
    specVersion <- peek @Word32 ((p `plusPtr` 256 :: Ptr Word32))
    pure $ ExtensionProperties
             extensionName specVersion

instance Storable ExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ExtensionProperties where
  zero = ExtensionProperties
           mempty
           zero

