{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language TypeOperators #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VK_MAX_EXTENSION_NAME_SIZE
  , pattern VK_MAX_EXTENSION_NAME_SIZE
  , VkExtensionProperties(..)
  , FN_vkEnumerateDeviceExtensionProperties
  , PFN_vkEnumerateDeviceExtensionProperties
  , vkEnumerateDeviceExtensionProperties
  , FN_vkEnumerateInstanceExtensionProperties
  , PFN_vkEnumerateInstanceExtensionProperties
  , vkEnumerateInstanceExtensionProperties
  ) where

import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CChar(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , castPtrToFunPtr
  , nullPtr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import qualified GHC.Ptr
  ( Ptr(Ptr)
  )
import System.IO.Unsafe
  ( unsafeDupablePerformIO
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  , vkGetInstanceProcAddr'
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VK_MAX_EXTENSION_NAME_SIZE"
type VK_MAX_EXTENSION_NAME_SIZE = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_EXTENSION_NAME_SIZE"
pattern VK_MAX_EXTENSION_NAME_SIZE :: Integral a => a
pattern VK_MAX_EXTENSION_NAME_SIZE = 256

-- | VkExtensionProperties - Structure specifying an extension properties
--
-- = Description
--
-- Unresolved directive in VkExtensionProperties.txt -
-- include::{generated}\/validity\/structs\/VkExtensionProperties.txt[]
--
-- = See Also
--
-- 'vkEnumerateDeviceExtensionProperties',
-- 'vkEnumerateInstanceExtensionProperties'
data VkExtensionProperties = VkExtensionProperties
  { -- | @extensionName@ is a null-terminated string specifying the name of the
  -- extension.
  vkExtensionName :: Vector VK_MAX_EXTENSION_NAME_SIZE CChar
  , -- | @specVersion@ is the version of this extension. It is an integer,
  -- incremented with backward compatible changes.
  vkSpecVersion :: Word32
  }
  deriving (Eq, Show)

instance Storable VkExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek ptr = VkExtensionProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 256)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExtensionName (poked :: VkExtensionProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkExtensionProperties))

instance Zero VkExtensionProperties where
  zero = VkExtensionProperties zero
                               zero

-- | vkEnumerateDeviceExtensionProperties - Returns properties of available
-- physical device extensions
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device that will be queried.
--
-- -   @pLayerName@ is either @NULL@ or a pointer to a null-terminated
--     UTF-8 string naming the layer to retrieve extensions from.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     extension properties available or queried, and is treated in the
--     same fashion as the
--     'vkEnumerateInstanceExtensionProperties'::@pPropertyCount@
--     parameter.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkExtensionProperties' structures.
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
-- Unresolved directive in vkEnumerateDeviceExtensionProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateDeviceExtensionProperties.txt[]
--
-- = See Also
--
-- 'VkExtensionProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateDeviceExtensionProperties" vkEnumerateDeviceExtensionProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
#else
vkEnumerateDeviceExtensionProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
vkEnumerateDeviceExtensionProperties deviceCmds = mkVkEnumerateDeviceExtensionProperties (pVkEnumerateDeviceExtensionProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceExtensionProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult)
#endif

type FN_vkEnumerateDeviceExtensionProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
type PFN_vkEnumerateDeviceExtensionProperties = FunPtr FN_vkEnumerateDeviceExtensionProperties

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
--     'VkExtensionProperties' structures.
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
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available properties were returned.
--
-- Because the list of available layers may change externally between calls
-- to 'vkEnumerateInstanceExtensionProperties', two calls may retrieve
-- different results if a @pLayerName@ is available in one call but not in
-- another. The extensions supported by a layer may also change between two
-- calls, e.g. if the layer implementation is replaced by a different
-- version between those calls.
--
-- Implementations /must/ not advertise any pair of extensions that cannot
-- be enabled together due to behavioral differences, or any extension that
-- cannot be enabled against the advertised version.
--
-- Unresolved directive in vkEnumerateInstanceExtensionProperties.txt -
-- include::{generated}\/validity\/protos\/vkEnumerateInstanceExtensionProperties.txt[]
--
-- = See Also
--
-- 'VkExtensionProperties'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateInstanceExtensionProperties" vkEnumerateInstanceExtensionProperties :: ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
#else
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceExtensionProperties
  :: FunPtr (("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult) -> (("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult)

vkEnumerateInstanceExtensionProperties :: ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
vkEnumerateInstanceExtensionProperties = mkVkEnumerateInstanceExtensionProperties procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkEnumerateInstanceExtensionProperties $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "vkEnumerateInstanceExtensionProperties\NUL"#)
#endif

type FN_vkEnumerateInstanceExtensionProperties = ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
type PFN_vkEnumerateInstanceExtensionProperties = FunPtr FN_vkEnumerateInstanceExtensionProperties
