{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language TypeOperators #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VK_MAX_DESCRIPTION_SIZE
  , pattern VK_MAX_DESCRIPTION_SIZE
  , VkLayerProperties(..)
  , FN_vkEnumerateDeviceLayerProperties
  , PFN_vkEnumerateDeviceLayerProperties
  , vkEnumerateDeviceLayerProperties
  , FN_vkEnumerateInstanceLayerProperties
  , PFN_vkEnumerateInstanceLayerProperties
  , vkEnumerateInstanceLayerProperties
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
import Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VK_MAX_EXTENSION_NAME_SIZE
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VK_MAX_DESCRIPTION_SIZE"
type VK_MAX_DESCRIPTION_SIZE = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_DESCRIPTION_SIZE"
pattern VK_MAX_DESCRIPTION_SIZE :: Integral a => a
pattern VK_MAX_DESCRIPTION_SIZE = 256

-- | VkLayerProperties - Structure specifying layer properties
--
-- = See Also
--
-- 'vkEnumerateDeviceLayerProperties', 'vkEnumerateInstanceLayerProperties'
data VkLayerProperties = VkLayerProperties
  { -- | @layerName@ is a null-terminated UTF-8 string specifying the name of the
  -- layer. Use this name in the @ppEnabledLayerNames@ array passed in the
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo'
  -- structure to enable this layer for an instance.
  vkLayerName :: Vector VK_MAX_EXTENSION_NAME_SIZE CChar
  , -- | @specVersion@ is the Vulkan version the layer was written to, encoded as
  -- described in
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#extendingvulkan-coreversions-versionnumbers>.
  vkSpecVersion :: Word32
  , -- | @implementationVersion@ is the version of this layer. It is an integer,
  -- increasing with backward compatible changes.
  vkImplementationVersion :: Word32
  , -- | @description@ is a null-terminated UTF-8 string providing additional
  -- details that /can/ be used by the application to identify the layer.
  vkDescription :: Vector VK_MAX_DESCRIPTION_SIZE CChar
  }
  deriving (Eq, Show)

instance Storable VkLayerProperties where
  sizeOf ~_ = 520
  alignment ~_ = 4
  peek ptr = VkLayerProperties <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 256)
                               <*> peek (ptr `plusPtr` 260)
                               <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLayerName (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 260) (vkImplementationVersion (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 264) (vkDescription (poked :: VkLayerProperties))

instance Zero VkLayerProperties where
  zero = VkLayerProperties zero
                           zero
                           zero
                           zero

-- | vkEnumerateDeviceLayerProperties - Returns properties of available
-- physical device layers
--
-- = Parameters
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     layer properties available or queried.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkLayerProperties' structures.
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
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available layer properties were returned.
--
-- The list of layers enumerated by 'vkEnumerateDeviceLayerProperties'
-- /must/ be exactly the sequence of layers enabled for the instance. The
-- members of 'VkLayerProperties' for each enumerated layer /must/ be the
-- same as the properties when the layer was enumerated by
-- 'vkEnumerateInstanceLayerProperties'.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@ 'VkLayerProperties' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'VkLayerProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateDeviceLayerProperties" vkEnumerateDeviceLayerProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
#else
vkEnumerateDeviceLayerProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
vkEnumerateDeviceLayerProperties deviceCmds = mkVkEnumerateDeviceLayerProperties (pVkEnumerateDeviceLayerProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceLayerProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult)
#endif

type FN_vkEnumerateDeviceLayerProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
type PFN_vkEnumerateDeviceLayerProperties = FunPtr FN_vkEnumerateDeviceLayerProperties

-- | vkEnumerateInstanceLayerProperties - Returns up to requested number of
-- global layer properties
--
-- = Parameters
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     layer properties available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'VkLayerProperties' structures.
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
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available layer properties were returned.
--
-- The list of available layers may change at any time due to actions
-- outside of the Vulkan implementation, so two calls to
-- 'vkEnumerateInstanceLayerProperties' with the same parameters /may/
-- return different results, or retrieve different @pPropertyCount@ values
-- or @pProperties@ contents. Once an instance has been created, the layers
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
--     to an array of @pPropertyCount@ 'VkLayerProperties' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'VkLayerProperties'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateInstanceLayerProperties" vkEnumerateInstanceLayerProperties :: ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
#else
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceLayerProperties
  :: FunPtr (("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult) -> (("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult)

vkEnumerateInstanceLayerProperties :: ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
vkEnumerateInstanceLayerProperties = mkVkEnumerateInstanceLayerProperties procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkEnumerateInstanceLayerProperties $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "vkEnumerateInstanceLayerProperties\NUL"#)
#endif

type FN_vkEnumerateInstanceLayerProperties = ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
type PFN_vkEnumerateInstanceLayerProperties = FunPtr FN_vkEnumerateInstanceLayerProperties
