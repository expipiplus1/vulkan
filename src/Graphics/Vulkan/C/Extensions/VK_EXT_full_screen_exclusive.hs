{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( HMONITOR
  , VkFullScreenExclusiveEXT(..)
  , pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
  , VkSurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , VkSurfaceFullScreenExclusiveInfoEXT(..)
  , VkSurfaceFullScreenExclusiveWin32InfoEXT(..)
  , FN_vkAcquireFullScreenExclusiveModeEXT
  , PFN_vkAcquireFullScreenExclusiveModeEXT
  , vkAcquireFullScreenExclusiveModeEXT
  , FN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , vkGetPhysicalDeviceSurfacePresentModes2EXT
  , FN_vkReleaseFullScreenExclusiveModeEXT
  , PFN_vkReleaseFullScreenExclusiveModeEXT
  , vkReleaseFullScreenExclusiveModeEXT
  , pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  , vkGetDeviceGroupSurfacePresentModes2EXT
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( vkGetDeviceGroupSurfacePresentModes2EXT
  )


-- No documentation found for TopLevel "HMONITOR"
type HMONITOR = Ptr ()
  

-- ** VkFullScreenExclusiveEXT

-- No documentation found for TopLevel "VkFullScreenExclusiveEXT"
newtype VkFullScreenExclusiveEXT = VkFullScreenExclusiveEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkFullScreenExclusiveEXT where
  showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT = showString "VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT"
  showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT = showString "VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT"
  showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT = showString "VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT"
  showsPrec _ VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = showString "VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT"
  showsPrec p (VkFullScreenExclusiveEXT x) = showParen (p >= 11) (showString "VkFullScreenExclusiveEXT " . showsPrec 11 x)

instance Read VkFullScreenExclusiveEXT where
  readPrec = parens ( choose [ ("VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT",                pure VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT)
                             , ("VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT",                pure VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT)
                             , ("VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT",             pure VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT)
                             , ("VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT", pure VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFullScreenExclusiveEXT")
                        v <- step readPrec
                        pure (VkFullScreenExclusiveEXT v)
                        )
                    )

-- No documentation found for Nested "VkFullScreenExclusiveEXT" "VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT"
pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT = VkFullScreenExclusiveEXT 0

-- No documentation found for Nested "VkFullScreenExclusiveEXT" "VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT"
pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT = VkFullScreenExclusiveEXT 1

-- No documentation found for Nested "VkFullScreenExclusiveEXT" "VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT"
pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT = VkFullScreenExclusiveEXT 2

-- No documentation found for Nested "VkFullScreenExclusiveEXT" "VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT"
pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: VkFullScreenExclusiveEXT
pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = VkFullScreenExclusiveEXT 3

-- No documentation found for TopLevel "VkSurfaceCapabilitiesFullScreenExclusiveEXT"
data VkSurfaceCapabilitiesFullScreenExclusiveEXT = VkSurfaceCapabilitiesFullScreenExclusiveEXT
  { -- No documentation found for Nested "VkSurfaceCapabilitiesFullScreenExclusiveEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSurfaceCapabilitiesFullScreenExclusiveEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSurfaceCapabilitiesFullScreenExclusiveEXT" "fullScreenExclusiveSupported"
  vkFullScreenExclusiveSupported :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkSurfaceCapabilitiesFullScreenExclusiveEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSurfaceCapabilitiesFullScreenExclusiveEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceCapabilitiesFullScreenExclusiveEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceCapabilitiesFullScreenExclusiveEXT))
                *> poke (ptr `plusPtr` 16) (vkFullScreenExclusiveSupported (poked :: VkSurfaceCapabilitiesFullScreenExclusiveEXT))

instance Zero VkSurfaceCapabilitiesFullScreenExclusiveEXT where
  zero = VkSurfaceCapabilitiesFullScreenExclusiveEXT VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
                                                     zero
                                                     zero

-- No documentation found for TopLevel "VkSurfaceFullScreenExclusiveInfoEXT"
data VkSurfaceFullScreenExclusiveInfoEXT = VkSurfaceFullScreenExclusiveInfoEXT
  { -- No documentation found for Nested "VkSurfaceFullScreenExclusiveInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSurfaceFullScreenExclusiveInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSurfaceFullScreenExclusiveInfoEXT" "fullScreenExclusive"
  vkFullScreenExclusive :: VkFullScreenExclusiveEXT
  }
  deriving (Eq, Show)

instance Storable VkSurfaceFullScreenExclusiveInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSurfaceFullScreenExclusiveInfoEXT <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceFullScreenExclusiveInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceFullScreenExclusiveInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFullScreenExclusive (poked :: VkSurfaceFullScreenExclusiveInfoEXT))

instance Zero VkSurfaceFullScreenExclusiveInfoEXT where
  zero = VkSurfaceFullScreenExclusiveInfoEXT VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
                                             zero
                                             zero

-- No documentation found for TopLevel "VkSurfaceFullScreenExclusiveWin32InfoEXT"
data VkSurfaceFullScreenExclusiveWin32InfoEXT = VkSurfaceFullScreenExclusiveWin32InfoEXT
  { -- No documentation found for Nested "VkSurfaceFullScreenExclusiveWin32InfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSurfaceFullScreenExclusiveWin32InfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSurfaceFullScreenExclusiveWin32InfoEXT" "hmonitor"
  vkHmonitor :: HMONITOR
  }
  deriving (Eq, Show)

instance Storable VkSurfaceFullScreenExclusiveWin32InfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSurfaceFullScreenExclusiveWin32InfoEXT <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSurfaceFullScreenExclusiveWin32InfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSurfaceFullScreenExclusiveWin32InfoEXT))
                *> poke (ptr `plusPtr` 16) (vkHmonitor (poked :: VkSurfaceFullScreenExclusiveWin32InfoEXT))

instance Zero VkSurfaceFullScreenExclusiveWin32InfoEXT where
  zero = VkSurfaceFullScreenExclusiveWin32InfoEXT VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
                                                  zero
                                                  zero

-- No documentation found for TopLevel "vkAcquireFullScreenExclusiveModeEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAcquireFullScreenExclusiveModeEXT" vkAcquireFullScreenExclusiveModeEXT :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
#else
vkAcquireFullScreenExclusiveModeEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
vkAcquireFullScreenExclusiveModeEXT deviceCmds = mkVkAcquireFullScreenExclusiveModeEXT (pVkAcquireFullScreenExclusiveModeEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireFullScreenExclusiveModeEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
#endif

type FN_vkAcquireFullScreenExclusiveModeEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkAcquireFullScreenExclusiveModeEXT = FunPtr FN_vkAcquireFullScreenExclusiveModeEXT

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfacePresentModes2EXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSurfacePresentModes2EXT" vkGetPhysicalDeviceSurfacePresentModes2EXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
#else
vkGetPhysicalDeviceSurfacePresentModes2EXT :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
vkGetPhysicalDeviceSurfacePresentModes2EXT deviceCmds = mkVkGetPhysicalDeviceSurfacePresentModes2EXT (pVkGetPhysicalDeviceSurfacePresentModes2EXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModes2EXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceSurfacePresentModes2EXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT = FunPtr FN_vkGetPhysicalDeviceSurfacePresentModes2EXT

-- No documentation found for TopLevel "vkReleaseFullScreenExclusiveModeEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkReleaseFullScreenExclusiveModeEXT" vkReleaseFullScreenExclusiveModeEXT :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
#else
vkReleaseFullScreenExclusiveModeEXT :: DeviceCmds -> ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
vkReleaseFullScreenExclusiveModeEXT deviceCmds = mkVkReleaseFullScreenExclusiveModeEXT (pVkReleaseFullScreenExclusiveModeEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseFullScreenExclusiveModeEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
#endif

type FN_vkReleaseFullScreenExclusiveModeEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkReleaseFullScreenExclusiveModeEXT = FunPtr FN_vkReleaseFullScreenExclusiveModeEXT

-- No documentation found for Nested "VkResult" "VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT"
pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT :: VkResult
pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT = VkResult (-1000255000)

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME"
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = "VK_EXT_full_screen_exclusive"

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION"
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = 3

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT"
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT = VkStructureType 1000255002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT = VkStructureType 1000255000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT = VkStructureType 1000255001
