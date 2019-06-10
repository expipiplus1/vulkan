{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR(..)
  , VkDisplayPlaneCapabilities2KHR(..)
  , VkDisplayPlaneInfo2KHR(..)
  , VkDisplayPlaneProperties2KHR(..)
  , VkDisplayProperties2KHR(..)
  , FN_vkGetDisplayModeProperties2KHR
  , PFN_vkGetDisplayModeProperties2KHR
  , vkGetDisplayModeProperties2KHR
  , FN_vkGetDisplayPlaneCapabilities2KHR
  , PFN_vkGetDisplayPlaneCapabilities2KHR
  , vkGetDisplayPlaneCapabilities2KHR
  , FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , FN_vkGetPhysicalDeviceDisplayProperties2KHR
  , PFN_vkGetPhysicalDeviceDisplayProperties2KHR
  , vkGetPhysicalDeviceDisplayProperties2KHR
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  ) where

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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplayKHR
  , VkDisplayModeKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDisplayModeProperties2KHR"
data VkDisplayModeProperties2KHR = VkDisplayModeProperties2KHR
  { -- No documentation found for Nested "VkDisplayModeProperties2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplayModeProperties2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplayModeProperties2KHR" "displayModeProperties"
  vkDisplayModeProperties :: VkDisplayModePropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayModeProperties2KHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDisplayModeProperties2KHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayModeProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayModeProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayModeProperties (poked :: VkDisplayModeProperties2KHR))

instance Zero VkDisplayModeProperties2KHR where
  zero = VkDisplayModeProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
                                     zero
                                     zero

-- No documentation found for TopLevel "VkDisplayPlaneCapabilities2KHR"
data VkDisplayPlaneCapabilities2KHR = VkDisplayPlaneCapabilities2KHR
  { -- No documentation found for Nested "VkDisplayPlaneCapabilities2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplayPlaneCapabilities2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplayPlaneCapabilities2KHR" "capabilities"
  vkCapabilities :: VkDisplayPlaneCapabilitiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneCapabilities2KHR where
  sizeOf ~_ = 88
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneCapabilities2KHR <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
                                            <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneCapabilities2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneCapabilities2KHR))
                *> poke (ptr `plusPtr` 16) (vkCapabilities (poked :: VkDisplayPlaneCapabilities2KHR))

instance Zero VkDisplayPlaneCapabilities2KHR where
  zero = VkDisplayPlaneCapabilities2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
                                        zero
                                        zero

-- No documentation found for TopLevel "VkDisplayPlaneInfo2KHR"
data VkDisplayPlaneInfo2KHR = VkDisplayPlaneInfo2KHR
  { -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "mode"
  vkMode :: VkDisplayModeKHR
  , -- No documentation found for Nested "VkDisplayPlaneInfo2KHR" "planeIndex"
  vkPlaneIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneInfo2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneInfo2KHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 16) (vkMode (poked :: VkDisplayPlaneInfo2KHR))
                *> poke (ptr `plusPtr` 24) (vkPlaneIndex (poked :: VkDisplayPlaneInfo2KHR))

instance Zero VkDisplayPlaneInfo2KHR where
  zero = VkDisplayPlaneInfo2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
                                zero
                                zero
                                zero

-- No documentation found for TopLevel "VkDisplayPlaneProperties2KHR"
data VkDisplayPlaneProperties2KHR = VkDisplayPlaneProperties2KHR
  { -- No documentation found for Nested "VkDisplayPlaneProperties2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplayPlaneProperties2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplayPlaneProperties2KHR" "displayPlaneProperties"
  vkDisplayPlaneProperties :: VkDisplayPlanePropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneProperties2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayPlaneProperties2KHR <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPlaneProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPlaneProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayPlaneProperties (poked :: VkDisplayPlaneProperties2KHR))

instance Zero VkDisplayPlaneProperties2KHR where
  zero = VkDisplayPlaneProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
                                      zero
                                      zero

-- No documentation found for TopLevel "VkDisplayProperties2KHR"
data VkDisplayProperties2KHR = VkDisplayProperties2KHR
  { -- No documentation found for Nested "VkDisplayProperties2KHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplayProperties2KHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplayProperties2KHR" "displayProperties"
  vkDisplayProperties :: VkDisplayPropertiesKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayProperties2KHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDisplayProperties2KHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayProperties2KHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayProperties2KHR))
                *> poke (ptr `plusPtr` 16) (vkDisplayProperties (poked :: VkDisplayProperties2KHR))

instance Zero VkDisplayProperties2KHR where
  zero = VkDisplayProperties2KHR VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
                                 zero
                                 zero

-- No documentation found for TopLevel "vkGetDisplayModeProperties2KHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayModeProperties2KHR" vkGetDisplayModeProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult
#else
vkGetDisplayModeProperties2KHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult
vkGetDisplayModeProperties2KHR deviceCmds = mkVkGetDisplayModeProperties2KHR (pVkGetDisplayModeProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModeProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult)
#endif

type FN_vkGetDisplayModeProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult
type PFN_vkGetDisplayModeProperties2KHR = FunPtr FN_vkGetDisplayModeProperties2KHR

-- No documentation found for TopLevel "vkGetDisplayPlaneCapabilities2KHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayPlaneCapabilities2KHR" vkGetDisplayPlaneCapabilities2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult
#else
vkGetDisplayPlaneCapabilities2KHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult
vkGetDisplayPlaneCapabilities2KHR deviceCmds = mkVkGetDisplayPlaneCapabilities2KHR (pVkGetDisplayPlaneCapabilities2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilities2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult)
#endif

type FN_vkGetDisplayPlaneCapabilities2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult
type PFN_vkGetDisplayPlaneCapabilities2KHR = FunPtr FN_vkGetDisplayPlaneCapabilities2KHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPlaneProperties2KHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayPlaneProperties2KHR" vkGetPhysicalDeviceDisplayPlaneProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult
#else
vkGetPhysicalDeviceDisplayPlaneProperties2KHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult
vkGetPhysicalDeviceDisplayPlaneProperties2KHR deviceCmds = mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR (pVkGetPhysicalDeviceDisplayPlaneProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR = FunPtr FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayProperties2KHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayProperties2KHR" vkGetPhysicalDeviceDisplayProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult
#else
vkGetPhysicalDeviceDisplayProperties2KHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult
vkGetPhysicalDeviceDisplayProperties2KHR deviceCmds = mkVkGetPhysicalDeviceDisplayProperties2KHR (pVkGetPhysicalDeviceDisplayProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceDisplayProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayProperties2KHR = FunPtr FN_vkGetPhysicalDeviceDisplayProperties2KHR

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME"
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_display_properties2"

-- No documentation found for TopLevel "VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION"
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR = VkStructureType 1000121002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR = VkStructureType 1000121004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR = VkStructureType 1000121003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR = VkStructureType 1000121001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR = VkStructureType 1000121000
