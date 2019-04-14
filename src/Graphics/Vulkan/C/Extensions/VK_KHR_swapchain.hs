{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR(..)
  , VkBindImageMemorySwapchainInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkDeviceGroupPresentInfoKHR(..)
  , VkDeviceGroupPresentModeFlagBitsKHR(..)
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
  , VkDeviceGroupPresentModeFlagsKHR
  , VkDeviceGroupSwapchainCreateInfoKHR(..)
  , VkImageSwapchainCreateInfoKHR(..)
  , VkPresentInfoKHR(..)
  , VkSwapchainCreateFlagBitsKHR(..)
  , VkSwapchainCreateFlagsKHR
  , VkSwapchainCreateInfoKHR(..)
  , VkSwapchainKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkAcquireNextImage2KHR
#endif
  , FN_vkAcquireNextImage2KHR
  , PFN_vkAcquireNextImage2KHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkAcquireNextImageKHR
#endif
  , FN_vkAcquireNextImageKHR
  , PFN_vkAcquireNextImageKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateSwapchainKHR
#endif
  , FN_vkCreateSwapchainKHR
  , PFN_vkCreateSwapchainKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkDestroySwapchainKHR
#endif
  , FN_vkDestroySwapchainKHR
  , PFN_vkDestroySwapchainKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetDeviceGroupPresentCapabilitiesKHR
#endif
  , FN_vkGetDeviceGroupPresentCapabilitiesKHR
  , PFN_vkGetDeviceGroupPresentCapabilitiesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetDeviceGroupSurfacePresentModesKHR
#endif
  , FN_vkGetDeviceGroupSurfacePresentModesKHR
  , PFN_vkGetDeviceGroupSurfacePresentModesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDevicePresentRectanglesKHR
#endif
  , FN_vkGetPhysicalDevicePresentRectanglesKHR
  , PFN_vkGetPhysicalDevicePresentRectanglesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetSwapchainImagesKHR
#endif
  , FN_vkGetSwapchainImagesKHR
  , PFN_vkGetSwapchainImagesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkQueuePresentKHR
#endif
  , FN_vkQueuePresentKHR
  , PFN_vkQueuePresentKHR
  , pattern VK_ERROR_OUT_OF_DATE_KHR
  , pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  , pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_KHR_SWAPCHAIN_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR
  , pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_SUBOPTIMAL_KHR
  , pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  , Word64
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkSharingMode(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkImageUsageFlags
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  , VkQueue
  , VkSemaphore
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VK_MAX_DEVICE_GROUP_SIZE
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  , VkCompositeAlphaFlagBitsKHR(..)
  , VkPresentModeKHR(..)
  , VkSurfaceTransformFlagBitsKHR(..)
  , VkSurfaceKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkAcquireNextImageInfoKHR"
data VkAcquireNextImageInfoKHR = VkAcquireNextImageInfoKHR
  { -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "swapchain"
  vkSwapchain :: VkSwapchainKHR
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "timeout"
  vkTimeout :: Word64
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "semaphore"
  vkSemaphore :: VkSemaphore
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "fence"
  vkFence :: VkFence
  , -- No documentation found for Nested "VkAcquireNextImageInfoKHR" "deviceMask"
  vkDeviceMask :: Word32
  }
  deriving (Eq, Show)

instance Storable VkAcquireNextImageInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkAcquireNextImageInfoKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
                                       <*> peek (ptr `plusPtr` 40)
                                       <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkTimeout (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSemaphore (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkFence (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkDeviceMask (poked :: VkAcquireNextImageInfoKHR))
-- No documentation found for TopLevel "VkBindImageMemorySwapchainInfoKHR"
data VkBindImageMemorySwapchainInfoKHR = VkBindImageMemorySwapchainInfoKHR
  { -- No documentation found for Nested "VkBindImageMemorySwapchainInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindImageMemorySwapchainInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindImageMemorySwapchainInfoKHR" "swapchain"
  vkSwapchain :: VkSwapchainKHR
  , -- No documentation found for Nested "VkBindImageMemorySwapchainInfoKHR" "imageIndex"
  vkImageIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkBindImageMemorySwapchainInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkBindImageMemorySwapchainInfoKHR <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImageMemorySwapchainInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImageMemorySwapchainInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkBindImageMemorySwapchainInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkImageIndex (poked :: VkBindImageMemorySwapchainInfoKHR))
-- No documentation found for TopLevel "VkDeviceGroupPresentCapabilitiesKHR"
data VkDeviceGroupPresentCapabilitiesKHR = VkDeviceGroupPresentCapabilitiesKHR
  { -- No documentation found for Nested "VkDeviceGroupPresentCapabilitiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGroupPresentCapabilitiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGroupPresentCapabilitiesKHR" "presentMask"
  vkPresentMask :: Vector VK_MAX_DEVICE_GROUP_SIZE Word32
  , -- No documentation found for Nested "VkDeviceGroupPresentCapabilitiesKHR" "modes"
  vkModes :: VkDeviceGroupPresentModeFlagsKHR
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupPresentCapabilitiesKHR where
  sizeOf ~_ = 152
  alignment ~_ = 8
  peek ptr = VkDeviceGroupPresentCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 144)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupPresentCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupPresentCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkPresentMask (poked :: VkDeviceGroupPresentCapabilitiesKHR))
                *> poke (ptr `plusPtr` 144) (vkModes (poked :: VkDeviceGroupPresentCapabilitiesKHR))
-- No documentation found for TopLevel "VkDeviceGroupPresentInfoKHR"
data VkDeviceGroupPresentInfoKHR = VkDeviceGroupPresentInfoKHR
  { -- No documentation found for Nested "VkDeviceGroupPresentInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGroupPresentInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGroupPresentInfoKHR" "swapchainCount"
  vkSwapchainCount :: Word32
  , -- No documentation found for Nested "VkDeviceGroupPresentInfoKHR" "pDeviceMasks"
  vkPDeviceMasks :: Ptr Word32
  , -- No documentation found for Nested "VkDeviceGroupPresentInfoKHR" "mode"
  vkMode :: VkDeviceGroupPresentModeFlagBitsKHR
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupPresentInfoKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDeviceGroupPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchainCount (poked :: VkDeviceGroupPresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPDeviceMasks (poked :: VkDeviceGroupPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMode (poked :: VkDeviceGroupPresentInfoKHR))
-- ** VkDeviceGroupPresentModeFlagBitsKHR

-- No documentation found for TopLevel "VkDeviceGroupPresentModeFlagBitsKHR"
newtype VkDeviceGroupPresentModeFlagBitsKHR = VkDeviceGroupPresentModeFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDeviceGroupPresentModeFlagBitsKHR where
  showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR = showString "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR"
  showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR = showString "VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR"
  showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR = showString "VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR"
  showsPrec _ VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = showString "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR"
  showsPrec p (VkDeviceGroupPresentModeFlagBitsKHR x) = showParen (p >= 11) (showString "VkDeviceGroupPresentModeFlagBitsKHR " . showsPrec 11 x)

instance Read VkDeviceGroupPresentModeFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR",              pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR)
                             , ("VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR",             pure VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR)
                             , ("VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR",                pure VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR)
                             , ("VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR", pure VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDeviceGroupPresentModeFlagBitsKHR")
                        v <- step readPrec
                        pure (VkDeviceGroupPresentModeFlagBitsKHR v)
                        )
                    )

-- No documentation found for Nested "VkDeviceGroupPresentModeFlagBitsKHR" "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR"
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR :: VkDeviceGroupPresentModeFlagBitsKHR
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR = VkDeviceGroupPresentModeFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkDeviceGroupPresentModeFlagBitsKHR" "VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR"
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR :: VkDeviceGroupPresentModeFlagBitsKHR
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR = VkDeviceGroupPresentModeFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkDeviceGroupPresentModeFlagBitsKHR" "VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR"
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR :: VkDeviceGroupPresentModeFlagBitsKHR
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR = VkDeviceGroupPresentModeFlagBitsKHR 0x00000004

-- No documentation found for Nested "VkDeviceGroupPresentModeFlagBitsKHR" "VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR"
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR :: VkDeviceGroupPresentModeFlagBitsKHR
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = VkDeviceGroupPresentModeFlagBitsKHR 0x00000008
-- No documentation found for TopLevel "VkDeviceGroupPresentModeFlagsKHR"
type VkDeviceGroupPresentModeFlagsKHR = VkDeviceGroupPresentModeFlagBitsKHR
-- No documentation found for TopLevel "VkDeviceGroupSwapchainCreateInfoKHR"
data VkDeviceGroupSwapchainCreateInfoKHR = VkDeviceGroupSwapchainCreateInfoKHR
  { -- No documentation found for Nested "VkDeviceGroupSwapchainCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGroupSwapchainCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGroupSwapchainCreateInfoKHR" "modes"
  vkModes :: VkDeviceGroupPresentModeFlagsKHR
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupSwapchainCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupSwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkModes (poked :: VkDeviceGroupSwapchainCreateInfoKHR))
-- No documentation found for TopLevel "VkImageSwapchainCreateInfoKHR"
data VkImageSwapchainCreateInfoKHR = VkImageSwapchainCreateInfoKHR
  { -- No documentation found for Nested "VkImageSwapchainCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageSwapchainCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageSwapchainCreateInfoKHR" "swapchain"
  vkSwapchain :: VkSwapchainKHR
  }
  deriving (Eq, Show)

instance Storable VkImageSwapchainCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageSwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkImageSwapchainCreateInfoKHR))
-- No documentation found for TopLevel "VkPresentInfoKHR"
data VkPresentInfoKHR = VkPresentInfoKHR
  { -- No documentation found for Nested "VkPresentInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPresentInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPresentInfoKHR" "waitSemaphoreCount"
  vkWaitSemaphoreCount :: Word32
  , -- No documentation found for Nested "VkPresentInfoKHR" "pWaitSemaphores"
  vkPWaitSemaphores :: Ptr VkSemaphore
  , -- No documentation found for Nested "VkPresentInfoKHR" "swapchainCount"
  vkSwapchainCount :: Word32
  , -- No documentation found for Nested "VkPresentInfoKHR" "pSwapchains"
  vkPSwapchains :: Ptr VkSwapchainKHR
  , -- No documentation found for Nested "VkPresentInfoKHR" "pImageIndices"
  vkPImageIndices :: Ptr Word32
  , -- No documentation found for Nested "VkPresentInfoKHR" "pResults"
  vkPResults :: Ptr VkResult
  }
  deriving (Eq, Show)

instance Storable VkPresentInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphores (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSwapchainCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkPSwapchains (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPImageIndices (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkPResults (poked :: VkPresentInfoKHR))
-- ** VkSwapchainCreateFlagBitsKHR

-- No documentation found for TopLevel "VkSwapchainCreateFlagBitsKHR"
newtype VkSwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkSwapchainCreateFlagBitsKHR where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkSwapchainCreateFlagBitsKHR 0x00000001) = showString "VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
  showsPrec _ (VkSwapchainCreateFlagBitsKHR 0x00000002) = showString "VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR"
  showsPrec p (VkSwapchainCreateFlagBitsKHR x) = showParen (p >= 11) (showString "VkSwapchainCreateFlagBitsKHR " . showsPrec 11 x)

instance Read VkSwapchainCreateFlagBitsKHR where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR", pure (VkSwapchainCreateFlagBitsKHR 0x00000001))
                             , ("VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR",                   pure (VkSwapchainCreateFlagBitsKHR 0x00000002))
                             , ("VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR", pure (VkSwapchainCreateFlagBitsKHR 0x00000001))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSwapchainCreateFlagBitsKHR")
                        v <- step readPrec
                        pure (VkSwapchainCreateFlagBitsKHR v)
                        )
                    )


-- No documentation found for TopLevel "VkSwapchainCreateFlagsKHR"
type VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagBitsKHR
-- No documentation found for TopLevel "VkSwapchainCreateInfoKHR"
data VkSwapchainCreateInfoKHR = VkSwapchainCreateInfoKHR
  { -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "flags"
  vkFlags :: VkSwapchainCreateFlagsKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "surface"
  vkSurface :: VkSurfaceKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "minImageCount"
  vkMinImageCount :: Word32
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageFormat"
  vkImageFormat :: VkFormat
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageColorSpace"
  vkImageColorSpace :: VkColorSpaceKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageExtent"
  vkImageExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageArrayLayers"
  vkImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageUsage"
  vkImageUsage :: VkImageUsageFlags
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "imageSharingMode"
  vkImageSharingMode :: VkSharingMode
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "queueFamilyIndexCount"
  vkQueueFamilyIndexCount :: Word32
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "pQueueFamilyIndices"
  vkPQueueFamilyIndices :: Ptr Word32
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "preTransform"
  vkPreTransform :: VkSurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "compositeAlpha"
  vkCompositeAlpha :: VkCompositeAlphaFlagBitsKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "presentMode"
  vkPresentMode :: VkPresentModeKHR
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "clipped"
  vkClipped :: VkBool32
  , -- No documentation found for Nested "VkSwapchainCreateInfoKHR" "oldSwapchain"
  vkOldSwapchain :: VkSwapchainKHR
  }
  deriving (Eq, Show)

instance Storable VkSwapchainCreateInfoKHR where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek ptr = VkSwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 52)
                                      <*> peek (ptr `plusPtr` 56)
                                      <*> peek (ptr `plusPtr` 60)
                                      <*> peek (ptr `plusPtr` 64)
                                      <*> peek (ptr `plusPtr` 72)
                                      <*> peek (ptr `plusPtr` 80)
                                      <*> peek (ptr `plusPtr` 84)
                                      <*> peek (ptr `plusPtr` 88)
                                      <*> peek (ptr `plusPtr` 92)
                                      <*> peek (ptr `plusPtr` 96)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkSurface (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMinImageCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (vkImageFormat (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkImageColorSpace (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkImageExtent (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkImageArrayLayers (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkImageUsage (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 60) (vkImageSharingMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 64) (vkQueueFamilyIndexCount (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 72) (vkPQueueFamilyIndices (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 80) (vkPreTransform (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 84) (vkCompositeAlpha (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 88) (vkPresentMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 92) (vkClipped (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 96) (vkOldSwapchain (poked :: VkSwapchainCreateInfoKHR))
-- | Dummy data to tag the 'Ptr' with
data VkSwapchainKHR_T
-- No documentation found for TopLevel "VkSwapchainKHR"
type VkSwapchainKHR = Ptr VkSwapchainKHR_T
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkAcquireNextImage2KHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAcquireNextImage2KHR" vkAcquireNextImage2KHR :: ("device" ::: VkDevice) -> ("pAcquireInfo" ::: Ptr VkAcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult

#endif
type FN_vkAcquireNextImage2KHR = ("device" ::: VkDevice) -> ("pAcquireInfo" ::: Ptr VkAcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult
type PFN_vkAcquireNextImage2KHR = FunPtr FN_vkAcquireNextImage2KHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkAcquireNextImageKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAcquireNextImageKHR" vkAcquireNextImageKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("timeout" ::: Word64) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult

#endif
type FN_vkAcquireNextImageKHR = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("timeout" ::: Word64) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult
type PFN_vkAcquireNextImageKHR = FunPtr FN_vkAcquireNextImageKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateSwapchainKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSwapchainKHR" vkCreateSwapchainKHR :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchain" ::: Ptr VkSwapchainKHR) -> IO VkResult

#endif
type FN_vkCreateSwapchainKHR = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchain" ::: Ptr VkSwapchainKHR) -> IO VkResult
type PFN_vkCreateSwapchainKHR = FunPtr FN_vkCreateSwapchainKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkDestroySwapchainKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySwapchainKHR" vkDestroySwapchainKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroySwapchainKHR = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySwapchainKHR = FunPtr FN_vkDestroySwapchainKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetDeviceGroupPresentCapabilitiesKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceGroupPresentCapabilitiesKHR" vkGetDeviceGroupPresentCapabilitiesKHR :: ("device" ::: VkDevice) -> ("pDeviceGroupPresentCapabilities" ::: Ptr VkDeviceGroupPresentCapabilitiesKHR) -> IO VkResult

#endif
type FN_vkGetDeviceGroupPresentCapabilitiesKHR = ("device" ::: VkDevice) -> ("pDeviceGroupPresentCapabilities" ::: Ptr VkDeviceGroupPresentCapabilitiesKHR) -> IO VkResult
type PFN_vkGetDeviceGroupPresentCapabilitiesKHR = FunPtr FN_vkGetDeviceGroupPresentCapabilitiesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetDeviceGroupSurfacePresentModesKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceGroupSurfacePresentModesKHR" vkGetDeviceGroupSurfacePresentModesKHR :: ("device" ::: VkDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult

#endif
type FN_vkGetDeviceGroupSurfacePresentModesKHR = ("device" ::: VkDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult
type PFN_vkGetDeviceGroupSurfacePresentModesKHR = FunPtr FN_vkGetDeviceGroupSurfacePresentModesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDevicePresentRectanglesKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDevicePresentRectanglesKHR" vkGetPhysicalDevicePresentRectanglesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult

#endif
type FN_vkGetPhysicalDevicePresentRectanglesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult
type PFN_vkGetPhysicalDevicePresentRectanglesKHR = FunPtr FN_vkGetPhysicalDevicePresentRectanglesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetSwapchainImagesKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetSwapchainImagesKHR" vkGetSwapchainImagesKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr VkImage) -> IO VkResult

#endif
type FN_vkGetSwapchainImagesKHR = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr VkImage) -> IO VkResult
type PFN_vkGetSwapchainImagesKHR = FunPtr FN_vkGetSwapchainImagesKHR
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkQueuePresentKHR"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueuePresentKHR" vkQueuePresentKHR :: ("queue" ::: VkQueue) -> ("pPresentInfo" ::: Ptr VkPresentInfoKHR) -> IO VkResult

#endif
type FN_vkQueuePresentKHR = ("queue" ::: VkQueue) -> ("pPresentInfo" ::: Ptr VkPresentInfoKHR) -> IO VkResult
type PFN_vkQueuePresentKHR = FunPtr FN_vkQueuePresentKHR
-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_DATE_KHR"
pattern VK_ERROR_OUT_OF_DATE_KHR :: VkResult
pattern VK_ERROR_OUT_OF_DATE_KHR = VkResult (-1000001004)
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_PRESENT_SRC_KHR"
pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR :: VkImageLayout
pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR = VkImageLayout 1000001002
-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_EXTENSION_NAME"
pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"
-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_SPEC_VERSION"
pattern VK_KHR_SWAPCHAIN_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SWAPCHAIN_SPEC_VERSION = 70
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SWAPCHAIN_KHR"
pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR = VkObjectType 1000001000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR = VkStructureType 1000060010
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR = VkStructureType 1000060009
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR = VkStructureType 1000060007
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR = VkStructureType 1000060011
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR = VkStructureType 1000060012
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR = VkStructureType 1000060008
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_INFO_KHR"
pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR = VkStructureType 1000001001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR = VkStructureType 1000001000
-- No documentation found for Nested "VkResult" "VK_SUBOPTIMAL_KHR"
pattern VK_SUBOPTIMAL_KHR :: VkResult
pattern VK_SUBOPTIMAL_KHR = VkResult 1000001003
-- No documentation found for Nested "VkSwapchainCreateFlagBitsKHR" "VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR"
pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR :: VkSwapchainCreateFlagBitsKHR
pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR = VkSwapchainCreateFlagBitsKHR 0x00000002
-- No documentation found for Nested "VkSwapchainCreateFlagBitsKHR" "VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR :: VkSwapchainCreateFlagBitsKHR
pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = VkSwapchainCreateFlagBitsKHR 0x00000001
