{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkSwapchainCreateFlagBitsKHR(..)
  , VkDeviceGroupPresentModeFlagBitsKHR(..)
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
  , pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  , pattern VK_SUBOPTIMAL_KHR
  , pattern VK_ERROR_OUT_OF_DATE_KHR
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
  , pattern VK_KHR_SWAPCHAIN_SPEC_VERSION
  , pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME
  , VkSwapchainKHR
  , vkCreateSwapchainKHR
  , vkDestroySwapchainKHR
  , vkGetSwapchainImagesKHR
  , vkAcquireNextImageKHR
  , vkQueuePresentKHR
  , vkGetDeviceGroupPresentCapabilitiesKHR
  , vkGetDeviceGroupSurfacePresentModesKHR
  , vkAcquireNextImage2KHR
  , vkGetPhysicalDevicePresentRectanglesKHR
  , VkSwapchainCreateInfoKHR(..)
  , VkPresentInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkImageSwapchainCreateInfoKHR(..)
  , VkBindImageMemorySwapchainInfoKHR(..)
  , VkAcquireNextImageInfoKHR(..)
  , VkDeviceGroupPresentInfoKHR(..)
  , VkDeviceGroupSwapchainCreateInfoKHR(..)
  , VkSwapchainCreateFlagsKHR
  , VkDeviceGroupPresentModeFlagsKHR
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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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


import Graphics.Vulkan.Core10.Buffer
  ( VkSharingMode(..)
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageUsageFlags
  , VkPhysicalDevice
  , VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkImage
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkExtent2D(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( VkQueue
  , VkFence
  , VkSemaphore
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VK_MAX_DEVICE_GROUP_SIZE
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  , VkCompositeAlphaFlagBitsKHR(..)
  , VkSurfaceTransformFlagBitsKHR(..)
  , VkColorSpaceKHR(..)
  , VkSurfaceKHR
  )


-- ** VkSwapchainCreateFlagBitsKHR

-- | 
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


-- ** VkDeviceGroupPresentModeFlagBitsKHR

-- | 
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

-- | Present from local memory
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR :: VkDeviceGroupPresentModeFlagBitsKHR
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR = VkDeviceGroupPresentModeFlagBitsKHR 0x00000001

-- | Present from remote memory
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR :: VkDeviceGroupPresentModeFlagBitsKHR
pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR = VkDeviceGroupPresentModeFlagBitsKHR 0x00000002

-- | Present sum of local and/or remote memory
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR :: VkDeviceGroupPresentModeFlagBitsKHR
pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR = VkDeviceGroupPresentModeFlagBitsKHR 0x00000004

-- | Each physical device presents from local memory
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR :: VkDeviceGroupPresentModeFlagBitsKHR
pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = VkDeviceGroupPresentModeFlagBitsKHR 0x00000008
-- | Nothing
pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR :: VkImageLayout
pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR = VkImageLayout 1000001002
-- | Nothing
pattern VK_SUBOPTIMAL_KHR :: VkResult
pattern VK_SUBOPTIMAL_KHR = VkResult 1000001003
-- | Nothing
pattern VK_ERROR_OUT_OF_DATE_KHR :: VkResult
pattern VK_ERROR_OUT_OF_DATE_KHR = VkResult (-1000001004)
-- | Nothing
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR = VkStructureType 1000001000
-- | Nothing
pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR = VkStructureType 1000001001
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR = VkStructureType 1000060007
-- | Nothing
pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR = VkStructureType 1000060008
-- | Nothing
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR = VkStructureType 1000060009
-- | Nothing
pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR = VkStructureType 1000060010
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR = VkStructureType 1000060011
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR = VkStructureType 1000060012
-- | Just "VkSwapchainKHR"
pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR = VkObjectType 1000001000
-- | Just "Allow images with VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR :: VkSwapchainCreateFlagBitsKHR
pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = VkSwapchainCreateFlagBitsKHR 0x00000001
-- | Just "Swapchain is protected"
pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR :: VkSwapchainCreateFlagBitsKHR
pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR = VkSwapchainCreateFlagBitsKHR 0x00000002
pattern VK_KHR_SWAPCHAIN_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SWAPCHAIN_SPEC_VERSION = 70
pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_swapchain"
-- |
data VkSwapchainKHR_T
type VkSwapchainKHR = Ptr VkSwapchainKHR_T
-- | 
foreign import ccall "vkCreateSwapchainKHR" vkCreateSwapchainKHR :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchain" ::: Ptr VkSwapchainKHR) -> IO VkResult
-- | 
foreign import ccall "vkDestroySwapchainKHR" vkDestroySwapchainKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | 
foreign import ccall "vkGetSwapchainImagesKHR" vkGetSwapchainImagesKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr VkImage) -> IO VkResult
-- | 
foreign import ccall "vkAcquireNextImageKHR" vkAcquireNextImageKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("timeout" ::: Word64) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult
-- | 
foreign import ccall "vkQueuePresentKHR" vkQueuePresentKHR :: ("queue" ::: VkQueue) -> ("pPresentInfo" ::: Ptr VkPresentInfoKHR) -> IO VkResult
-- | 
foreign import ccall "vkGetDeviceGroupPresentCapabilitiesKHR" vkGetDeviceGroupPresentCapabilitiesKHR :: ("device" ::: VkDevice) -> ("pDeviceGroupPresentCapabilities" ::: Ptr VkDeviceGroupPresentCapabilitiesKHR) -> IO VkResult
-- | 
foreign import ccall "vkGetDeviceGroupSurfacePresentModesKHR" vkGetDeviceGroupSurfacePresentModesKHR :: ("device" ::: VkDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult
-- | 
foreign import ccall "vkAcquireNextImage2KHR" vkAcquireNextImage2KHR :: ("device" ::: VkDevice) -> ("pAcquireInfo" ::: Ptr VkAcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult
-- | 
foreign import ccall "vkGetPhysicalDevicePresentRectanglesKHR" vkGetPhysicalDevicePresentRectanglesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult
-- | TODO: Struct comments
data VkSwapchainCreateInfoKHR = VkSwapchainCreateInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkSwapchainCreateFlagsKHR
  , vkSurface :: VkSurfaceKHR
  , vkMinImageCount :: Word32
  , vkImageFormat :: VkFormat
  , vkImageColorSpace :: VkColorSpaceKHR
  , vkImageExtent :: VkExtent2D
  , vkImageArrayLayers :: Word32
  , vkImageUsage :: VkImageUsageFlags
  , vkImageSharingMode :: VkSharingMode
  , vkQueueFamilyIndexCount :: Word32
  , vkQueueFamilyIndices :: Ptr Word32
  , vkPreTransform :: VkSurfaceTransformFlagBitsKHR
  , vkCompositeAlpha :: VkCompositeAlphaFlagBitsKHR
  , vkPresentMode :: VkPresentModeKHR
  , vkClipped :: VkBool32
  , vkOldSwapchain :: VkSwapchainKHR
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSwapchainCreateInfoKHR))
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
                *> poke (ptr `plusPtr` 72) (vkQueueFamilyIndices (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 80) (vkPreTransform (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 84) (vkCompositeAlpha (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 88) (vkPresentMode (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 92) (vkClipped (poked :: VkSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 96) (vkOldSwapchain (poked :: VkSwapchainCreateInfoKHR))
-- | TODO: Struct comments
data VkPresentInfoKHR = VkPresentInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkWaitSemaphoreCount :: Word32
  , vkWaitSemaphores :: Ptr VkSemaphore
  , vkSwapchainCount :: Word32
  , vkSwapchains :: Ptr VkSwapchainKHR
  , vkImageIndices :: Ptr Word32
  , vkResults :: Ptr VkResult
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkWaitSemaphores (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSwapchainCount (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkSwapchains (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkImageIndices (poked :: VkPresentInfoKHR))
                *> poke (ptr `plusPtr` 56) (vkResults (poked :: VkPresentInfoKHR))
-- | TODO: Struct comments
data VkDeviceGroupPresentCapabilitiesKHR = VkDeviceGroupPresentCapabilitiesKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkPresentMask :: Vector VK_MAX_DEVICE_GROUP_SIZE Word32
  , vkModes :: VkDeviceGroupPresentModeFlagsKHR
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceGroupPresentCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkPresentMask (poked :: VkDeviceGroupPresentCapabilitiesKHR))
                *> poke (ptr `plusPtr` 144) (vkModes (poked :: VkDeviceGroupPresentCapabilitiesKHR))
-- | TODO: Struct comments
data VkImageSwapchainCreateInfoKHR = VkImageSwapchainCreateInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSwapchain :: VkSwapchainKHR
  }
  deriving (Eq, Show)

instance Storable VkImageSwapchainCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkImageSwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkImageSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkImageSwapchainCreateInfoKHR))
-- | TODO: Struct comments
data VkBindImageMemorySwapchainInfoKHR = VkBindImageMemorySwapchainInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSwapchain :: VkSwapchainKHR
  , vkImageIndex :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkBindImageMemorySwapchainInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkBindImageMemorySwapchainInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkImageIndex (poked :: VkBindImageMemorySwapchainInfoKHR))
-- | TODO: Struct comments
data VkAcquireNextImageInfoKHR = VkAcquireNextImageInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSwapchain :: VkSwapchainKHR
  , vkTimeout :: Word64
  , vkSemaphore :: VkSemaphore
  , vkFence :: VkFence
  , vkDeviceMask :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchain (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkTimeout (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkSemaphore (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkFence (poked :: VkAcquireNextImageInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkDeviceMask (poked :: VkAcquireNextImageInfoKHR))
-- | TODO: Struct comments
data VkDeviceGroupPresentInfoKHR = VkDeviceGroupPresentInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSwapchainCount :: Word32
  , vkDeviceMasks :: Ptr Word32
  , vkMode :: VkDeviceGroupPresentModeFlagBitsKHR
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceGroupPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSwapchainCount (poked :: VkDeviceGroupPresentInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDeviceMasks (poked :: VkDeviceGroupPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkMode (poked :: VkDeviceGroupPresentInfoKHR))
-- | TODO: Struct comments
data VkDeviceGroupSwapchainCreateInfoKHR = VkDeviceGroupSwapchainCreateInfoKHR
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkModes :: VkDeviceGroupPresentModeFlagsKHR
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupSwapchainCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupSwapchainCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceGroupSwapchainCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkModes (poked :: VkDeviceGroupSwapchainCreateInfoKHR))
type VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagBitsKHR
type VkDeviceGroupPresentModeFlagsKHR = VkDeviceGroupPresentModeFlagBitsKHR
