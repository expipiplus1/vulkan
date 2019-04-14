{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT(..)
  , VkXYColorEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkSetHdrMetadataEXT
#endif
  , FN_vkSetHdrMetadataEXT
  , PFN_vkSetHdrMetadataEXT
  , pattern VK_EXT_HDR_METADATA_EXTENSION_NAME
  , pattern VK_EXT_HDR_METADATA_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
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
  ( VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkHdrMetadataEXT"
data VkHdrMetadataEXT = VkHdrMetadataEXT
  { -- No documentation found for Nested "VkHdrMetadataEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkHdrMetadataEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkHdrMetadataEXT" "displayPrimaryRed"
  vkDisplayPrimaryRed :: VkXYColorEXT
  , -- No documentation found for Nested "VkHdrMetadataEXT" "displayPrimaryGreen"
  vkDisplayPrimaryGreen :: VkXYColorEXT
  , -- No documentation found for Nested "VkHdrMetadataEXT" "displayPrimaryBlue"
  vkDisplayPrimaryBlue :: VkXYColorEXT
  , -- No documentation found for Nested "VkHdrMetadataEXT" "whitePoint"
  vkWhitePoint :: VkXYColorEXT
  , -- No documentation found for Nested "VkHdrMetadataEXT" "maxLuminance"
  vkMaxLuminance :: CFloat
  , -- No documentation found for Nested "VkHdrMetadataEXT" "minLuminance"
  vkMinLuminance :: CFloat
  , -- No documentation found for Nested "VkHdrMetadataEXT" "maxContentLightLevel"
  vkMaxContentLightLevel :: CFloat
  , -- No documentation found for Nested "VkHdrMetadataEXT" "maxFrameAverageLightLevel"
  vkMaxFrameAverageLightLevel :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkHdrMetadataEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkHdrMetadataEXT <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 8)
                              <*> peek (ptr `plusPtr` 16)
                              <*> peek (ptr `plusPtr` 24)
                              <*> peek (ptr `plusPtr` 32)
                              <*> peek (ptr `plusPtr` 40)
                              <*> peek (ptr `plusPtr` 48)
                              <*> peek (ptr `plusPtr` 52)
                              <*> peek (ptr `plusPtr` 56)
                              <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 16) (vkDisplayPrimaryRed (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 24) (vkDisplayPrimaryGreen (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 32) (vkDisplayPrimaryBlue (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 40) (vkWhitePoint (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 48) (vkMaxLuminance (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 52) (vkMinLuminance (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 56) (vkMaxContentLightLevel (poked :: VkHdrMetadataEXT))
                *> poke (ptr `plusPtr` 60) (vkMaxFrameAverageLightLevel (poked :: VkHdrMetadataEXT))
-- No documentation found for TopLevel "VkXYColorEXT"
data VkXYColorEXT = VkXYColorEXT
  { -- No documentation found for Nested "VkXYColorEXT" "x"
  vkX :: CFloat
  , -- No documentation found for Nested "VkXYColorEXT" "y"
  vkY :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkXYColorEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkXYColorEXT <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkXYColorEXT))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkXYColorEXT))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkSetHdrMetadataEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetHdrMetadataEXT" vkSetHdrMetadataEXT :: ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> ("pMetadata" ::: Ptr VkHdrMetadataEXT) -> IO ()

#endif
type FN_vkSetHdrMetadataEXT = ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> ("pMetadata" ::: Ptr VkHdrMetadataEXT) -> IO ()
type PFN_vkSetHdrMetadataEXT = FunPtr FN_vkSetHdrMetadataEXT
-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_EXTENSION_NAME"
pattern VK_EXT_HDR_METADATA_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_HDR_METADATA_EXTENSION_NAME = "VK_EXT_hdr_metadata"
-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_SPEC_VERSION"
pattern VK_EXT_HDR_METADATA_SPEC_VERSION :: Integral a => a
pattern VK_EXT_HDR_METADATA_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_HDR_METADATA_EXT"
pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT = VkStructureType 1000105000
