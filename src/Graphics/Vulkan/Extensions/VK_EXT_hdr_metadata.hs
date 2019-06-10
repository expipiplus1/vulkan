{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  HdrMetadataEXT(..)
  , 
#endif
  XYColorEXT(..)
  , setHdrMetadataEXT
  , pattern EXT_HDR_METADATA_EXTENSION_NAME
  , pattern EXT_HDR_METADATA_SPEC_VERSION
  , pattern STRUCTURE_TYPE_HDR_METADATA_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( vkSetHdrMetadataEXT
  , pattern VK_EXT_HDR_METADATA_EXTENSION_NAME
  , pattern VK_EXT_HDR_METADATA_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_HDR_METADATA_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkHdrMetadataEXT"
data HdrMetadataEXT = HdrMetadataEXT
  { -- No documentation found for Nested "HdrMetadataEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "HdrMetadataEXT" "displayPrimaryRed"
  displayPrimaryRed :: XYColorEXT
  , -- No documentation found for Nested "HdrMetadataEXT" "displayPrimaryGreen"
  displayPrimaryGreen :: XYColorEXT
  , -- No documentation found for Nested "HdrMetadataEXT" "displayPrimaryBlue"
  displayPrimaryBlue :: XYColorEXT
  , -- No documentation found for Nested "HdrMetadataEXT" "whitePoint"
  whitePoint :: XYColorEXT
  , -- No documentation found for Nested "HdrMetadataEXT" "maxLuminance"
  maxLuminance :: Float
  , -- No documentation found for Nested "HdrMetadataEXT" "minLuminance"
  minLuminance :: Float
  , -- No documentation found for Nested "HdrMetadataEXT" "maxContentLightLevel"
  maxContentLightLevel :: Float
  , -- No documentation found for Nested "HdrMetadataEXT" "maxFrameAverageLightLevel"
  maxFrameAverageLightLevel :: Float
  }
  deriving (Show, Eq)

instance Zero HdrMetadataEXT where
  zero = HdrMetadataEXT Nothing
                        zero
                        zero
                        zero
                        zero
                        zero
                        zero
                        zero
                        zero

#endif


-- No documentation found for TopLevel "VkXYColorEXT"
data XYColorEXT = XYColorEXT
  { -- No documentation found for Nested "XYColorEXT" "x"
  x :: Float
  , -- No documentation found for Nested "XYColorEXT" "y"
  y :: Float
  }
  deriving (Show, Eq)

instance Zero XYColorEXT where
  zero = XYColorEXT zero
                    zero



-- No documentation found for TopLevel "vkSetHdrMetadataEXT"
setHdrMetadataEXT :: Device ->  Vector SwapchainKHR ->  Vector HdrMetadataEXT ->  IO ()
setHdrMetadataEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_EXTENSION_NAME"
pattern EXT_HDR_METADATA_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_HDR_METADATA_EXTENSION_NAME = VK_EXT_HDR_METADATA_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_SPEC_VERSION"
pattern EXT_HDR_METADATA_SPEC_VERSION :: Integral a => a
pattern EXT_HDR_METADATA_SPEC_VERSION = VK_EXT_HDR_METADATA_SPEC_VERSION
