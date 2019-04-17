{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata
  ( withCStructHdrMetadataEXT
  , fromCStructHdrMetadataEXT
  , HdrMetadataEXT(..)
  , withCStructXYColorEXT
  , fromCStructXYColorEXT
  , XYColorEXT(..)
  , setHdrMetadataEXT
  , pattern VK_EXT_HDR_METADATA_SPEC_VERSION
  , pattern VK_EXT_HDR_METADATA_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( setHdrMetadataEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT(..)
  , VkXYColorEXT(..)
  , pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT
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
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( pattern VK_EXT_HDR_METADATA_EXTENSION_NAME
  , pattern VK_EXT_HDR_METADATA_SPEC_VERSION
  )


-- No documentation found for TopLevel "HdrMetadataEXT"
data HdrMetadataEXT = HdrMetadataEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "HdrMetadataEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "HdrMetadataEXT" "displayPrimaryRed"
  vkDisplayPrimaryRed :: XYColorEXT
  , -- No documentation found for Nested "HdrMetadataEXT" "displayPrimaryGreen"
  vkDisplayPrimaryGreen :: XYColorEXT
  , -- No documentation found for Nested "HdrMetadataEXT" "displayPrimaryBlue"
  vkDisplayPrimaryBlue :: XYColorEXT
  , -- No documentation found for Nested "HdrMetadataEXT" "whitePoint"
  vkWhitePoint :: XYColorEXT
  , -- No documentation found for Nested "HdrMetadataEXT" "maxLuminance"
  vkMaxLuminance :: CFloat
  , -- No documentation found for Nested "HdrMetadataEXT" "minLuminance"
  vkMinLuminance :: CFloat
  , -- No documentation found for Nested "HdrMetadataEXT" "maxContentLightLevel"
  vkMaxContentLightLevel :: CFloat
  , -- No documentation found for Nested "HdrMetadataEXT" "maxFrameAverageLightLevel"
  vkMaxFrameAverageLightLevel :: CFloat
  }
  deriving (Show, Eq)
withCStructHdrMetadataEXT :: HdrMetadataEXT -> (VkHdrMetadataEXT -> IO a) -> IO a
withCStructHdrMetadataEXT from cont = withCStructXYColorEXT (vkWhitePoint (from :: HdrMetadataEXT)) (\whitePoint -> withCStructXYColorEXT (vkDisplayPrimaryBlue (from :: HdrMetadataEXT)) (\displayPrimaryBlue -> withCStructXYColorEXT (vkDisplayPrimaryGreen (from :: HdrMetadataEXT)) (\displayPrimaryGreen -> withCStructXYColorEXT (vkDisplayPrimaryRed (from :: HdrMetadataEXT)) (\displayPrimaryRed -> maybeWith withSomeVkStruct (vkPNext (from :: HdrMetadataEXT)) (\pPNext -> cont (VkHdrMetadataEXT VK_STRUCTURE_TYPE_HDR_METADATA_EXT pPNext displayPrimaryRed displayPrimaryGreen displayPrimaryBlue whitePoint (vkMaxLuminance (from :: HdrMetadataEXT)) (vkMinLuminance (from :: HdrMetadataEXT)) (vkMaxContentLightLevel (from :: HdrMetadataEXT)) (vkMaxFrameAverageLightLevel (from :: HdrMetadataEXT))))))))
fromCStructHdrMetadataEXT :: VkHdrMetadataEXT -> IO HdrMetadataEXT
fromCStructHdrMetadataEXT c = HdrMetadataEXT <$> -- Univalued Member elided
                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkHdrMetadataEXT)))
                                             <*> (fromCStructXYColorEXT (vkDisplayPrimaryRed (c :: VkHdrMetadataEXT)))
                                             <*> (fromCStructXYColorEXT (vkDisplayPrimaryGreen (c :: VkHdrMetadataEXT)))
                                             <*> (fromCStructXYColorEXT (vkDisplayPrimaryBlue (c :: VkHdrMetadataEXT)))
                                             <*> (fromCStructXYColorEXT (vkWhitePoint (c :: VkHdrMetadataEXT)))
                                             <*> pure (vkMaxLuminance (c :: VkHdrMetadataEXT))
                                             <*> pure (vkMinLuminance (c :: VkHdrMetadataEXT))
                                             <*> pure (vkMaxContentLightLevel (c :: VkHdrMetadataEXT))
                                             <*> pure (vkMaxFrameAverageLightLevel (c :: VkHdrMetadataEXT))
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
-- No documentation found for TopLevel "XYColorEXT"
data XYColorEXT = XYColorEXT
  { -- No documentation found for Nested "XYColorEXT" "x"
  vkX :: CFloat
  , -- No documentation found for Nested "XYColorEXT" "y"
  vkY :: CFloat
  }
  deriving (Show, Eq)
withCStructXYColorEXT :: XYColorEXT -> (VkXYColorEXT -> IO a) -> IO a
withCStructXYColorEXT from cont = cont (VkXYColorEXT (vkX (from :: XYColorEXT)) (vkY (from :: XYColorEXT)))
fromCStructXYColorEXT :: VkXYColorEXT -> IO XYColorEXT
fromCStructXYColorEXT c = XYColorEXT <$> pure (vkX (c :: VkXYColorEXT))
                                     <*> pure (vkY (c :: VkXYColorEXT))
instance Zero XYColorEXT where
  zero = XYColorEXT zero
                    zero

-- | Wrapper for 'vkSetHdrMetadataEXT'
setHdrMetadataEXT :: Device ->  Vector SwapchainKHR ->  Vector HdrMetadataEXT ->  IO ()
setHdrMetadataEXT = \(Device device commandTable) -> \swapchains -> \metadata -> withVec withCStructHdrMetadataEXT metadata (\pMetadata -> withVec (&) swapchains (\pSwapchains -> Graphics.Vulkan.C.Dynamic.setHdrMetadataEXT commandTable device (fromIntegral $ Data.Vector.length swapchains `min` Data.Vector.length metadata) pSwapchains pMetadata *> (pure ())))
