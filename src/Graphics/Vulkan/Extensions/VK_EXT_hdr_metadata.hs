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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT(..)
  , VkXYColorEXT(..)
  , vkSetHdrMetadataEXT
  , pattern VK_EXT_HDR_METADATA_EXTENSION_NAME
  , pattern VK_EXT_HDR_METADATA_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_HDR_METADATA_EXT
  )



-- | VkHdrMetadataEXT - structure to specify Hdr metadata
--
-- == Valid Usage (Implicit)
--
-- __Note__
--
-- The validity and use of this data is outside the scope of Vulkan.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata.VkXYColorEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata.vkSetHdrMetadataEXT'
data HdrMetadataEXT = HdrMetadataEXT
  { -- Univalued member elided
  -- No documentation found for Nested "HdrMetadataEXT" "pNext"
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
  maxLuminance :: CFloat
  , -- No documentation found for Nested "HdrMetadataEXT" "minLuminance"
  minLuminance :: CFloat
  , -- No documentation found for Nested "HdrMetadataEXT" "maxContentLightLevel"
  maxContentLightLevel :: CFloat
  , -- No documentation found for Nested "HdrMetadataEXT" "maxFrameAverageLightLevel"
  maxFrameAverageLightLevel :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkHdrMetadataEXT' and
-- marshal a 'HdrMetadataEXT' into it. The 'VkHdrMetadataEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructHdrMetadataEXT :: HdrMetadataEXT -> (VkHdrMetadataEXT -> IO a) -> IO a
withCStructHdrMetadataEXT marshalled cont = withCStructXYColorEXT (whitePoint (marshalled :: HdrMetadataEXT)) (\whitePoint'' -> withCStructXYColorEXT (displayPrimaryBlue (marshalled :: HdrMetadataEXT)) (\displayPrimaryBlue'' -> withCStructXYColorEXT (displayPrimaryGreen (marshalled :: HdrMetadataEXT)) (\displayPrimaryGreen'' -> withCStructXYColorEXT (displayPrimaryRed (marshalled :: HdrMetadataEXT)) (\displayPrimaryRed'' -> maybeWith withSomeVkStruct (next (marshalled :: HdrMetadataEXT)) (\pPNext -> cont (VkHdrMetadataEXT VK_STRUCTURE_TYPE_HDR_METADATA_EXT pPNext displayPrimaryRed'' displayPrimaryGreen'' displayPrimaryBlue'' whitePoint'' (maxLuminance (marshalled :: HdrMetadataEXT)) (minLuminance (marshalled :: HdrMetadataEXT)) (maxContentLightLevel (marshalled :: HdrMetadataEXT)) (maxFrameAverageLightLevel (marshalled :: HdrMetadataEXT))))))))

-- | A function to read a 'VkHdrMetadataEXT' and all additional
-- structures in the pointer chain into a 'HdrMetadataEXT'.
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



-- | VkXYColorEXT - structure to specify X,Y chromaticity coordinates
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata.VkHdrMetadataEXT'
data XYColorEXT = XYColorEXT
  { -- No documentation found for Nested "XYColorEXT" "x"
  x :: CFloat
  , -- No documentation found for Nested "XYColorEXT" "y"
  y :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkXYColorEXT' and
-- marshal a 'XYColorEXT' into it. The 'VkXYColorEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructXYColorEXT :: XYColorEXT -> (VkXYColorEXT -> IO a) -> IO a
withCStructXYColorEXT marshalled cont = cont (VkXYColorEXT (x (marshalled :: XYColorEXT)) (y (marshalled :: XYColorEXT)))

-- | A function to read a 'VkXYColorEXT' and all additional
-- structures in the pointer chain into a 'XYColorEXT'.
fromCStructXYColorEXT :: VkXYColorEXT -> IO XYColorEXT
fromCStructXYColorEXT c = XYColorEXT <$> pure (vkX (c :: VkXYColorEXT))
                                     <*> pure (vkY (c :: VkXYColorEXT))

instance Zero XYColorEXT where
  zero = XYColorEXT zero
                    zero



-- | vkSetHdrMetadataEXT - function to set Hdr metadata
--
-- = Parameters
--
-- -   @device@ is the logical device where the swapchain(s) were created.
--
-- -   @swapchainCount@ is the number of swapchains included in
--     @pSwapchains@.
--
-- -   @pSwapchains@ is a pointer to the array of @swapchainCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handles.
--
-- -   @pMetadata@ is a pointer to the array of @swapchainCount@
--     'Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata.VkHdrMetadataEXT'
--     structures.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pSwapchains@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handles
--
-- -   @pMetadata@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata.VkHdrMetadataEXT'
--     structures
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- -   Both of @device@, and the elements of @pSwapchains@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata.VkHdrMetadataEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
setHdrMetadataEXT :: Device ->  Vector SwapchainKHR ->  Vector HdrMetadataEXT ->  IO ()
setHdrMetadataEXT = \(Device device' commandTable) -> \swapchains' -> \metadata' -> withVec withCStructHdrMetadataEXT metadata' (\pMetadata' -> withVec (&) swapchains' (\pSwapchains' -> vkSetHdrMetadataEXT commandTable device' (fromIntegral $ Data.Vector.length swapchains' `min` Data.Vector.length metadata') pSwapchains' pMetadata' *> (pure ())))

-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_EXTENSION_NAME"
pattern EXT_HDR_METADATA_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_HDR_METADATA_EXTENSION_NAME = VK_EXT_HDR_METADATA_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_HDR_METADATA_SPEC_VERSION"
pattern EXT_HDR_METADATA_SPEC_VERSION :: Integral a => a
pattern EXT_HDR_METADATA_SPEC_VERSION = VK_EXT_HDR_METADATA_SPEC_VERSION
