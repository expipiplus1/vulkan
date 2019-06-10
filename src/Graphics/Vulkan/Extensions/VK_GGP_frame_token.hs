{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_GGP_frame_token
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PresentFrameTokenGGP(..)
  , 
#endif
  pattern GGP_FRAME_TOKEN_EXTENSION_NAME
  , pattern GGP_FRAME_TOKEN_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME
  , pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( GgpFrameToken
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPresentFrameTokenGGP"
data PresentFrameTokenGGP = PresentFrameTokenGGP
  { -- No documentation found for Nested "PresentFrameTokenGGP" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PresentFrameTokenGGP" "frameToken"
  frameToken :: GgpFrameToken
  }
  deriving (Show, Eq)

instance Zero PresentFrameTokenGGP where
  zero = PresentFrameTokenGGP Nothing
                              zero

#endif

-- No documentation found for TopLevel "VK_GGP_FRAME_TOKEN_EXTENSION_NAME"
pattern GGP_FRAME_TOKEN_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern GGP_FRAME_TOKEN_EXTENSION_NAME = VK_GGP_FRAME_TOKEN_EXTENSION_NAME

-- No documentation found for TopLevel "VK_GGP_FRAME_TOKEN_SPEC_VERSION"
pattern GGP_FRAME_TOKEN_SPEC_VERSION :: Integral a => a
pattern GGP_FRAME_TOKEN_SPEC_VERSION = VK_GGP_FRAME_TOKEN_SPEC_VERSION
