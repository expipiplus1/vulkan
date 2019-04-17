{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_GGP_frame_token
  ( withCStructPresentFrameTokenGGP
  , fromCStructPresentFrameTokenGGP
  , PresentFrameTokenGGP(..)
  , pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION
  , pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
  ) where

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
import Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( VkPresentFrameTokenGGP(..)
  , GgpFrameToken
  , pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME
  , pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION
  )


-- No documentation found for TopLevel "PresentFrameTokenGGP"
data PresentFrameTokenGGP = PresentFrameTokenGGP
  { -- Univalued Member elided
  -- No documentation found for Nested "PresentFrameTokenGGP" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PresentFrameTokenGGP" "frameToken"
  vkFrameToken :: GgpFrameToken
  }
  deriving (Show, Eq)
withCStructPresentFrameTokenGGP :: PresentFrameTokenGGP -> (VkPresentFrameTokenGGP -> IO a) -> IO a
withCStructPresentFrameTokenGGP from cont = maybeWith withSomeVkStruct (vkPNext (from :: PresentFrameTokenGGP)) (\pPNext -> cont (VkPresentFrameTokenGGP VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP pPNext (vkFrameToken (from :: PresentFrameTokenGGP))))
fromCStructPresentFrameTokenGGP :: VkPresentFrameTokenGGP -> IO PresentFrameTokenGGP
fromCStructPresentFrameTokenGGP c = PresentFrameTokenGGP <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPresentFrameTokenGGP)))
                                                         <*> pure (vkFrameToken (c :: VkPresentFrameTokenGGP))
instance Zero PresentFrameTokenGGP where
  zero = PresentFrameTokenGGP Nothing
                              zero
