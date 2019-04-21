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



-- | VkPresentFrameTokenGGP - The Google Games Platform frame token
--
-- == Valid Usage
--
-- Unresolved directive in VkPresentFrameTokenGGP.txt -
-- include::{generated}\/validity\/structs\/VkPresentFrameTokenGGP.txt[]
--
-- = See Also
--
-- No cross-references are available
data PresentFrameTokenGGP = PresentFrameTokenGGP
  { -- Univalued member elided
  -- No documentation found for Nested "PresentFrameTokenGGP" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PresentFrameTokenGGP" "frameToken"
  frameToken :: GgpFrameToken
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPresentFrameTokenGGP' and
-- marshal a 'PresentFrameTokenGGP' into it. The 'VkPresentFrameTokenGGP' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPresentFrameTokenGGP :: PresentFrameTokenGGP -> (VkPresentFrameTokenGGP -> IO a) -> IO a
withCStructPresentFrameTokenGGP marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PresentFrameTokenGGP)) (\pPNext -> cont (VkPresentFrameTokenGGP VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP pPNext (frameToken (marshalled :: PresentFrameTokenGGP))))

-- | A function to read a 'VkPresentFrameTokenGGP' and all additional
-- structures in the pointer chain into a 'PresentFrameTokenGGP'.
fromCStructPresentFrameTokenGGP :: VkPresentFrameTokenGGP -> IO PresentFrameTokenGGP
fromCStructPresentFrameTokenGGP c = PresentFrameTokenGGP <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPresentFrameTokenGGP)))
                                                         <*> pure (vkFrameToken (c :: VkPresentFrameTokenGGP))

instance Zero PresentFrameTokenGGP where
  zero = PresentFrameTokenGGP Nothing
                              zero

