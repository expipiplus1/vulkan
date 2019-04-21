{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( GgpFrameToken
  , VkPresentFrameTokenGGP(..)
  , pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME
  , pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  )


-- No documentation found for TopLevel "GgpFrameToken"
type GgpFrameToken = Word32
  

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
data VkPresentFrameTokenGGP = VkPresentFrameTokenGGP
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @frameToken@ /must/ be a valid 'GgpFrameToken'
  vkFrameToken :: GgpFrameToken
  }
  deriving (Eq, Show)

instance Storable VkPresentFrameTokenGGP where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPresentFrameTokenGGP <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPresentFrameTokenGGP))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPresentFrameTokenGGP))
                *> poke (ptr `plusPtr` 16) (vkFrameToken (poked :: VkPresentFrameTokenGGP))

instance Zero VkPresentFrameTokenGGP where
  zero = VkPresentFrameTokenGGP VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
                                zero
                                zero

-- No documentation found for TopLevel "VK_GGP_FRAME_TOKEN_EXTENSION_NAME"
pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_GGP_FRAME_TOKEN_EXTENSION_NAME = "VK_GGP_frame_token"

-- No documentation found for TopLevel "VK_GGP_FRAME_TOKEN_SPEC_VERSION"
pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION :: Integral a => a
pattern VK_GGP_FRAME_TOKEN_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP"
pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP :: VkStructureType
pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP = VkStructureType 1000191000
