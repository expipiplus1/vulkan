{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_queue_family_foreign
  ( pattern VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME
  , pattern VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION
  , pattern VK_QUEUE_FAMILY_FOREIGN_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )





-- No documentation found for TopLevel "VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME"
pattern VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME = "VK_EXT_queue_family_foreign"

-- No documentation found for TopLevel "VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION"
pattern VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION :: Integral a => a
pattern VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION = 1

-- No documentation found for Nested "Word32" "VK_QUEUE_FAMILY_FOREIGN_EXT"
pattern VK_QUEUE_FAMILY_FOREIGN_EXT :: Word32
pattern VK_QUEUE_FAMILY_FOREIGN_EXT = 0xfffffffd
