{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_queue_family_foreign
  ( pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME
  , pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_queue_family_foreign
  ( pattern VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME
  , pattern VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME"
pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME = VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION"
pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION :: Integral a => a
pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION = VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION
