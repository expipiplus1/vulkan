{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_queue_family_foreign  ( EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION
                                                      , pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION
                                                      , EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME
                                                      , pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME
                                                      , QUEUE_FAMILY_FOREIGN_EXT
                                                      , pattern QUEUE_FAMILY_FOREIGN_EXT
                                                      ) where

import Data.String (IsString)
import Vulkan.Core10.APIConstants (QUEUE_FAMILY_FOREIGN_EXT)
import Vulkan.Core10.APIConstants (pattern QUEUE_FAMILY_FOREIGN_EXT)
type EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION"
pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_QUEUE_FAMILY_FOREIGN_SPEC_VERSION = 1


type EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME = "VK_EXT_queue_family_foreign"

-- No documentation found for TopLevel "VK_EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME"
pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_QUEUE_FAMILY_FOREIGN_EXTENSION_NAME = "VK_EXT_queue_family_foreign"

