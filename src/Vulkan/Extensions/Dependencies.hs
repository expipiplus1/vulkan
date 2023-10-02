{-# language CPP #-}
-- No documentation found for Chapter "Dependencies"
module Vulkan.Extensions.Dependencies  ( extensionDependencies
                                       , extensionCoreRequirement
                                       ) where

import Data.Word (Word32)
import Data.ByteString (ByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10 (pattern API_VERSION_1_0)
-- | The set of other extensions required to use this extension
extensionDependencies :: ("extensionName" ::: ByteString) -> [ByteString]
extensionDependencies = \case
  _ -> []

-- | The minimum required API version to use this extension
extensionCoreRequirement :: ("extensionName" ::: ByteString) -> Word32
extensionCoreRequirement = \case
  _ -> API_VERSION_1_0
