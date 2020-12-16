{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_fence"
module Vulkan.Core11.Promoted_From_VK_KHR_external_fence  (ExportFenceCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExportFenceCreateInfo

instance ToCStruct ExportFenceCreateInfo
instance Show ExportFenceCreateInfo

instance FromCStruct ExportFenceCreateInfo

