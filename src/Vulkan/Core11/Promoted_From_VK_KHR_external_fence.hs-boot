{-# language CPP #-}
module Vulkan.Core11.Promoted_From_VK_KHR_external_fence  (ExportFenceCreateInfo) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExportFenceCreateInfo

instance ToCStruct ExportFenceCreateInfo
instance Show ExportFenceCreateInfo

instance FromCStruct ExportFenceCreateInfo

