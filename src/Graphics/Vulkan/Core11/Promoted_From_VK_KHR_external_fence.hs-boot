{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_fence  (ExportFenceCreateInfo) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ExportFenceCreateInfo

instance ToCStruct ExportFenceCreateInfo
instance Show ExportFenceCreateInfo

instance FromCStruct ExportFenceCreateInfo

