{-# language CPP #-}
module Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore  (ExportSemaphoreCreateInfo) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExportSemaphoreCreateInfo

instance ToCStruct ExportSemaphoreCreateInfo
instance Show ExportSemaphoreCreateInfo

instance FromCStruct ExportSemaphoreCreateInfo

