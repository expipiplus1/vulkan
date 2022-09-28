{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_semaphore_capabilities"
module Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities  ( ExternalSemaphoreProperties
                                                                           , PhysicalDeviceExternalSemaphoreInfo
                                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data ExternalSemaphoreProperties

instance ToCStruct ExternalSemaphoreProperties
instance Show ExternalSemaphoreProperties

instance FromCStruct ExternalSemaphoreProperties


type role PhysicalDeviceExternalSemaphoreInfo nominal
data PhysicalDeviceExternalSemaphoreInfo (es :: [Type])

instance ( Extendss PhysicalDeviceExternalSemaphoreInfo es
         , PokeChain es ) => ToCStruct (PhysicalDeviceExternalSemaphoreInfo es)
instance Show (Chain es) => Show (PhysicalDeviceExternalSemaphoreInfo es)

instance ( Extendss PhysicalDeviceExternalSemaphoreInfo es
         , PeekChain es ) => FromCStruct (PhysicalDeviceExternalSemaphoreInfo es)

