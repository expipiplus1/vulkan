{-# language CPP #-}
module Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities  ( ExternalSemaphoreProperties
                                                                           , PhysicalDeviceExternalSemaphoreInfo
                                                                           ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
data ExternalSemaphoreProperties

instance ToCStruct ExternalSemaphoreProperties
instance Show ExternalSemaphoreProperties

instance FromCStruct ExternalSemaphoreProperties


type role PhysicalDeviceExternalSemaphoreInfo nominal
data PhysicalDeviceExternalSemaphoreInfo (es :: [Type])

instance PokeChain es => ToCStruct (PhysicalDeviceExternalSemaphoreInfo es)
instance Show (Chain es) => Show (PhysicalDeviceExternalSemaphoreInfo es)

instance PeekChain es => FromCStruct (PhysicalDeviceExternalSemaphoreInfo es)

