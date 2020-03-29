{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities  ( ExternalSemaphoreProperties
                                                                                    , PhysicalDeviceExternalSemaphoreInfo
                                                                                    ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data ExternalSemaphoreProperties

instance ToCStruct ExternalSemaphoreProperties
instance Show ExternalSemaphoreProperties

instance FromCStruct ExternalSemaphoreProperties


type role PhysicalDeviceExternalSemaphoreInfo nominal
data PhysicalDeviceExternalSemaphoreInfo (es :: [Type])

instance PokeChain es => ToCStruct (PhysicalDeviceExternalSemaphoreInfo es)
instance Show (Chain es) => Show (PhysicalDeviceExternalSemaphoreInfo es)

instance PeekChain es => FromCStruct (PhysicalDeviceExternalSemaphoreInfo es)

