{-# language CPP #-}
-- No documentation found for Chapter "VK_ANDROID_external_memory_android_hardware_buffer"
module Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer  ( AndroidHardwareBufferFormatPropertiesANDROID
                                                                             , AndroidHardwareBufferPropertiesANDROID
                                                                             , AndroidHardwareBufferUsageANDROID
                                                                             , ExternalFormatANDROID
                                                                             , ImportAndroidHardwareBufferInfoANDROID
                                                                             , MemoryGetAndroidHardwareBufferInfoANDROID
                                                                             , AHardwareBuffer
                                                                             ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
data AndroidHardwareBufferFormatPropertiesANDROID

instance ToCStruct AndroidHardwareBufferFormatPropertiesANDROID
instance Show AndroidHardwareBufferFormatPropertiesANDROID

instance FromCStruct AndroidHardwareBufferFormatPropertiesANDROID


type role AndroidHardwareBufferPropertiesANDROID nominal
data AndroidHardwareBufferPropertiesANDROID (es :: [Type])

instance (Extendss AndroidHardwareBufferPropertiesANDROID es, PokeChain es) => ToCStruct (AndroidHardwareBufferPropertiesANDROID es)
instance Show (Chain es) => Show (AndroidHardwareBufferPropertiesANDROID es)

instance (Extendss AndroidHardwareBufferPropertiesANDROID es, PeekChain es) => FromCStruct (AndroidHardwareBufferPropertiesANDROID es)


data AndroidHardwareBufferUsageANDROID

instance ToCStruct AndroidHardwareBufferUsageANDROID
instance Show AndroidHardwareBufferUsageANDROID

instance FromCStruct AndroidHardwareBufferUsageANDROID


data ExternalFormatANDROID

instance ToCStruct ExternalFormatANDROID
instance Show ExternalFormatANDROID

instance FromCStruct ExternalFormatANDROID


data ImportAndroidHardwareBufferInfoANDROID

instance ToCStruct ImportAndroidHardwareBufferInfoANDROID
instance Show ImportAndroidHardwareBufferInfoANDROID

instance FromCStruct ImportAndroidHardwareBufferInfoANDROID


data MemoryGetAndroidHardwareBufferInfoANDROID

instance ToCStruct MemoryGetAndroidHardwareBufferInfoANDROID
instance Show MemoryGetAndroidHardwareBufferInfoANDROID

instance FromCStruct MemoryGetAndroidHardwareBufferInfoANDROID


data AHardwareBuffer

