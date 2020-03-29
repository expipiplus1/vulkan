{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer  ( AndroidHardwareBufferFormatPropertiesANDROID
                                                                                      , AndroidHardwareBufferPropertiesANDROID
                                                                                      , AndroidHardwareBufferUsageANDROID
                                                                                      , ExternalFormatANDROID
                                                                                      , ImportAndroidHardwareBufferInfoANDROID
                                                                                      , MemoryGetAndroidHardwareBufferInfoANDROID
                                                                                      ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data AndroidHardwareBufferFormatPropertiesANDROID

instance ToCStruct AndroidHardwareBufferFormatPropertiesANDROID
instance Show AndroidHardwareBufferFormatPropertiesANDROID

instance FromCStruct AndroidHardwareBufferFormatPropertiesANDROID


type role AndroidHardwareBufferPropertiesANDROID nominal
data AndroidHardwareBufferPropertiesANDROID (es :: [Type])

instance PokeChain es => ToCStruct (AndroidHardwareBufferPropertiesANDROID es)
instance Show (Chain es) => Show (AndroidHardwareBufferPropertiesANDROID es)

instance PeekChain es => FromCStruct (AndroidHardwareBufferPropertiesANDROID es)


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

