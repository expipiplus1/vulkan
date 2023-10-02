{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_external_sci_sync2"
module Vulkan.Extensions.VK_NV_external_sci_sync2  ( DeviceSemaphoreSciSyncPoolReservationCreateInfoNV
                                                   , PhysicalDeviceExternalSciSync2FeaturesNV
                                                   , SemaphoreSciSyncCreateInfoNV
                                                   , SemaphoreSciSyncPoolCreateInfoNV
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceSemaphoreSciSyncPoolReservationCreateInfoNV

instance ToCStruct DeviceSemaphoreSciSyncPoolReservationCreateInfoNV
instance Show DeviceSemaphoreSciSyncPoolReservationCreateInfoNV

instance FromCStruct DeviceSemaphoreSciSyncPoolReservationCreateInfoNV


data PhysicalDeviceExternalSciSync2FeaturesNV

instance ToCStruct PhysicalDeviceExternalSciSync2FeaturesNV
instance Show PhysicalDeviceExternalSciSync2FeaturesNV

instance FromCStruct PhysicalDeviceExternalSciSync2FeaturesNV


data SemaphoreSciSyncCreateInfoNV

instance ToCStruct SemaphoreSciSyncCreateInfoNV
instance Show SemaphoreSciSyncCreateInfoNV

instance FromCStruct SemaphoreSciSyncCreateInfoNV


data SemaphoreSciSyncPoolCreateInfoNV

instance ToCStruct SemaphoreSciSyncPoolCreateInfoNV
instance Show SemaphoreSciSyncPoolCreateInfoNV

instance FromCStruct SemaphoreSciSyncPoolCreateInfoNV

