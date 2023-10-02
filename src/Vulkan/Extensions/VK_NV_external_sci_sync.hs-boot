{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_external_sci_sync"
module Vulkan.Extensions.VK_NV_external_sci_sync  ( ExportFenceSciSyncInfoNV
                                                  , ExportSemaphoreSciSyncInfoNV
                                                  , FenceGetSciSyncInfoNV
                                                  , ImportFenceSciSyncInfoNV
                                                  , ImportSemaphoreSciSyncInfoNV
                                                  , PhysicalDeviceExternalSciSyncFeaturesNV
                                                  , SciSyncAttributesInfoNV
                                                  , SemaphoreGetSciSyncInfoNV
                                                  , NvSciSyncAttrList
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)

data ExportFenceSciSyncInfoNV

instance ToCStruct ExportFenceSciSyncInfoNV
instance Show ExportFenceSciSyncInfoNV

instance FromCStruct ExportFenceSciSyncInfoNV


data ExportSemaphoreSciSyncInfoNV

instance ToCStruct ExportSemaphoreSciSyncInfoNV
instance Show ExportSemaphoreSciSyncInfoNV

instance FromCStruct ExportSemaphoreSciSyncInfoNV


data FenceGetSciSyncInfoNV

instance ToCStruct FenceGetSciSyncInfoNV
instance Show FenceGetSciSyncInfoNV

instance FromCStruct FenceGetSciSyncInfoNV


data ImportFenceSciSyncInfoNV

instance ToCStruct ImportFenceSciSyncInfoNV
instance Show ImportFenceSciSyncInfoNV

instance FromCStruct ImportFenceSciSyncInfoNV


data ImportSemaphoreSciSyncInfoNV

instance ToCStruct ImportSemaphoreSciSyncInfoNV
instance Show ImportSemaphoreSciSyncInfoNV

instance FromCStruct ImportSemaphoreSciSyncInfoNV


data PhysicalDeviceExternalSciSyncFeaturesNV

instance ToCStruct PhysicalDeviceExternalSciSyncFeaturesNV
instance Show PhysicalDeviceExternalSciSyncFeaturesNV

instance FromCStruct PhysicalDeviceExternalSciSyncFeaturesNV


data SciSyncAttributesInfoNV

instance ToCStruct SciSyncAttributesInfoNV
instance Show SciSyncAttributesInfoNV

instance FromCStruct SciSyncAttributesInfoNV


data SemaphoreGetSciSyncInfoNV

instance ToCStruct SemaphoreGetSciSyncInfoNV
instance Show SemaphoreGetSciSyncInfoNV

instance FromCStruct SemaphoreGetSciSyncInfoNV


type NvSciSyncAttrList = Ptr ()

