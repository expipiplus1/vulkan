{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_external_memory_sci_buf"
module Vulkan.Extensions.VK_NV_external_memory_sci_buf  ( ExportMemorySciBufInfoNV
                                                        , ImportMemorySciBufInfoNV
                                                        , MemoryGetSciBufInfoNV
                                                        , MemorySciBufPropertiesNV
                                                        , PhysicalDeviceExternalMemorySciBufFeaturesNV
                                                        , NvSciBufAttrList
                                                        , NvSciBufObj
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)

data ExportMemorySciBufInfoNV

instance ToCStruct ExportMemorySciBufInfoNV
instance Show ExportMemorySciBufInfoNV

instance FromCStruct ExportMemorySciBufInfoNV


data ImportMemorySciBufInfoNV

instance ToCStruct ImportMemorySciBufInfoNV
instance Show ImportMemorySciBufInfoNV

instance FromCStruct ImportMemorySciBufInfoNV


data MemoryGetSciBufInfoNV

instance ToCStruct MemoryGetSciBufInfoNV
instance Show MemoryGetSciBufInfoNV

instance FromCStruct MemoryGetSciBufInfoNV


data MemorySciBufPropertiesNV

instance ToCStruct MemorySciBufPropertiesNV
instance Show MemorySciBufPropertiesNV

instance FromCStruct MemorySciBufPropertiesNV


data PhysicalDeviceExternalMemorySciBufFeaturesNV

instance ToCStruct PhysicalDeviceExternalMemorySciBufFeaturesNV
instance Show PhysicalDeviceExternalMemorySciBufFeaturesNV

instance FromCStruct PhysicalDeviceExternalMemorySciBufFeaturesNV


type NvSciBufAttrList = Ptr ()


type NvSciBufObj = Ptr ()

