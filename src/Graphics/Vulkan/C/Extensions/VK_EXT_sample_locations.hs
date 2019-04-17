{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( VkAttachmentSampleLocationsEXT(..)
  , VkMultisamplePropertiesEXT(..)
  , VkPhysicalDeviceSampleLocationsPropertiesEXT(..)
  , VkPipelineSampleLocationsStateCreateInfoEXT(..)
  , VkRenderPassSampleLocationsBeginInfoEXT(..)
  , VkSampleLocationEXT(..)
  , VkSampleLocationsInfoEXT(..)
  , VkSubpassSampleLocationsEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdSetSampleLocationsEXT
#endif
  , FN_vkCmdSetSampleLocationsEXT
  , PFN_vkCmdSetSampleLocationsEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceMultisamplePropertiesEXT
#endif
  , FN_vkGetPhysicalDeviceMultisamplePropertiesEXT
  , PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT
  , pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  , pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  , pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION
  , pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkSampleCountFlagBits(..)
  , VkPhysicalDevice
  , VkSampleCountFlags
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkDynamicState(..)
  , VkExtent2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkAttachmentSampleLocationsEXT"
data VkAttachmentSampleLocationsEXT = VkAttachmentSampleLocationsEXT
  { -- No documentation found for Nested "VkAttachmentSampleLocationsEXT" "attachmentIndex"
  vkAttachmentIndex :: Word32
  , -- No documentation found for Nested "VkAttachmentSampleLocationsEXT" "sampleLocationsInfo"
  vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkAttachmentSampleLocationsEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkAttachmentSampleLocationsEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachmentIndex (poked :: VkAttachmentSampleLocationsEXT))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationsInfo (poked :: VkAttachmentSampleLocationsEXT))

instance Zero VkAttachmentSampleLocationsEXT where
  zero = VkAttachmentSampleLocationsEXT zero
                                        zero
-- No documentation found for TopLevel "VkMultisamplePropertiesEXT"
data VkMultisamplePropertiesEXT = VkMultisamplePropertiesEXT
  { -- No documentation found for Nested "VkMultisamplePropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMultisamplePropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMultisamplePropertiesEXT" "maxSampleLocationGridSize"
  vkMaxSampleLocationGridSize :: VkExtent2D
  }
  deriving (Eq, Show)

instance Storable VkMultisamplePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMultisamplePropertiesEXT <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMultisamplePropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMultisamplePropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxSampleLocationGridSize (poked :: VkMultisamplePropertiesEXT))

instance Zero VkMultisamplePropertiesEXT where
  zero = VkMultisamplePropertiesEXT zero
                                    zero
                                    zero
-- No documentation found for TopLevel "VkPhysicalDeviceSampleLocationsPropertiesEXT"
data VkPhysicalDeviceSampleLocationsPropertiesEXT = VkPhysicalDeviceSampleLocationsPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSampleCounts"
  vkSampleLocationSampleCounts :: VkSampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "maxSampleLocationGridSize"
  vkMaxSampleLocationGridSize :: VkExtent2D
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationCoordinateRange"
  vkSampleLocationCoordinateRange :: Vector 2 CFloat
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSubPixelBits"
  vkSampleLocationSubPixelBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "variableSampleLocations"
  vkVariableSampleLocations :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSampleLocationsPropertiesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceSampleLocationsPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 20)
                                                          <*> peek (ptr `plusPtr` 28)
                                                          <*> peek (ptr `plusPtr` 36)
                                                          <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationSampleCounts (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxSampleLocationGridSize (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkSampleLocationCoordinateRange (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkSampleLocationSubPixelBits (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkVariableSampleLocations (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))

instance Zero VkPhysicalDeviceSampleLocationsPropertiesEXT where
  zero = VkPhysicalDeviceSampleLocationsPropertiesEXT zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
-- No documentation found for TopLevel "VkPipelineSampleLocationsStateCreateInfoEXT"
data VkPipelineSampleLocationsStateCreateInfoEXT = VkPipelineSampleLocationsStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineSampleLocationsStateCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineSampleLocationsStateCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsEnable"
  vkSampleLocationsEnable :: VkBool32
  , -- No documentation found for Nested "VkPipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsInfo"
  vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkPipelineSampleLocationsStateCreateInfoEXT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkPipelineSampleLocationsStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationsEnable (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkSampleLocationsInfo (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))

instance Zero VkPipelineSampleLocationsStateCreateInfoEXT where
  zero = VkPipelineSampleLocationsStateCreateInfoEXT zero
                                                     zero
                                                     zero
                                                     zero
-- No documentation found for TopLevel "VkRenderPassSampleLocationsBeginInfoEXT"
data VkRenderPassSampleLocationsBeginInfoEXT = VkRenderPassSampleLocationsBeginInfoEXT
  { -- No documentation found for Nested "VkRenderPassSampleLocationsBeginInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassSampleLocationsBeginInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassSampleLocationsBeginInfoEXT" "attachmentInitialSampleLocationsCount"
  vkAttachmentInitialSampleLocationsCount :: Word32
  , -- No documentation found for Nested "VkRenderPassSampleLocationsBeginInfoEXT" "pAttachmentInitialSampleLocations"
  vkPAttachmentInitialSampleLocations :: Ptr VkAttachmentSampleLocationsEXT
  , -- No documentation found for Nested "VkRenderPassSampleLocationsBeginInfoEXT" "postSubpassSampleLocationsCount"
  vkPostSubpassSampleLocationsCount :: Word32
  , -- No documentation found for Nested "VkRenderPassSampleLocationsBeginInfoEXT" "pPostSubpassSampleLocations"
  vkPPostSubpassSampleLocations :: Ptr VkSubpassSampleLocationsEXT
  }
  deriving (Eq, Show)

instance Storable VkRenderPassSampleLocationsBeginInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkRenderPassSampleLocationsBeginInfoEXT <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 32)
                                                     <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkAttachmentInitialSampleLocationsCount (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPAttachmentInitialSampleLocations (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPostSubpassSampleLocationsCount (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkPPostSubpassSampleLocations (poked :: VkRenderPassSampleLocationsBeginInfoEXT))

instance Zero VkRenderPassSampleLocationsBeginInfoEXT where
  zero = VkRenderPassSampleLocationsBeginInfoEXT zero
                                                 zero
                                                 zero
                                                 zero
                                                 zero
                                                 zero
-- No documentation found for TopLevel "VkSampleLocationEXT"
data VkSampleLocationEXT = VkSampleLocationEXT
  { -- No documentation found for Nested "VkSampleLocationEXT" "x"
  vkX :: CFloat
  , -- No documentation found for Nested "VkSampleLocationEXT" "y"
  vkY :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkSampleLocationEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkSampleLocationEXT <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkSampleLocationEXT))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkSampleLocationEXT))

instance Zero VkSampleLocationEXT where
  zero = VkSampleLocationEXT zero
                             zero
-- No documentation found for TopLevel "VkSampleLocationsInfoEXT"
data VkSampleLocationsInfoEXT = VkSampleLocationsInfoEXT
  { -- No documentation found for Nested "VkSampleLocationsInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkSampleLocationsInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkSampleLocationsInfoEXT" "sampleLocationsPerPixel"
  vkSampleLocationsPerPixel :: VkSampleCountFlagBits
  , -- No documentation found for Nested "VkSampleLocationsInfoEXT" "sampleLocationGridSize"
  vkSampleLocationGridSize :: VkExtent2D
  , -- No documentation found for Nested "VkSampleLocationsInfoEXT" "sampleLocationsCount"
  vkSampleLocationsCount :: Word32
  , -- No documentation found for Nested "VkSampleLocationsInfoEXT" "pSampleLocations"
  vkPSampleLocations :: Ptr VkSampleLocationEXT
  }
  deriving (Eq, Show)

instance Storable VkSampleLocationsInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkSampleLocationsInfoEXT <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 28)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationsPerPixel (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkSampleLocationGridSize (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 28) (vkSampleLocationsCount (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPSampleLocations (poked :: VkSampleLocationsInfoEXT))

instance Zero VkSampleLocationsInfoEXT where
  zero = VkSampleLocationsInfoEXT zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  zero
-- No documentation found for TopLevel "VkSubpassSampleLocationsEXT"
data VkSubpassSampleLocationsEXT = VkSubpassSampleLocationsEXT
  { -- No documentation found for Nested "VkSubpassSampleLocationsEXT" "subpassIndex"
  vkSubpassIndex :: Word32
  , -- No documentation found for Nested "VkSubpassSampleLocationsEXT" "sampleLocationsInfo"
  vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkSubpassSampleLocationsEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkSubpassSampleLocationsEXT <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSubpassIndex (poked :: VkSubpassSampleLocationsEXT))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationsInfo (poked :: VkSubpassSampleLocationsEXT))

instance Zero VkSubpassSampleLocationsEXT where
  zero = VkSubpassSampleLocationsEXT zero
                                     zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdSetSampleLocationsEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetSampleLocationsEXT" vkCmdSetSampleLocationsEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ()

#endif
type FN_vkCmdSetSampleLocationsEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ()
type PFN_vkCmdSetSampleLocationsEXT = FunPtr FN_vkCmdSetSampleLocationsEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceMultisamplePropertiesEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMultisamplePropertiesEXT" vkGetPhysicalDeviceMultisamplePropertiesEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ()

#endif
type FN_vkGetPhysicalDeviceMultisamplePropertiesEXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ()
type PFN_vkGetPhysicalDeviceMultisamplePropertiesEXT = FunPtr FN_vkGetPhysicalDeviceMultisamplePropertiesEXT
-- No documentation found for Nested "VkDynamicState" "VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT"
pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT :: VkDynamicState
pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT = VkDynamicState 1000143000
-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME"
pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = "VK_EXT_sample_locations"
-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION"
pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = VkImageCreateFlagBits 0x00001000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT = VkStructureType 1000143004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT = VkStructureType 1000143003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT = VkStructureType 1000143002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT = VkStructureType 1000143001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT"
pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT = VkStructureType 1000143000
