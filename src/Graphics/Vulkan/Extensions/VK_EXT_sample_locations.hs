{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  , pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION
  , pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  , vkCmdSetSampleLocationsEXT
  , vkGetPhysicalDeviceMultisamplePropertiesEXT
  , VkSampleLocationEXT(..)
  , VkSampleLocationsInfoEXT(..)
  , VkAttachmentSampleLocationsEXT(..)
  , VkSubpassSampleLocationsEXT(..)
  , VkRenderPassSampleLocationsBeginInfoEXT(..)
  , VkPipelineSampleLocationsStateCreateInfoEXT(..)
  , VkPhysicalDeviceSampleLocationsPropertiesEXT(..)
  , VkMultisamplePropertiesEXT(..)
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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkSampleCountFlags
  , VkSampleCountFlagBits(..)
  , VkPhysicalDevice
  , VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkExtent2D(..)
  , VkDynamicState(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( VkCommandBuffer
  )


-- | Nothing
pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT :: VkDynamicState
pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT = VkDynamicState 1000143000
-- | Nothing
pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT = VkStructureType 1000143000
-- | Nothing
pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT = VkStructureType 1000143001
-- | Nothing
pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT = VkStructureType 1000143002
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT = VkStructureType 1000143003
-- | Nothing
pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT = VkStructureType 1000143004
-- | Nothing
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = VkImageCreateFlagBits 0x00001000
pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1
pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = "VK_EXT_sample_locations"
-- | 
foreign import ccall "vkCmdSetSampleLocationsEXT" vkCmdSetSampleLocationsEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ()
-- | 
foreign import ccall "vkGetPhysicalDeviceMultisamplePropertiesEXT" vkGetPhysicalDeviceMultisamplePropertiesEXT :: ("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ()
-- | TODO: Struct comments
data VkSampleLocationEXT = VkSampleLocationEXT
  { vkX :: CFloat
  , vkY :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkSampleLocationEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkSampleLocationEXT <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkSampleLocationEXT))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkSampleLocationEXT))
-- | TODO: Struct comments
data VkSampleLocationsInfoEXT = VkSampleLocationsInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSampleLocationsPerPixel :: VkSampleCountFlagBits
  , vkSampleLocationGridSize :: VkExtent2D
  , vkSampleLocationsCount :: Word32
  , vkSampleLocations :: Ptr VkSampleLocationEXT
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationsPerPixel (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkSampleLocationGridSize (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 28) (vkSampleLocationsCount (poked :: VkSampleLocationsInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkSampleLocations (poked :: VkSampleLocationsInfoEXT))
-- | TODO: Struct comments
data VkAttachmentSampleLocationsEXT = VkAttachmentSampleLocationsEXT
  { vkAttachmentIndex :: Word32
  , vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkAttachmentSampleLocationsEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkAttachmentSampleLocationsEXT <$> peek (ptr `plusPtr` 0)
                                            <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAttachmentIndex (poked :: VkAttachmentSampleLocationsEXT))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationsInfo (poked :: VkAttachmentSampleLocationsEXT))
-- | TODO: Struct comments
data VkSubpassSampleLocationsEXT = VkSubpassSampleLocationsEXT
  { vkSubpassIndex :: Word32
  , vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkSubpassSampleLocationsEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkSubpassSampleLocationsEXT <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSubpassIndex (poked :: VkSubpassSampleLocationsEXT))
                *> poke (ptr `plusPtr` 8) (vkSampleLocationsInfo (poked :: VkSubpassSampleLocationsEXT))
-- | TODO: Struct comments
data VkRenderPassSampleLocationsBeginInfoEXT = VkRenderPassSampleLocationsBeginInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkAttachmentInitialSampleLocationsCount :: Word32
  , vkAttachmentInitialSampleLocations :: Ptr VkAttachmentSampleLocationsEXT
  , vkPostSubpassSampleLocationsCount :: Word32
  , vkPostSubpassSampleLocations :: Ptr VkSubpassSampleLocationsEXT
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkAttachmentInitialSampleLocationsCount (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkAttachmentInitialSampleLocations (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPostSubpassSampleLocationsCount (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkPostSubpassSampleLocations (poked :: VkRenderPassSampleLocationsBeginInfoEXT))
-- | TODO: Struct comments
data VkPipelineSampleLocationsStateCreateInfoEXT = VkPipelineSampleLocationsStateCreateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSampleLocationsEnable :: VkBool32
  , vkSampleLocationsInfo :: VkSampleLocationsInfoEXT
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationsEnable (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkSampleLocationsInfo (poked :: VkPipelineSampleLocationsStateCreateInfoEXT))
-- | TODO: Struct comments
data VkPhysicalDeviceSampleLocationsPropertiesEXT = VkPhysicalDeviceSampleLocationsPropertiesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkSampleLocationSampleCounts :: VkSampleCountFlags
  , vkMaxSampleLocationGridSize :: VkExtent2D
  , vkSampleLocationCoordinateRange :: Vector 2 CFloat
  , vkSampleLocationSubPixelBits :: Word32
  , vkVariableSampleLocations :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkSampleLocationSampleCounts (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxSampleLocationGridSize (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkSampleLocationCoordinateRange (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkSampleLocationSubPixelBits (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkVariableSampleLocations (poked :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
-- | TODO: Struct comments
data VkMultisamplePropertiesEXT = VkMultisamplePropertiesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMaxSampleLocationGridSize :: VkExtent2D
  }
  deriving (Eq, Show)

instance Storable VkMultisamplePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMultisamplePropertiesEXT <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMultisamplePropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMultisamplePropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxSampleLocationGridSize (poked :: VkMultisamplePropertiesEXT))
