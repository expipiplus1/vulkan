{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( AttachmentSampleLocationsEXT(..)
#if defined(VK_USE_PLATFORM_GGP)
  , MultisamplePropertiesEXT(..)
  , PhysicalDeviceSampleLocationsPropertiesEXT(..)
  , PipelineSampleLocationsStateCreateInfoEXT(..)
  , RenderPassSampleLocationsBeginInfoEXT(..)
#endif
  , SampleLocationEXT(..)
#if defined(VK_USE_PLATFORM_GGP)
  , SampleLocationsInfoEXT(..)
#endif
  , SubpassSampleLocationsEXT(..)
  , cmdSetSampleLocationsEXT
#if defined(VK_USE_PLATFORM_GGP)
  , getPhysicalDeviceMultisamplePropertiesEXT
#endif
  , pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  , pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION
  , pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  , pattern STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  , pattern STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif
import Data.Word
  ( Word32
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif
import Foreign.Marshal.Utils
  ( with
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( vkCmdSetSampleLocationsEXT
  , pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  , pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( vkGetPhysicalDeviceMultisamplePropertiesEXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  , SampleCountFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( SampleCountFlagBits
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  )
#endif
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  )



-- No documentation found for TopLevel "VkAttachmentSampleLocationsEXT"
data AttachmentSampleLocationsEXT = AttachmentSampleLocationsEXT
  { -- No documentation found for Nested "AttachmentSampleLocationsEXT" "attachmentIndex"
  attachmentIndex :: Word32
  , -- No documentation found for Nested "AttachmentSampleLocationsEXT" "sampleLocationsInfo"
  sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)

instance Zero AttachmentSampleLocationsEXT where
  zero = AttachmentSampleLocationsEXT zero
                                      zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMultisamplePropertiesEXT"
data MultisamplePropertiesEXT = MultisamplePropertiesEXT
  { -- No documentation found for Nested "MultisamplePropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MultisamplePropertiesEXT" "maxSampleLocationGridSize"
  maxSampleLocationGridSize :: Extent2D
  }
  deriving (Show, Eq)

instance Zero MultisamplePropertiesEXT where
  zero = MultisamplePropertiesEXT Nothing
                                  zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceSampleLocationsPropertiesEXT"
data PhysicalDeviceSampleLocationsPropertiesEXT = PhysicalDeviceSampleLocationsPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSampleCounts"
  sampleLocationSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "maxSampleLocationGridSize"
  maxSampleLocationGridSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationCoordinateRange"
  sampleLocationCoordinateRange :: (Float, Float)
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSubPixelBits"
  sampleLocationSubPixelBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "variableSampleLocations"
  variableSampleLocations :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceSampleLocationsPropertiesEXT where
  zero = PhysicalDeviceSampleLocationsPropertiesEXT Nothing
                                                    zero
                                                    zero
                                                    (zero, zero)
                                                    zero
                                                    False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineSampleLocationsStateCreateInfoEXT"
data PipelineSampleLocationsStateCreateInfoEXT = PipelineSampleLocationsStateCreateInfoEXT
  { -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsEnable"
  sampleLocationsEnable :: Bool
  , -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsInfo"
  sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)

instance Zero PipelineSampleLocationsStateCreateInfoEXT where
  zero = PipelineSampleLocationsStateCreateInfoEXT Nothing
                                                   False
                                                   zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkRenderPassSampleLocationsBeginInfoEXT"
data RenderPassSampleLocationsBeginInfoEXT = RenderPassSampleLocationsBeginInfoEXT
  { -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pAttachmentInitialSampleLocations"
  attachmentInitialSampleLocations :: Vector AttachmentSampleLocationsEXT
  , -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pPostSubpassSampleLocations"
  postSubpassSampleLocations :: Vector SubpassSampleLocationsEXT
  }
  deriving (Show, Eq)

instance Zero RenderPassSampleLocationsBeginInfoEXT where
  zero = RenderPassSampleLocationsBeginInfoEXT Nothing
                                               mempty
                                               mempty

#endif


-- No documentation found for TopLevel "VkSampleLocationEXT"
data SampleLocationEXT = SampleLocationEXT
  { -- No documentation found for Nested "SampleLocationEXT" "x"
  x :: Float
  , -- No documentation found for Nested "SampleLocationEXT" "y"
  y :: Float
  }
  deriving (Show, Eq)

instance Zero SampleLocationEXT where
  zero = SampleLocationEXT zero
                           zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSampleLocationsInfoEXT"
data SampleLocationsInfoEXT = SampleLocationsInfoEXT
  { -- No documentation found for Nested "SampleLocationsInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "sampleLocationsPerPixel"
  sampleLocationsPerPixel :: SampleCountFlagBits
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "sampleLocationGridSize"
  sampleLocationGridSize :: Extent2D
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "pSampleLocations"
  sampleLocations :: Vector SampleLocationEXT
  }
  deriving (Show, Eq)

instance Zero SampleLocationsInfoEXT where
  zero = SampleLocationsInfoEXT Nothing
                                zero
                                zero
                                mempty

#endif


-- No documentation found for TopLevel "VkSubpassSampleLocationsEXT"
data SubpassSampleLocationsEXT = SubpassSampleLocationsEXT
  { -- No documentation found for Nested "SubpassSampleLocationsEXT" "subpassIndex"
  subpassIndex :: Word32
  , -- No documentation found for Nested "SubpassSampleLocationsEXT" "sampleLocationsInfo"
  sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)

instance Zero SubpassSampleLocationsEXT where
  zero = SubpassSampleLocationsEXT zero
                                   zero



-- No documentation found for TopLevel "vkCmdSetSampleLocationsEXT"
cmdSetSampleLocationsEXT :: CommandBuffer ->  SampleLocationsInfoEXT ->  IO ()
cmdSetSampleLocationsEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceMultisamplePropertiesEXT"
getPhysicalDeviceMultisamplePropertiesEXT :: PhysicalDevice ->  SampleCountFlagBits ->  IO (MultisamplePropertiesEXT)
getPhysicalDeviceMultisamplePropertiesEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif

-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME"
pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION"
pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: Integral a => a
pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION = VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION
