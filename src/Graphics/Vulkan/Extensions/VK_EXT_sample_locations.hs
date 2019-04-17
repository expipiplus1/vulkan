{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( withCStructAttachmentSampleLocationsEXT
  , fromCStructAttachmentSampleLocationsEXT
  , AttachmentSampleLocationsEXT(..)
  , withCStructMultisamplePropertiesEXT
  , fromCStructMultisamplePropertiesEXT
  , MultisamplePropertiesEXT(..)
  , withCStructPhysicalDeviceSampleLocationsPropertiesEXT
  , fromCStructPhysicalDeviceSampleLocationsPropertiesEXT
  , PhysicalDeviceSampleLocationsPropertiesEXT(..)
  , withCStructPipelineSampleLocationsStateCreateInfoEXT
  , fromCStructPipelineSampleLocationsStateCreateInfoEXT
  , PipelineSampleLocationsStateCreateInfoEXT(..)
  , withCStructRenderPassSampleLocationsBeginInfoEXT
  , fromCStructRenderPassSampleLocationsBeginInfoEXT
  , RenderPassSampleLocationsBeginInfoEXT(..)
  , withCStructSampleLocationEXT
  , fromCStructSampleLocationEXT
  , SampleLocationEXT(..)
  , withCStructSampleLocationsInfoEXT
  , fromCStructSampleLocationsInfoEXT
  , SampleLocationsInfoEXT(..)
  , withCStructSubpassSampleLocationsEXT
  , fromCStructSubpassSampleLocationsEXT
  , SubpassSampleLocationsEXT(..)
  , cmdSetSampleLocationsEXT
  , getPhysicalDeviceMultisamplePropertiesEXT
  , pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION
  , pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  , pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Vector.Generic.Sized
  ( fromTuple
  )
import qualified Data.Vector.Storable.Sized
  ( unsafeIndex
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdSetSampleLocationsEXT
  , getPhysicalDeviceMultisamplePropertiesEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( VkAttachmentSampleLocationsEXT(..)
  , VkMultisamplePropertiesEXT(..)
  , VkPhysicalDeviceSampleLocationsPropertiesEXT(..)
  , VkPipelineSampleLocationsStateCreateInfoEXT(..)
  , VkRenderPassSampleLocationsBeginInfoEXT(..)
  , VkSampleLocationEXT(..)
  , VkSampleLocationsInfoEXT(..)
  , VkSubpassSampleLocationsEXT(..)
  , pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  , SampleCountFlagBits
  , SampleCountFlags
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , fromCStructExtent2D
  , withCStructExtent2D
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( pattern VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT
  , pattern VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
  , pattern VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION
  , pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  )


-- No documentation found for TopLevel "AttachmentSampleLocationsEXT"
data AttachmentSampleLocationsEXT = AttachmentSampleLocationsEXT
  { -- No documentation found for Nested "AttachmentSampleLocationsEXT" "attachmentIndex"
  vkAttachmentIndex :: Word32
  , -- No documentation found for Nested "AttachmentSampleLocationsEXT" "sampleLocationsInfo"
  vkSampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)
withCStructAttachmentSampleLocationsEXT :: AttachmentSampleLocationsEXT -> (VkAttachmentSampleLocationsEXT -> IO a) -> IO a
withCStructAttachmentSampleLocationsEXT from cont = withCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (from :: AttachmentSampleLocationsEXT)) (\sampleLocationsInfo -> cont (VkAttachmentSampleLocationsEXT (vkAttachmentIndex (from :: AttachmentSampleLocationsEXT)) sampleLocationsInfo))
fromCStructAttachmentSampleLocationsEXT :: VkAttachmentSampleLocationsEXT -> IO AttachmentSampleLocationsEXT
fromCStructAttachmentSampleLocationsEXT c = AttachmentSampleLocationsEXT <$> pure (vkAttachmentIndex (c :: VkAttachmentSampleLocationsEXT))
                                                                         <*> (fromCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (c :: VkAttachmentSampleLocationsEXT)))
instance Zero AttachmentSampleLocationsEXT where
  zero = AttachmentSampleLocationsEXT zero
                                      zero
-- No documentation found for TopLevel "MultisamplePropertiesEXT"
data MultisamplePropertiesEXT = MultisamplePropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "MultisamplePropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MultisamplePropertiesEXT" "maxSampleLocationGridSize"
  vkMaxSampleLocationGridSize :: Extent2D
  }
  deriving (Show, Eq)
withCStructMultisamplePropertiesEXT :: MultisamplePropertiesEXT -> (VkMultisamplePropertiesEXT -> IO a) -> IO a
withCStructMultisamplePropertiesEXT from cont = withCStructExtent2D (vkMaxSampleLocationGridSize (from :: MultisamplePropertiesEXT)) (\maxSampleLocationGridSize -> maybeWith withSomeVkStruct (vkPNext (from :: MultisamplePropertiesEXT)) (\pPNext -> cont (VkMultisamplePropertiesEXT VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT pPNext maxSampleLocationGridSize)))
fromCStructMultisamplePropertiesEXT :: VkMultisamplePropertiesEXT -> IO MultisamplePropertiesEXT
fromCStructMultisamplePropertiesEXT c = MultisamplePropertiesEXT <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMultisamplePropertiesEXT)))
                                                                 <*> (fromCStructExtent2D (vkMaxSampleLocationGridSize (c :: VkMultisamplePropertiesEXT)))
instance Zero MultisamplePropertiesEXT where
  zero = MultisamplePropertiesEXT Nothing
                                  zero
-- No documentation found for TopLevel "PhysicalDeviceSampleLocationsPropertiesEXT"
data PhysicalDeviceSampleLocationsPropertiesEXT = PhysicalDeviceSampleLocationsPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSampleCounts"
  vkSampleLocationSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "maxSampleLocationGridSize"
  vkMaxSampleLocationGridSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationCoordinateRange"
  vkSampleLocationCoordinateRange :: (CFloat, CFloat)
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSubPixelBits"
  vkSampleLocationSubPixelBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "variableSampleLocations"
  vkVariableSampleLocations :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceSampleLocationsPropertiesEXT :: PhysicalDeviceSampleLocationsPropertiesEXT -> (VkPhysicalDeviceSampleLocationsPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceSampleLocationsPropertiesEXT from cont = withCStructExtent2D (vkMaxSampleLocationGridSize (from :: PhysicalDeviceSampleLocationsPropertiesEXT)) (\maxSampleLocationGridSize -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceSampleLocationsPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceSampleLocationsPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT pPNext (vkSampleLocationSampleCounts (from :: PhysicalDeviceSampleLocationsPropertiesEXT)) maxSampleLocationGridSize (fromTuple (vkSampleLocationCoordinateRange (from :: PhysicalDeviceSampleLocationsPropertiesEXT))) (vkSampleLocationSubPixelBits (from :: PhysicalDeviceSampleLocationsPropertiesEXT)) (boolToBool32 (vkVariableSampleLocations (from :: PhysicalDeviceSampleLocationsPropertiesEXT))))))
fromCStructPhysicalDeviceSampleLocationsPropertiesEXT :: VkPhysicalDeviceSampleLocationsPropertiesEXT -> IO PhysicalDeviceSampleLocationsPropertiesEXT
fromCStructPhysicalDeviceSampleLocationsPropertiesEXT c = PhysicalDeviceSampleLocationsPropertiesEXT <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT)))
                                                                                                     <*> pure (vkSampleLocationSampleCounts (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                                                                                                     <*> (fromCStructExtent2D (vkMaxSampleLocationGridSize (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT)))
                                                                                                     <*> pure (let x = (vkSampleLocationCoordinateRange (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                                                                     , Data.Vector.Storable.Sized.unsafeIndex x 1 ))
                                                                                                     <*> pure (vkSampleLocationSubPixelBits (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                                                                                                     <*> pure (bool32ToBool (vkVariableSampleLocations (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT)))
instance Zero PhysicalDeviceSampleLocationsPropertiesEXT where
  zero = PhysicalDeviceSampleLocationsPropertiesEXT Nothing
                                                    zero
                                                    zero
                                                    (zero, zero)
                                                    zero
                                                    False
-- No documentation found for TopLevel "PipelineSampleLocationsStateCreateInfoEXT"
data PipelineSampleLocationsStateCreateInfoEXT = PipelineSampleLocationsStateCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsEnable"
  vkSampleLocationsEnable :: Bool
  , -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsInfo"
  vkSampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)
withCStructPipelineSampleLocationsStateCreateInfoEXT :: PipelineSampleLocationsStateCreateInfoEXT -> (VkPipelineSampleLocationsStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineSampleLocationsStateCreateInfoEXT from cont = withCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (from :: PipelineSampleLocationsStateCreateInfoEXT)) (\sampleLocationsInfo -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineSampleLocationsStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineSampleLocationsStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT pPNext (boolToBool32 (vkSampleLocationsEnable (from :: PipelineSampleLocationsStateCreateInfoEXT))) sampleLocationsInfo)))
fromCStructPipelineSampleLocationsStateCreateInfoEXT :: VkPipelineSampleLocationsStateCreateInfoEXT -> IO PipelineSampleLocationsStateCreateInfoEXT
fromCStructPipelineSampleLocationsStateCreateInfoEXT c = PipelineSampleLocationsStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineSampleLocationsStateCreateInfoEXT)))
                                                                                                   <*> pure (bool32ToBool (vkSampleLocationsEnable (c :: VkPipelineSampleLocationsStateCreateInfoEXT)))
                                                                                                   <*> (fromCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (c :: VkPipelineSampleLocationsStateCreateInfoEXT)))
instance Zero PipelineSampleLocationsStateCreateInfoEXT where
  zero = PipelineSampleLocationsStateCreateInfoEXT Nothing
                                                   False
                                                   zero
-- No documentation found for TopLevel "RenderPassSampleLocationsBeginInfoEXT"
data RenderPassSampleLocationsBeginInfoEXT = RenderPassSampleLocationsBeginInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pAttachmentInitialSampleLocations"
  vkPAttachmentInitialSampleLocations :: Vector AttachmentSampleLocationsEXT
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pPostSubpassSampleLocations"
  vkPPostSubpassSampleLocations :: Vector SubpassSampleLocationsEXT
  }
  deriving (Show, Eq)
withCStructRenderPassSampleLocationsBeginInfoEXT :: RenderPassSampleLocationsBeginInfoEXT -> (VkRenderPassSampleLocationsBeginInfoEXT -> IO a) -> IO a
withCStructRenderPassSampleLocationsBeginInfoEXT from cont = withVec withCStructSubpassSampleLocationsEXT (vkPPostSubpassSampleLocations (from :: RenderPassSampleLocationsBeginInfoEXT)) (\pPostSubpassSampleLocations -> withVec withCStructAttachmentSampleLocationsEXT (vkPAttachmentInitialSampleLocations (from :: RenderPassSampleLocationsBeginInfoEXT)) (\pAttachmentInitialSampleLocations -> maybeWith withSomeVkStruct (vkPNext (from :: RenderPassSampleLocationsBeginInfoEXT)) (\pPNext -> cont (VkRenderPassSampleLocationsBeginInfoEXT VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT pPNext (fromIntegral (Data.Vector.length (vkPAttachmentInitialSampleLocations (from :: RenderPassSampleLocationsBeginInfoEXT)))) pAttachmentInitialSampleLocations (fromIntegral (Data.Vector.length (vkPPostSubpassSampleLocations (from :: RenderPassSampleLocationsBeginInfoEXT)))) pPostSubpassSampleLocations))))
fromCStructRenderPassSampleLocationsBeginInfoEXT :: VkRenderPassSampleLocationsBeginInfoEXT -> IO RenderPassSampleLocationsBeginInfoEXT
fromCStructRenderPassSampleLocationsBeginInfoEXT c = RenderPassSampleLocationsBeginInfoEXT <$> -- Univalued Member elided
                                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkRenderPassSampleLocationsBeginInfoEXT)))
                                                                                           -- Length valued member elided
                                                                                           <*> (Data.Vector.generateM (fromIntegral (vkAttachmentInitialSampleLocationsCount (c :: VkRenderPassSampleLocationsBeginInfoEXT))) (((fromCStructAttachmentSampleLocationsEXT <=<) . peekElemOff) (vkPAttachmentInitialSampleLocations (c :: VkRenderPassSampleLocationsBeginInfoEXT))))
                                                                                           -- Length valued member elided
                                                                                           <*> (Data.Vector.generateM (fromIntegral (vkPostSubpassSampleLocationsCount (c :: VkRenderPassSampleLocationsBeginInfoEXT))) (((fromCStructSubpassSampleLocationsEXT <=<) . peekElemOff) (vkPPostSubpassSampleLocations (c :: VkRenderPassSampleLocationsBeginInfoEXT))))
instance Zero RenderPassSampleLocationsBeginInfoEXT where
  zero = RenderPassSampleLocationsBeginInfoEXT Nothing
                                               Data.Vector.empty
                                               Data.Vector.empty
-- No documentation found for TopLevel "SampleLocationEXT"
data SampleLocationEXT = SampleLocationEXT
  { -- No documentation found for Nested "SampleLocationEXT" "x"
  vkX :: CFloat
  , -- No documentation found for Nested "SampleLocationEXT" "y"
  vkY :: CFloat
  }
  deriving (Show, Eq)
withCStructSampleLocationEXT :: SampleLocationEXT -> (VkSampleLocationEXT -> IO a) -> IO a
withCStructSampleLocationEXT from cont = cont (VkSampleLocationEXT (vkX (from :: SampleLocationEXT)) (vkY (from :: SampleLocationEXT)))
fromCStructSampleLocationEXT :: VkSampleLocationEXT -> IO SampleLocationEXT
fromCStructSampleLocationEXT c = SampleLocationEXT <$> pure (vkX (c :: VkSampleLocationEXT))
                                                   <*> pure (vkY (c :: VkSampleLocationEXT))
instance Zero SampleLocationEXT where
  zero = SampleLocationEXT zero
                           zero
-- No documentation found for TopLevel "SampleLocationsInfoEXT"
data SampleLocationsInfoEXT = SampleLocationsInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "SampleLocationsInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "sampleLocationsPerPixel"
  vkSampleLocationsPerPixel :: SampleCountFlagBits
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "sampleLocationGridSize"
  vkSampleLocationGridSize :: Extent2D
  -- Length valued member elided
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "pSampleLocations"
  vkPSampleLocations :: Vector SampleLocationEXT
  }
  deriving (Show, Eq)
withCStructSampleLocationsInfoEXT :: SampleLocationsInfoEXT -> (VkSampleLocationsInfoEXT -> IO a) -> IO a
withCStructSampleLocationsInfoEXT from cont = withVec withCStructSampleLocationEXT (vkPSampleLocations (from :: SampleLocationsInfoEXT)) (\pSampleLocations -> withCStructExtent2D (vkSampleLocationGridSize (from :: SampleLocationsInfoEXT)) (\sampleLocationGridSize -> maybeWith withSomeVkStruct (vkPNext (from :: SampleLocationsInfoEXT)) (\pPNext -> cont (VkSampleLocationsInfoEXT VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT pPNext (vkSampleLocationsPerPixel (from :: SampleLocationsInfoEXT)) sampleLocationGridSize (fromIntegral (Data.Vector.length (vkPSampleLocations (from :: SampleLocationsInfoEXT)))) pSampleLocations))))
fromCStructSampleLocationsInfoEXT :: VkSampleLocationsInfoEXT -> IO SampleLocationsInfoEXT
fromCStructSampleLocationsInfoEXT c = SampleLocationsInfoEXT <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSampleLocationsInfoEXT)))
                                                             <*> pure (vkSampleLocationsPerPixel (c :: VkSampleLocationsInfoEXT))
                                                             <*> (fromCStructExtent2D (vkSampleLocationGridSize (c :: VkSampleLocationsInfoEXT)))
                                                             -- Length valued member elided
                                                             <*> (Data.Vector.generateM (fromIntegral (vkSampleLocationsCount (c :: VkSampleLocationsInfoEXT))) (((fromCStructSampleLocationEXT <=<) . peekElemOff) (vkPSampleLocations (c :: VkSampleLocationsInfoEXT))))
instance Zero SampleLocationsInfoEXT where
  zero = SampleLocationsInfoEXT Nothing
                                zero
                                zero
                                Data.Vector.empty
-- No documentation found for TopLevel "SubpassSampleLocationsEXT"
data SubpassSampleLocationsEXT = SubpassSampleLocationsEXT
  { -- No documentation found for Nested "SubpassSampleLocationsEXT" "subpassIndex"
  vkSubpassIndex :: Word32
  , -- No documentation found for Nested "SubpassSampleLocationsEXT" "sampleLocationsInfo"
  vkSampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)
withCStructSubpassSampleLocationsEXT :: SubpassSampleLocationsEXT -> (VkSubpassSampleLocationsEXT -> IO a) -> IO a
withCStructSubpassSampleLocationsEXT from cont = withCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (from :: SubpassSampleLocationsEXT)) (\sampleLocationsInfo -> cont (VkSubpassSampleLocationsEXT (vkSubpassIndex (from :: SubpassSampleLocationsEXT)) sampleLocationsInfo))
fromCStructSubpassSampleLocationsEXT :: VkSubpassSampleLocationsEXT -> IO SubpassSampleLocationsEXT
fromCStructSubpassSampleLocationsEXT c = SubpassSampleLocationsEXT <$> pure (vkSubpassIndex (c :: VkSubpassSampleLocationsEXT))
                                                                   <*> (fromCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (c :: VkSubpassSampleLocationsEXT)))
instance Zero SubpassSampleLocationsEXT where
  zero = SubpassSampleLocationsEXT zero
                                   zero

-- | Wrapper for 'vkCmdSetSampleLocationsEXT'
cmdSetSampleLocationsEXT :: CommandBuffer ->  SampleLocationsInfoEXT ->  IO ()
cmdSetSampleLocationsEXT = \(CommandBuffer commandBuffer commandTable) -> \sampleLocationsInfo -> (\a -> withCStructSampleLocationsInfoEXT a . flip with) sampleLocationsInfo (\pSampleLocationsInfo -> Graphics.Vulkan.C.Dynamic.cmdSetSampleLocationsEXT commandTable commandBuffer pSampleLocationsInfo *> (pure ()))

-- | Wrapper for 'vkGetPhysicalDeviceMultisamplePropertiesEXT'
getPhysicalDeviceMultisamplePropertiesEXT :: PhysicalDevice ->  SampleCountFlagBits ->  IO (MultisamplePropertiesEXT)
getPhysicalDeviceMultisamplePropertiesEXT = \(PhysicalDevice physicalDevice commandTable) -> \samples -> alloca (\pMultisampleProperties -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceMultisamplePropertiesEXT commandTable physicalDevice samples pMultisampleProperties *> ((fromCStructMultisamplePropertiesEXT <=< peek) pMultisampleProperties))
