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
  , vkCmdSetSampleLocationsEXT
  , vkGetPhysicalDeviceMultisamplePropertiesEXT
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



-- | VkAttachmentSampleLocationsEXT - Structure specifying the sample
-- locations state to use in the initial layout transition of attachments
--
-- = Description
--
-- If the image referenced by the framebuffer attachment at index
-- @attachmentIndex@ was not created with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
-- then the values specified in @sampleLocationsInfo@ are ignored.
--
-- == Valid Usage
--
-- Unresolved directive in VkAttachmentSampleLocationsEXT.txt -
-- include::{generated}\/validity\/structs\/VkAttachmentSampleLocationsEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data AttachmentSampleLocationsEXT = AttachmentSampleLocationsEXT
  { -- No documentation found for Nested "AttachmentSampleLocationsEXT" "attachmentIndex"
  attachmentIndex :: Word32
  , -- No documentation found for Nested "AttachmentSampleLocationsEXT" "sampleLocationsInfo"
  sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAttachmentSampleLocationsEXT' and
-- marshal a 'AttachmentSampleLocationsEXT' into it. The 'VkAttachmentSampleLocationsEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAttachmentSampleLocationsEXT :: AttachmentSampleLocationsEXT -> (VkAttachmentSampleLocationsEXT -> IO a) -> IO a
withCStructAttachmentSampleLocationsEXT marshalled cont = withCStructSampleLocationsInfoEXT (sampleLocationsInfo (marshalled :: AttachmentSampleLocationsEXT)) (\sampleLocationsInfo'' -> cont (VkAttachmentSampleLocationsEXT (attachmentIndex (marshalled :: AttachmentSampleLocationsEXT)) sampleLocationsInfo''))

-- | A function to read a 'VkAttachmentSampleLocationsEXT' and all additional
-- structures in the pointer chain into a 'AttachmentSampleLocationsEXT'.
fromCStructAttachmentSampleLocationsEXT :: VkAttachmentSampleLocationsEXT -> IO AttachmentSampleLocationsEXT
fromCStructAttachmentSampleLocationsEXT c = AttachmentSampleLocationsEXT <$> pure (vkAttachmentIndex (c :: VkAttachmentSampleLocationsEXT))
                                                                         <*> (fromCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (c :: VkAttachmentSampleLocationsEXT)))

instance Zero AttachmentSampleLocationsEXT where
  zero = AttachmentSampleLocationsEXT zero
                                      zero



-- | VkMultisamplePropertiesEXT - Structure returning information about
-- sample count specific additional multisampling capabilities
--
-- = Description
--
-- Unresolved directive in VkMultisamplePropertiesEXT.txt -
-- include::{generated}\/validity\/structs\/VkMultisamplePropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data MultisamplePropertiesEXT = MultisamplePropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "MultisamplePropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MultisamplePropertiesEXT" "maxSampleLocationGridSize"
  maxSampleLocationGridSize :: Extent2D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMultisamplePropertiesEXT' and
-- marshal a 'MultisamplePropertiesEXT' into it. The 'VkMultisamplePropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMultisamplePropertiesEXT :: MultisamplePropertiesEXT -> (VkMultisamplePropertiesEXT -> IO a) -> IO a
withCStructMultisamplePropertiesEXT marshalled cont = withCStructExtent2D (maxSampleLocationGridSize (marshalled :: MultisamplePropertiesEXT)) (\maxSampleLocationGridSize'' -> maybeWith withSomeVkStruct (next (marshalled :: MultisamplePropertiesEXT)) (\pPNext -> cont (VkMultisamplePropertiesEXT VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT pPNext maxSampleLocationGridSize'')))

-- | A function to read a 'VkMultisamplePropertiesEXT' and all additional
-- structures in the pointer chain into a 'MultisamplePropertiesEXT'.
fromCStructMultisamplePropertiesEXT :: VkMultisamplePropertiesEXT -> IO MultisamplePropertiesEXT
fromCStructMultisamplePropertiesEXT c = MultisamplePropertiesEXT <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMultisamplePropertiesEXT)))
                                                                 <*> (fromCStructExtent2D (vkMaxSampleLocationGridSize (c :: VkMultisamplePropertiesEXT)))

instance Zero MultisamplePropertiesEXT where
  zero = MultisamplePropertiesEXT Nothing
                                  zero



-- | VkPhysicalDeviceSampleLocationsPropertiesEXT - Structure describing
-- sample location limits that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in VkPhysicalDeviceSampleLocationsPropertiesEXT.txt
-- -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceSampleLocationsPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceSampleLocationsPropertiesEXT = PhysicalDeviceSampleLocationsPropertiesEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSampleCounts"
  sampleLocationSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "maxSampleLocationGridSize"
  maxSampleLocationGridSize :: Extent2D
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationCoordinateRange"
  sampleLocationCoordinateRange :: (CFloat, CFloat)
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSubPixelBits"
  sampleLocationSubPixelBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceSampleLocationsPropertiesEXT" "variableSampleLocations"
  variableSampleLocations :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceSampleLocationsPropertiesEXT' and
-- marshal a 'PhysicalDeviceSampleLocationsPropertiesEXT' into it. The 'VkPhysicalDeviceSampleLocationsPropertiesEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceSampleLocationsPropertiesEXT :: PhysicalDeviceSampleLocationsPropertiesEXT -> (VkPhysicalDeviceSampleLocationsPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceSampleLocationsPropertiesEXT marshalled cont = withCStructExtent2D (maxSampleLocationGridSize (marshalled :: PhysicalDeviceSampleLocationsPropertiesEXT)) (\maxSampleLocationGridSize'' -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceSampleLocationsPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceSampleLocationsPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT pPNext (sampleLocationSampleCounts (marshalled :: PhysicalDeviceSampleLocationsPropertiesEXT)) maxSampleLocationGridSize'' (fromTuple (sampleLocationCoordinateRange (marshalled :: PhysicalDeviceSampleLocationsPropertiesEXT))) (sampleLocationSubPixelBits (marshalled :: PhysicalDeviceSampleLocationsPropertiesEXT)) (boolToBool32 (variableSampleLocations (marshalled :: PhysicalDeviceSampleLocationsPropertiesEXT))))))

-- | A function to read a 'VkPhysicalDeviceSampleLocationsPropertiesEXT' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceSampleLocationsPropertiesEXT'.
fromCStructPhysicalDeviceSampleLocationsPropertiesEXT :: VkPhysicalDeviceSampleLocationsPropertiesEXT -> IO PhysicalDeviceSampleLocationsPropertiesEXT
fromCStructPhysicalDeviceSampleLocationsPropertiesEXT c = PhysicalDeviceSampleLocationsPropertiesEXT <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT)))
                                                                                                     <*> pure (vkSampleLocationSampleCounts (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                                                                                                     <*> (fromCStructExtent2D (vkMaxSampleLocationGridSize (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT)))
                                                                                                     <*> pure (let v = (vkSampleLocationCoordinateRange (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                                                                     , Data.Vector.Storable.Sized.unsafeIndex v 1 ))
                                                                                                     <*> pure (vkSampleLocationSubPixelBits (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT))
                                                                                                     <*> pure (bool32ToBool (vkVariableSampleLocations (c :: VkPhysicalDeviceSampleLocationsPropertiesEXT)))

instance Zero PhysicalDeviceSampleLocationsPropertiesEXT where
  zero = PhysicalDeviceSampleLocationsPropertiesEXT Nothing
                                                    zero
                                                    zero
                                                    (zero, zero)
                                                    zero
                                                    False



-- | VkPipelineSampleLocationsStateCreateInfoEXT - Structure specifying
-- sample locations for a pipeline
--
-- = Description
--
-- Unresolved directive in VkPipelineSampleLocationsStateCreateInfoEXT.txt
-- -
-- include::{generated}\/validity\/structs\/VkPipelineSampleLocationsStateCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data PipelineSampleLocationsStateCreateInfoEXT = PipelineSampleLocationsStateCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsEnable"
  sampleLocationsEnable :: Bool
  , -- No documentation found for Nested "PipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsInfo"
  sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineSampleLocationsStateCreateInfoEXT' and
-- marshal a 'PipelineSampleLocationsStateCreateInfoEXT' into it. The 'VkPipelineSampleLocationsStateCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineSampleLocationsStateCreateInfoEXT :: PipelineSampleLocationsStateCreateInfoEXT -> (VkPipelineSampleLocationsStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineSampleLocationsStateCreateInfoEXT marshalled cont = withCStructSampleLocationsInfoEXT (sampleLocationsInfo (marshalled :: PipelineSampleLocationsStateCreateInfoEXT)) (\sampleLocationsInfo'' -> maybeWith withSomeVkStruct (next (marshalled :: PipelineSampleLocationsStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineSampleLocationsStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT pPNext (boolToBool32 (sampleLocationsEnable (marshalled :: PipelineSampleLocationsStateCreateInfoEXT))) sampleLocationsInfo'')))

-- | A function to read a 'VkPipelineSampleLocationsStateCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'PipelineSampleLocationsStateCreateInfoEXT'.
fromCStructPipelineSampleLocationsStateCreateInfoEXT :: VkPipelineSampleLocationsStateCreateInfoEXT -> IO PipelineSampleLocationsStateCreateInfoEXT
fromCStructPipelineSampleLocationsStateCreateInfoEXT c = PipelineSampleLocationsStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineSampleLocationsStateCreateInfoEXT)))
                                                                                                   <*> pure (bool32ToBool (vkSampleLocationsEnable (c :: VkPipelineSampleLocationsStateCreateInfoEXT)))
                                                                                                   <*> (fromCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (c :: VkPipelineSampleLocationsStateCreateInfoEXT)))

instance Zero PipelineSampleLocationsStateCreateInfoEXT where
  zero = PipelineSampleLocationsStateCreateInfoEXT Nothing
                                                   False
                                                   zero



-- | VkRenderPassSampleLocationsBeginInfoEXT - Structure specifying sample
-- locations to use for the layout transition of custom sample locations
-- compatible depth\/stencil attachments
--
-- = Description
--
-- Unresolved directive in VkRenderPassSampleLocationsBeginInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkRenderPassSampleLocationsBeginInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data RenderPassSampleLocationsBeginInfoEXT = RenderPassSampleLocationsBeginInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pAttachmentInitialSampleLocations"
  attachmentInitialSampleLocations :: Vector AttachmentSampleLocationsEXT
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassSampleLocationsBeginInfoEXT" "pPostSubpassSampleLocations"
  postSubpassSampleLocations :: Vector SubpassSampleLocationsEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRenderPassSampleLocationsBeginInfoEXT' and
-- marshal a 'RenderPassSampleLocationsBeginInfoEXT' into it. The 'VkRenderPassSampleLocationsBeginInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRenderPassSampleLocationsBeginInfoEXT :: RenderPassSampleLocationsBeginInfoEXT -> (VkRenderPassSampleLocationsBeginInfoEXT -> IO a) -> IO a
withCStructRenderPassSampleLocationsBeginInfoEXT marshalled cont = withVec withCStructSubpassSampleLocationsEXT (postSubpassSampleLocations (marshalled :: RenderPassSampleLocationsBeginInfoEXT)) (\pPPostSubpassSampleLocations -> withVec withCStructAttachmentSampleLocationsEXT (attachmentInitialSampleLocations (marshalled :: RenderPassSampleLocationsBeginInfoEXT)) (\pPAttachmentInitialSampleLocations -> maybeWith withSomeVkStruct (next (marshalled :: RenderPassSampleLocationsBeginInfoEXT)) (\pPNext -> cont (VkRenderPassSampleLocationsBeginInfoEXT VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT pPNext (fromIntegral (Data.Vector.length (attachmentInitialSampleLocations (marshalled :: RenderPassSampleLocationsBeginInfoEXT)))) pPAttachmentInitialSampleLocations (fromIntegral (Data.Vector.length (postSubpassSampleLocations (marshalled :: RenderPassSampleLocationsBeginInfoEXT)))) pPPostSubpassSampleLocations))))

-- | A function to read a 'VkRenderPassSampleLocationsBeginInfoEXT' and all additional
-- structures in the pointer chain into a 'RenderPassSampleLocationsBeginInfoEXT'.
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



-- | VkSampleLocationEXT - Structure specifying the coordinates of a sample
-- location
--
-- = Description
--
-- The domain space of the sample location coordinates has an upper-left
-- origin within the pixel in framebuffer space.
--
-- The values specified in a
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkSampleLocationEXT'
-- structure are always clamped to the implementation-dependent sample
-- location coordinate range
-- [@sampleLocationCoordinateRange@[0],@sampleLocationCoordinateRange@[1]]
-- that /can/ be queried by chaining the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT'
-- structure to the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2'.
--
-- Unresolved directive in VkSampleLocationEXT.txt -
-- include::{generated}\/validity\/structs\/VkSampleLocationEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data SampleLocationEXT = SampleLocationEXT
  { -- No documentation found for Nested "SampleLocationEXT" "x"
  x :: CFloat
  , -- No documentation found for Nested "SampleLocationEXT" "y"
  y :: CFloat
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSampleLocationEXT' and
-- marshal a 'SampleLocationEXT' into it. The 'VkSampleLocationEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSampleLocationEXT :: SampleLocationEXT -> (VkSampleLocationEXT -> IO a) -> IO a
withCStructSampleLocationEXT marshalled cont = cont (VkSampleLocationEXT (x (marshalled :: SampleLocationEXT)) (y (marshalled :: SampleLocationEXT)))

-- | A function to read a 'VkSampleLocationEXT' and all additional
-- structures in the pointer chain into a 'SampleLocationEXT'.
fromCStructSampleLocationEXT :: VkSampleLocationEXT -> IO SampleLocationEXT
fromCStructSampleLocationEXT c = SampleLocationEXT <$> pure (vkX (c :: VkSampleLocationEXT))
                                                   <*> pure (vkY (c :: VkSampleLocationEXT))

instance Zero SampleLocationEXT where
  zero = SampleLocationEXT zero
                           zero



-- | VkSampleLocationsInfoEXT - Structure specifying a set of sample
-- locations
--
-- = Description
--
-- This structure /can/ be used either to specify the sample locations to
-- be used for rendering or to specify the set of sample locations an image
-- subresource has been last rendered with for the purposes of layout
-- transitions of depth\/stencil images created with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'.
--
-- The sample locations in @pSampleLocations@ specify
-- @sampleLocationsPerPixel@ number of sample locations for each pixel in
-- the grid of the size specified in @sampleLocationGridSize@. The sample
-- location for sample i at the pixel grid location (x,y) is taken from
-- @pSampleLocations@[(x + y * @sampleLocationGridSize.width@) *
-- @sampleLocationsPerPixel@ + i].
--
-- If the render pass has a fragment density map, the implementation will
-- choose the sample locations for the fragment and the contents of
-- @pSampleLocations@ /may/ be ignored.
--
-- == Valid Usage
--
-- Unresolved directive in VkSampleLocationsInfoEXT.txt -
-- include::{generated}\/validity\/structs\/VkSampleLocationsInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data SampleLocationsInfoEXT = SampleLocationsInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "SampleLocationsInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "sampleLocationsPerPixel"
  sampleLocationsPerPixel :: SampleCountFlagBits
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "sampleLocationGridSize"
  sampleLocationGridSize :: Extent2D
  -- Length valued member elided
  , -- No documentation found for Nested "SampleLocationsInfoEXT" "pSampleLocations"
  sampleLocations :: Vector SampleLocationEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSampleLocationsInfoEXT' and
-- marshal a 'SampleLocationsInfoEXT' into it. The 'VkSampleLocationsInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSampleLocationsInfoEXT :: SampleLocationsInfoEXT -> (VkSampleLocationsInfoEXT -> IO a) -> IO a
withCStructSampleLocationsInfoEXT marshalled cont = withVec withCStructSampleLocationEXT (sampleLocations (marshalled :: SampleLocationsInfoEXT)) (\pPSampleLocations -> withCStructExtent2D (sampleLocationGridSize (marshalled :: SampleLocationsInfoEXT)) (\sampleLocationGridSize'' -> maybeWith withSomeVkStruct (next (marshalled :: SampleLocationsInfoEXT)) (\pPNext -> cont (VkSampleLocationsInfoEXT VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT pPNext (sampleLocationsPerPixel (marshalled :: SampleLocationsInfoEXT)) sampleLocationGridSize'' (fromIntegral (Data.Vector.length (sampleLocations (marshalled :: SampleLocationsInfoEXT)))) pPSampleLocations))))

-- | A function to read a 'VkSampleLocationsInfoEXT' and all additional
-- structures in the pointer chain into a 'SampleLocationsInfoEXT'.
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



-- | VkSubpassSampleLocationsEXT - Structure specifying the sample locations
-- state to use for layout transitions of attachments performed after a
-- given subpass
--
-- = Description
--
-- If the image referenced by the depth\/stencil attachment used in the
-- subpass identified by @subpassIndex@ was not created with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT'
-- or if the subpass does not use a depth\/stencil attachment, and
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
-- is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' then the values specified in
-- @sampleLocationsInfo@ are ignored.
--
-- == Valid Usage
--
-- Unresolved directive in VkSubpassSampleLocationsEXT.txt -
-- include::{generated}\/validity\/structs\/VkSubpassSampleLocationsEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data SubpassSampleLocationsEXT = SubpassSampleLocationsEXT
  { -- No documentation found for Nested "SubpassSampleLocationsEXT" "subpassIndex"
  subpassIndex :: Word32
  , -- No documentation found for Nested "SubpassSampleLocationsEXT" "sampleLocationsInfo"
  sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubpassSampleLocationsEXT' and
-- marshal a 'SubpassSampleLocationsEXT' into it. The 'VkSubpassSampleLocationsEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubpassSampleLocationsEXT :: SubpassSampleLocationsEXT -> (VkSubpassSampleLocationsEXT -> IO a) -> IO a
withCStructSubpassSampleLocationsEXT marshalled cont = withCStructSampleLocationsInfoEXT (sampleLocationsInfo (marshalled :: SubpassSampleLocationsEXT)) (\sampleLocationsInfo'' -> cont (VkSubpassSampleLocationsEXT (subpassIndex (marshalled :: SubpassSampleLocationsEXT)) sampleLocationsInfo''))

-- | A function to read a 'VkSubpassSampleLocationsEXT' and all additional
-- structures in the pointer chain into a 'SubpassSampleLocationsEXT'.
fromCStructSubpassSampleLocationsEXT :: VkSubpassSampleLocationsEXT -> IO SubpassSampleLocationsEXT
fromCStructSubpassSampleLocationsEXT c = SubpassSampleLocationsEXT <$> pure (vkSubpassIndex (c :: VkSubpassSampleLocationsEXT))
                                                                   <*> (fromCStructSampleLocationsInfoEXT (vkSampleLocationsInfo (c :: VkSubpassSampleLocationsEXT)))

instance Zero SubpassSampleLocationsEXT where
  zero = SubpassSampleLocationsEXT zero
                                   zero



-- | vkCmdSetSampleLocationsEXT - Set the dynamic sample locations state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pSampleLocationsInfo@ is the sample locations state to set.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT'
--     dynamic state enabled
--
-- -   The @sampleLocationsPerPixel@ member of @pSampleLocationsInfo@
--     /must/ equal the @rasterizationSamples@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
--     structure the bound graphics pipeline has been created with
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
--     is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' then the current render
--     pass /must/ have been begun by specifying a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkRenderPassSampleLocationsBeginInfoEXT'
--     structure whose @pPostSubpassSampleLocations@ member contains an
--     element with a @subpassIndex@ matching the current subpass index and
--     the @sampleLocationsInfo@ member of that element /must/ match the
--     sample locations state pointed to by @pSampleLocationsInfo@
--
-- Unresolved directive in vkCmdSetSampleLocationsEXT.txt -
-- include::{generated}\/validity\/protos\/vkCmdSetSampleLocationsEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
cmdSetSampleLocationsEXT :: CommandBuffer ->  SampleLocationsInfoEXT ->  IO ()
cmdSetSampleLocationsEXT = \(CommandBuffer commandBuffer' commandTable) -> \sampleLocationsInfo' -> (\marshalled -> withCStructSampleLocationsInfoEXT marshalled . flip with) sampleLocationsInfo' (\pSampleLocationsInfo' -> vkCmdSetSampleLocationsEXT commandTable commandBuffer' pSampleLocationsInfo' *> (pure ()))


-- | vkGetPhysicalDeviceMultisamplePropertiesEXT - Report sample count
-- specific multisampling capabilities of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     additional multisampling capabilities.
--
-- -   @samples@ is the sample count to query the capabilities for.
--
-- -   @pMultisampleProperties@ is a pointer to a structure of type
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkMultisamplePropertiesEXT',
--     in which information about the additional multisampling capabilities
--     specific to the sample count is returned.
--
-- = Description
--
-- Unresolved directive in vkGetPhysicalDeviceMultisamplePropertiesEXT.txt
-- -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceMultisamplePropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceMultisamplePropertiesEXT :: PhysicalDevice ->  SampleCountFlagBits ->  IO (MultisamplePropertiesEXT)
getPhysicalDeviceMultisamplePropertiesEXT = \(PhysicalDevice physicalDevice' commandTable) -> \samples' -> alloca (\pMultisampleProperties' -> vkGetPhysicalDeviceMultisamplePropertiesEXT commandTable physicalDevice' samples' pMultisampleProperties' *> ((fromCStructMultisamplePropertiesEXT <=< peek) pMultisampleProperties'))
