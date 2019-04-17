{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  , DisplayModeCreateFlagsKHR
  , withCStructDisplayModeCreateInfoKHR
  , fromCStructDisplayModeCreateInfoKHR
  , DisplayModeCreateInfoKHR(..)
  , DisplayModeKHR
  , withCStructDisplayModeParametersKHR
  , fromCStructDisplayModeParametersKHR
  , DisplayModeParametersKHR(..)
  , withCStructDisplayModePropertiesKHR
  , fromCStructDisplayModePropertiesKHR
  , DisplayModePropertiesKHR(..)
  , DisplayPlaneAlphaFlagBitsKHR
  , DisplayPlaneAlphaFlagsKHR
  , withCStructDisplayPlaneCapabilitiesKHR
  , fromCStructDisplayPlaneCapabilitiesKHR
  , DisplayPlaneCapabilitiesKHR(..)
  , withCStructDisplayPlanePropertiesKHR
  , fromCStructDisplayPlanePropertiesKHR
  , DisplayPlanePropertiesKHR(..)
  , withCStructDisplayPropertiesKHR
  , fromCStructDisplayPropertiesKHR
  , DisplayPropertiesKHR(..)
  , DisplaySurfaceCreateFlagsKHR
  , withCStructDisplaySurfaceCreateInfoKHR
  , fromCStructDisplaySurfaceCreateInfoKHR
  , DisplaySurfaceCreateInfoKHR(..)
  , createDisplayModeKHR
  , createDisplayPlaneSurfaceKHR
  , getNumDisplayModePropertiesKHR
  , getDisplayModePropertiesKHR
  , getAllDisplayModePropertiesKHR
  , getDisplayPlaneCapabilitiesKHR
  , getNumDisplayPlaneSupportedDisplaysKHR
  , getDisplayPlaneSupportedDisplaysKHR
  , getAllDisplayPlaneSupportedDisplaysKHR
  , getNumPhysicalDeviceDisplayPlanePropertiesKHR
  , getPhysicalDeviceDisplayPlanePropertiesKHR
  , getAllPhysicalDeviceDisplayPlanePropertiesKHR
  , getNumPhysicalDeviceDisplayPropertiesKHR
  , getPhysicalDeviceDisplayPropertiesKHR
  , getAllPhysicalDeviceDisplayPropertiesKHR
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  , pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.ByteString
  ( ByteString
  , packCString
  , useAsCString
  )
import qualified Data.ByteString
  ( empty
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
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
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createDisplayModeKHR
  , createDisplayPlaneSurfaceKHR
  , getDisplayModePropertiesKHR
  , getDisplayPlaneCapabilitiesKHR
  , getDisplayPlaneSupportedDisplaysKHR
  , getPhysicalDeviceDisplayPlanePropertiesKHR
  , getPhysicalDeviceDisplayPropertiesKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateFlagsKHR(..)
  , VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModeParametersKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneAlphaFlagBitsKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateFlagsKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , VkDisplayKHR
  , VkDisplayModeKHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , Offset2D(..)
  , fromCStructExtent2D
  , fromCStructOffset2D
  , withCStructExtent2D
  , withCStructOffset2D
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , SurfaceTransformFlagsKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DISPLAY_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR
  )


-- No documentation found for TopLevel "DisplayKHR"
type DisplayKHR = VkDisplayKHR
-- No documentation found for TopLevel "DisplayModeCreateFlagsKHR"
type DisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR
-- No documentation found for TopLevel "DisplayModeCreateInfoKHR"
data DisplayModeCreateInfoKHR = DisplayModeCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplayModeCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayModeCreateInfoKHR" "flags"
  vkFlags :: DisplayModeCreateFlagsKHR
  , -- No documentation found for Nested "DisplayModeCreateInfoKHR" "parameters"
  vkParameters :: DisplayModeParametersKHR
  }
  deriving (Show, Eq)
withCStructDisplayModeCreateInfoKHR :: DisplayModeCreateInfoKHR -> (VkDisplayModeCreateInfoKHR -> IO a) -> IO a
withCStructDisplayModeCreateInfoKHR from cont = withCStructDisplayModeParametersKHR (vkParameters (from :: DisplayModeCreateInfoKHR)) (\arameters -> maybeWith withSomeVkStruct (vkPNext (from :: DisplayModeCreateInfoKHR)) (\pPNext -> cont (VkDisplayModeCreateInfoKHR VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR pPNext (vkFlags (from :: DisplayModeCreateInfoKHR)) arameters)))
fromCStructDisplayModeCreateInfoKHR :: VkDisplayModeCreateInfoKHR -> IO DisplayModeCreateInfoKHR
fromCStructDisplayModeCreateInfoKHR c = DisplayModeCreateInfoKHR <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayModeCreateInfoKHR)))
                                                                 <*> pure (vkFlags (c :: VkDisplayModeCreateInfoKHR))
                                                                 <*> (fromCStructDisplayModeParametersKHR (vkParameters (c :: VkDisplayModeCreateInfoKHR)))
instance Zero DisplayModeCreateInfoKHR where
  zero = DisplayModeCreateInfoKHR Nothing
                                  zero
                                  zero
-- No documentation found for TopLevel "DisplayModeKHR"
type DisplayModeKHR = VkDisplayModeKHR
-- No documentation found for TopLevel "DisplayModeParametersKHR"
data DisplayModeParametersKHR = DisplayModeParametersKHR
  { -- No documentation found for Nested "DisplayModeParametersKHR" "visibleRegion"
  vkVisibleRegion :: Extent2D
  , -- No documentation found for Nested "DisplayModeParametersKHR" "refreshRate"
  vkRefreshRate :: Word32
  }
  deriving (Show, Eq)
withCStructDisplayModeParametersKHR :: DisplayModeParametersKHR -> (VkDisplayModeParametersKHR -> IO a) -> IO a
withCStructDisplayModeParametersKHR from cont = withCStructExtent2D (vkVisibleRegion (from :: DisplayModeParametersKHR)) (\visibleRegion -> cont (VkDisplayModeParametersKHR visibleRegion (vkRefreshRate (from :: DisplayModeParametersKHR))))
fromCStructDisplayModeParametersKHR :: VkDisplayModeParametersKHR -> IO DisplayModeParametersKHR
fromCStructDisplayModeParametersKHR c = DisplayModeParametersKHR <$> (fromCStructExtent2D (vkVisibleRegion (c :: VkDisplayModeParametersKHR)))
                                                                 <*> pure (vkRefreshRate (c :: VkDisplayModeParametersKHR))
instance Zero DisplayModeParametersKHR where
  zero = DisplayModeParametersKHR zero
                                  zero
-- No documentation found for TopLevel "DisplayModePropertiesKHR"
data DisplayModePropertiesKHR = DisplayModePropertiesKHR
  { -- No documentation found for Nested "DisplayModePropertiesKHR" "displayMode"
  vkDisplayMode :: DisplayModeKHR
  , -- No documentation found for Nested "DisplayModePropertiesKHR" "parameters"
  vkParameters :: DisplayModeParametersKHR
  }
  deriving (Show, Eq)
withCStructDisplayModePropertiesKHR :: DisplayModePropertiesKHR -> (VkDisplayModePropertiesKHR -> IO a) -> IO a
withCStructDisplayModePropertiesKHR from cont = withCStructDisplayModeParametersKHR (vkParameters (from :: DisplayModePropertiesKHR)) (\arameters -> cont (VkDisplayModePropertiesKHR (vkDisplayMode (from :: DisplayModePropertiesKHR)) arameters))
fromCStructDisplayModePropertiesKHR :: VkDisplayModePropertiesKHR -> IO DisplayModePropertiesKHR
fromCStructDisplayModePropertiesKHR c = DisplayModePropertiesKHR <$> pure (vkDisplayMode (c :: VkDisplayModePropertiesKHR))
                                                                 <*> (fromCStructDisplayModeParametersKHR (vkParameters (c :: VkDisplayModePropertiesKHR)))
instance Zero DisplayModePropertiesKHR where
  zero = DisplayModePropertiesKHR zero
                                  zero
-- No documentation found for TopLevel "DisplayPlaneAlphaFlagBitsKHR"
type DisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR
-- No documentation found for TopLevel "DisplayPlaneAlphaFlagsKHR"
type DisplayPlaneAlphaFlagsKHR = DisplayPlaneAlphaFlagBitsKHR
-- No documentation found for TopLevel "DisplayPlaneCapabilitiesKHR"
data DisplayPlaneCapabilitiesKHR = DisplayPlaneCapabilitiesKHR
  { -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "supportedAlpha"
  vkSupportedAlpha :: DisplayPlaneAlphaFlagsKHR
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "minSrcPosition"
  vkMinSrcPosition :: Offset2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "maxSrcPosition"
  vkMaxSrcPosition :: Offset2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "minSrcExtent"
  vkMinSrcExtent :: Extent2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "maxSrcExtent"
  vkMaxSrcExtent :: Extent2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "minDstPosition"
  vkMinDstPosition :: Offset2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "maxDstPosition"
  vkMaxDstPosition :: Offset2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "minDstExtent"
  vkMinDstExtent :: Extent2D
  , -- No documentation found for Nested "DisplayPlaneCapabilitiesKHR" "maxDstExtent"
  vkMaxDstExtent :: Extent2D
  }
  deriving (Show, Eq)
withCStructDisplayPlaneCapabilitiesKHR :: DisplayPlaneCapabilitiesKHR -> (VkDisplayPlaneCapabilitiesKHR -> IO a) -> IO a
withCStructDisplayPlaneCapabilitiesKHR from cont = withCStructExtent2D (vkMaxDstExtent (from :: DisplayPlaneCapabilitiesKHR)) (\maxDstExtent -> withCStructExtent2D (vkMinDstExtent (from :: DisplayPlaneCapabilitiesKHR)) (\minDstExtent -> withCStructOffset2D (vkMaxDstPosition (from :: DisplayPlaneCapabilitiesKHR)) (\maxDstPosition -> withCStructOffset2D (vkMinDstPosition (from :: DisplayPlaneCapabilitiesKHR)) (\minDstPosition -> withCStructExtent2D (vkMaxSrcExtent (from :: DisplayPlaneCapabilitiesKHR)) (\maxSrcExtent -> withCStructExtent2D (vkMinSrcExtent (from :: DisplayPlaneCapabilitiesKHR)) (\minSrcExtent -> withCStructOffset2D (vkMaxSrcPosition (from :: DisplayPlaneCapabilitiesKHR)) (\maxSrcPosition -> withCStructOffset2D (vkMinSrcPosition (from :: DisplayPlaneCapabilitiesKHR)) (\minSrcPosition -> cont (VkDisplayPlaneCapabilitiesKHR (vkSupportedAlpha (from :: DisplayPlaneCapabilitiesKHR)) minSrcPosition maxSrcPosition minSrcExtent maxSrcExtent minDstPosition maxDstPosition minDstExtent maxDstExtent)))))))))
fromCStructDisplayPlaneCapabilitiesKHR :: VkDisplayPlaneCapabilitiesKHR -> IO DisplayPlaneCapabilitiesKHR
fromCStructDisplayPlaneCapabilitiesKHR c = DisplayPlaneCapabilitiesKHR <$> pure (vkSupportedAlpha (c :: VkDisplayPlaneCapabilitiesKHR))
                                                                       <*> (fromCStructOffset2D (vkMinSrcPosition (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructOffset2D (vkMaxSrcPosition (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructExtent2D (vkMinSrcExtent (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructExtent2D (vkMaxSrcExtent (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructOffset2D (vkMinDstPosition (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructOffset2D (vkMaxDstPosition (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructExtent2D (vkMinDstExtent (c :: VkDisplayPlaneCapabilitiesKHR)))
                                                                       <*> (fromCStructExtent2D (vkMaxDstExtent (c :: VkDisplayPlaneCapabilitiesKHR)))
instance Zero DisplayPlaneCapabilitiesKHR where
  zero = DisplayPlaneCapabilitiesKHR zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
-- No documentation found for TopLevel "DisplayPlanePropertiesKHR"
data DisplayPlanePropertiesKHR = DisplayPlanePropertiesKHR
  { -- No documentation found for Nested "DisplayPlanePropertiesKHR" "currentDisplay"
  vkCurrentDisplay :: DisplayKHR
  , -- No documentation found for Nested "DisplayPlanePropertiesKHR" "currentStackIndex"
  vkCurrentStackIndex :: Word32
  }
  deriving (Show, Eq)
withCStructDisplayPlanePropertiesKHR :: DisplayPlanePropertiesKHR -> (VkDisplayPlanePropertiesKHR -> IO a) -> IO a
withCStructDisplayPlanePropertiesKHR from cont = cont (VkDisplayPlanePropertiesKHR (vkCurrentDisplay (from :: DisplayPlanePropertiesKHR)) (vkCurrentStackIndex (from :: DisplayPlanePropertiesKHR)))
fromCStructDisplayPlanePropertiesKHR :: VkDisplayPlanePropertiesKHR -> IO DisplayPlanePropertiesKHR
fromCStructDisplayPlanePropertiesKHR c = DisplayPlanePropertiesKHR <$> pure (vkCurrentDisplay (c :: VkDisplayPlanePropertiesKHR))
                                                                   <*> pure (vkCurrentStackIndex (c :: VkDisplayPlanePropertiesKHR))
instance Zero DisplayPlanePropertiesKHR where
  zero = DisplayPlanePropertiesKHR zero
                                   zero
-- No documentation found for TopLevel "DisplayPropertiesKHR"
data DisplayPropertiesKHR = DisplayPropertiesKHR
  { -- No documentation found for Nested "DisplayPropertiesKHR" "display"
  vkDisplay :: DisplayKHR
  , -- No documentation found for Nested "DisplayPropertiesKHR" "displayName"
  vkDisplayName :: ByteString
  , -- No documentation found for Nested "DisplayPropertiesKHR" "physicalDimensions"
  vkPhysicalDimensions :: Extent2D
  , -- No documentation found for Nested "DisplayPropertiesKHR" "physicalResolution"
  vkPhysicalResolution :: Extent2D
  , -- No documentation found for Nested "DisplayPropertiesKHR" "supportedTransforms"
  vkSupportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "DisplayPropertiesKHR" "planeReorderPossible"
  vkPlaneReorderPossible :: Bool
  , -- No documentation found for Nested "DisplayPropertiesKHR" "persistentContent"
  vkPersistentContent :: Bool
  }
  deriving (Show, Eq)
withCStructDisplayPropertiesKHR :: DisplayPropertiesKHR -> (VkDisplayPropertiesKHR -> IO a) -> IO a
withCStructDisplayPropertiesKHR from cont = withCStructExtent2D (vkPhysicalResolution (from :: DisplayPropertiesKHR)) (\hysicalResolution -> withCStructExtent2D (vkPhysicalDimensions (from :: DisplayPropertiesKHR)) (\hysicalDimensions -> useAsCString (vkDisplayName (from :: DisplayPropertiesKHR)) (\pDisplayName -> cont (VkDisplayPropertiesKHR (vkDisplay (from :: DisplayPropertiesKHR)) pDisplayName hysicalDimensions hysicalResolution (vkSupportedTransforms (from :: DisplayPropertiesKHR)) (boolToBool32 (vkPlaneReorderPossible (from :: DisplayPropertiesKHR))) (boolToBool32 (vkPersistentContent (from :: DisplayPropertiesKHR)))))))
fromCStructDisplayPropertiesKHR :: VkDisplayPropertiesKHR -> IO DisplayPropertiesKHR
fromCStructDisplayPropertiesKHR c = DisplayPropertiesKHR <$> pure (vkDisplay (c :: VkDisplayPropertiesKHR))
                                                         <*> packCString (vkDisplayName (c :: VkDisplayPropertiesKHR))
                                                         <*> (fromCStructExtent2D (vkPhysicalDimensions (c :: VkDisplayPropertiesKHR)))
                                                         <*> (fromCStructExtent2D (vkPhysicalResolution (c :: VkDisplayPropertiesKHR)))
                                                         <*> pure (vkSupportedTransforms (c :: VkDisplayPropertiesKHR))
                                                         <*> pure (bool32ToBool (vkPlaneReorderPossible (c :: VkDisplayPropertiesKHR)))
                                                         <*> pure (bool32ToBool (vkPersistentContent (c :: VkDisplayPropertiesKHR)))
instance Zero DisplayPropertiesKHR where
  zero = DisplayPropertiesKHR zero
                              Data.ByteString.empty
                              zero
                              zero
                              zero
                              False
                              False
-- No documentation found for TopLevel "DisplaySurfaceCreateFlagsKHR"
type DisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR
-- No documentation found for TopLevel "DisplaySurfaceCreateInfoKHR"
data DisplaySurfaceCreateInfoKHR = DisplaySurfaceCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "flags"
  vkFlags :: DisplaySurfaceCreateFlagsKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "displayMode"
  vkDisplayMode :: DisplayModeKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "planeIndex"
  vkPlaneIndex :: Word32
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "planeStackIndex"
  vkPlaneStackIndex :: Word32
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "transform"
  vkTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "globalAlpha"
  vkGlobalAlpha :: CFloat
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "alphaMode"
  vkAlphaMode :: DisplayPlaneAlphaFlagBitsKHR
  , -- No documentation found for Nested "DisplaySurfaceCreateInfoKHR" "imageExtent"
  vkImageExtent :: Extent2D
  }
  deriving (Show, Eq)
withCStructDisplaySurfaceCreateInfoKHR :: DisplaySurfaceCreateInfoKHR -> (VkDisplaySurfaceCreateInfoKHR -> IO a) -> IO a
withCStructDisplaySurfaceCreateInfoKHR from cont = withCStructExtent2D (vkImageExtent (from :: DisplaySurfaceCreateInfoKHR)) (\imageExtent -> maybeWith withSomeVkStruct (vkPNext (from :: DisplaySurfaceCreateInfoKHR)) (\pPNext -> cont (VkDisplaySurfaceCreateInfoKHR VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR pPNext (vkFlags (from :: DisplaySurfaceCreateInfoKHR)) (vkDisplayMode (from :: DisplaySurfaceCreateInfoKHR)) (vkPlaneIndex (from :: DisplaySurfaceCreateInfoKHR)) (vkPlaneStackIndex (from :: DisplaySurfaceCreateInfoKHR)) (vkTransform (from :: DisplaySurfaceCreateInfoKHR)) (vkGlobalAlpha (from :: DisplaySurfaceCreateInfoKHR)) (vkAlphaMode (from :: DisplaySurfaceCreateInfoKHR)) imageExtent)))
fromCStructDisplaySurfaceCreateInfoKHR :: VkDisplaySurfaceCreateInfoKHR -> IO DisplaySurfaceCreateInfoKHR
fromCStructDisplaySurfaceCreateInfoKHR c = DisplaySurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplaySurfaceCreateInfoKHR)))
                                                                       <*> pure (vkFlags (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkDisplayMode (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkPlaneIndex (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkPlaneStackIndex (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkTransform (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkGlobalAlpha (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> pure (vkAlphaMode (c :: VkDisplaySurfaceCreateInfoKHR))
                                                                       <*> (fromCStructExtent2D (vkImageExtent (c :: VkDisplaySurfaceCreateInfoKHR)))
instance Zero DisplaySurfaceCreateInfoKHR where
  zero = DisplaySurfaceCreateInfoKHR Nothing
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero
                                     zero

-- | Wrapper for 'vkCreateDisplayModeKHR'
createDisplayModeKHR :: PhysicalDevice ->  DisplayKHR ->  DisplayModeCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (DisplayModeKHR)
createDisplayModeKHR = \(PhysicalDevice physicalDevice commandTable) -> \display -> \createInfo -> \allocator -> alloca (\pMode -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDisplayModeCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createDisplayModeKHR commandTable physicalDevice display pCreateInfo pAllocator pMode >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pMode)))))

-- | Wrapper for 'vkCreateDisplayPlaneSurfaceKHR'
createDisplayPlaneSurfaceKHR :: Instance ->  DisplaySurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createDisplayPlaneSurfaceKHR = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDisplaySurfaceCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createDisplayPlaneSurfaceKHR commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))

-- | Wrapper for 'vkGetDisplayModePropertiesKHR'
getNumDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  IO (VkResult, Word32)
getNumDisplayModePropertiesKHR = \(PhysicalDevice physicalDevice commandTable) -> \display -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getDisplayModePropertiesKHR commandTable physicalDevice display pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for 'vkGetDisplayModePropertiesKHR'
getDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  Word32 ->  IO (VkResult, Vector DisplayModePropertiesKHR)
getDisplayModePropertiesKHR = \(PhysicalDevice physicalDevice commandTable) -> \display -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getDisplayModePropertiesKHR commandTable physicalDevice display pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayModePropertiesKHR <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumDisplayModePropertiesKHR' to get the number of return values, then use that
-- number to call 'getDisplayModePropertiesKHR' to get all the values.
getAllDisplayModePropertiesKHR :: PhysicalDevice ->  DisplayKHR ->  IO (Vector DisplayModePropertiesKHR)
getAllDisplayModePropertiesKHR physicalDevice display =
  snd <$> getNumDisplayModePropertiesKHR physicalDevice display
    >>= \num -> snd <$> getDisplayModePropertiesKHR physicalDevice display num


-- | Wrapper for 'vkGetDisplayPlaneCapabilitiesKHR'
getDisplayPlaneCapabilitiesKHR :: PhysicalDevice ->  DisplayModeKHR ->  Word32 ->  IO (DisplayPlaneCapabilitiesKHR)
getDisplayPlaneCapabilitiesKHR = \(PhysicalDevice physicalDevice commandTable) -> \mode -> \planeIndex -> alloca (\pCapabilities -> Graphics.Vulkan.C.Dynamic.getDisplayPlaneCapabilitiesKHR commandTable physicalDevice mode planeIndex pCapabilities >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructDisplayPlaneCapabilitiesKHR <=< peek) pCapabilities)))

-- | Wrapper for 'vkGetDisplayPlaneSupportedDisplaysKHR'
getNumDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Word32)
getNumDisplayPlaneSupportedDisplaysKHR = \(PhysicalDevice physicalDevice commandTable) -> \planeIndex -> alloca (\pDisplayCount -> Graphics.Vulkan.C.Dynamic.getDisplayPlaneSupportedDisplaysKHR commandTable physicalDevice planeIndex pDisplayCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pDisplayCount)))

-- | Wrapper for 'vkGetDisplayPlaneSupportedDisplaysKHR'
getDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  Word32 ->  IO (VkResult, Vector DisplayKHR)
getDisplayPlaneSupportedDisplaysKHR = \(PhysicalDevice physicalDevice commandTable) -> \planeIndex -> \displayCount -> allocaArray (fromIntegral displayCount) (\pDisplays -> with displayCount (\pDisplayCount -> Graphics.Vulkan.C.Dynamic.getDisplayPlaneSupportedDisplaysKHR commandTable physicalDevice planeIndex pDisplayCount pDisplays >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM (peekElemOff pDisplays) =<< (fromIntegral <$> (peek pDisplayCount)))))))
-- | Call 'getNumDisplayPlaneSupportedDisplaysKHR' to get the number of return values, then use that
-- number to call 'getDisplayPlaneSupportedDisplaysKHR' to get all the values.
getAllDisplayPlaneSupportedDisplaysKHR :: PhysicalDevice ->  Word32 ->  IO (Vector DisplayKHR)
getAllDisplayPlaneSupportedDisplaysKHR physicalDevice planeIndex =
  snd <$> getNumDisplayPlaneSupportedDisplaysKHR physicalDevice planeIndex
    >>= \num -> snd <$> getDisplayPlaneSupportedDisplaysKHR physicalDevice planeIndex num


-- | Wrapper for 'vkGetPhysicalDeviceDisplayPlanePropertiesKHR'
getNumPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPlanePropertiesKHR = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceDisplayPlanePropertiesKHR commandTable physicalDevice pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for 'vkGetPhysicalDeviceDisplayPlanePropertiesKHR'
getPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPlanePropertiesKHR)
getPhysicalDeviceDisplayPlanePropertiesKHR = \(PhysicalDevice physicalDevice commandTable) -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceDisplayPlanePropertiesKHR commandTable physicalDevice pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayPlanePropertiesKHR <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumPhysicalDeviceDisplayPlanePropertiesKHR' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceDisplayPlanePropertiesKHR' to get all the values.
getAllPhysicalDeviceDisplayPlanePropertiesKHR :: PhysicalDevice ->  IO (Vector DisplayPlanePropertiesKHR)
getAllPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice =
  snd <$> getNumPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice
    >>= \num -> snd <$> getPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice num


-- | Wrapper for 'vkGetPhysicalDeviceDisplayPropertiesKHR'
getNumPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceDisplayPropertiesKHR = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceDisplayPropertiesKHR commandTable physicalDevice pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for 'vkGetPhysicalDeviceDisplayPropertiesKHR'
getPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector DisplayPropertiesKHR)
getPhysicalDeviceDisplayPropertiesKHR = \(PhysicalDevice physicalDevice commandTable) -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceDisplayPropertiesKHR commandTable physicalDevice pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructDisplayPropertiesKHR <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumPhysicalDeviceDisplayPropertiesKHR' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceDisplayPropertiesKHR' to get all the values.
getAllPhysicalDeviceDisplayPropertiesKHR :: PhysicalDevice ->  IO (Vector DisplayPropertiesKHR)
getAllPhysicalDeviceDisplayPropertiesKHR physicalDevice =
  snd <$> getNumPhysicalDeviceDisplayPropertiesKHR physicalDevice
    >>= \num -> snd <$> getPhysicalDeviceDisplayPropertiesKHR physicalDevice num

