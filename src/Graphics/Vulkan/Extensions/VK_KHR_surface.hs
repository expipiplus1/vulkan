{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_surface
  ( ColorSpaceKHR
  , pattern COLOR_SPACE_SRGB_NONLINEAR_KHR
  , pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
  , pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
  , pattern COLOR_SPACE_DCI_P3_LINEAR_EXT
  , pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT
  , pattern COLOR_SPACE_BT709_LINEAR_EXT
  , pattern COLOR_SPACE_BT709_NONLINEAR_EXT
  , pattern COLOR_SPACE_BT2020_LINEAR_EXT
  , pattern COLOR_SPACE_HDR10_ST2084_EXT
  , pattern COLOR_SPACE_DOLBYVISION_EXT
  , pattern COLOR_SPACE_HDR10_HLG_EXT
  , pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT
  , pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
  , pattern COLOR_SPACE_PASS_THROUGH_EXT
  , pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT
  , pattern COLOR_SPACE_DISPLAY_NATIVE_AMD
  , CompositeAlphaFlagBitsKHR
  , pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  , pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
  , pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
  , pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR
  , CompositeAlphaFlagsKHR
  , PresentModeKHR
  , pattern PRESENT_MODE_IMMEDIATE_KHR
  , pattern PRESENT_MODE_MAILBOX_KHR
  , pattern PRESENT_MODE_FIFO_KHR
  , pattern PRESENT_MODE_FIFO_RELAXED_KHR
  , pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  , pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , pattern SURFACE_TRANSFORM_IDENTITY_BIT_KHR
  , pattern SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
  , pattern SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
  , pattern SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
  , pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
  , pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
  , pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
  , pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
  , pattern SURFACE_TRANSFORM_INHERIT_BIT_KHR
  , SurfaceTransformFlagsKHR
  , destroySurfaceKHR
#if defined(VK_USE_PLATFORM_GGP)
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , getNumPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , getAllPhysicalDeviceSurfaceFormatsKHR
#endif
  , getNumPhysicalDeviceSurfacePresentModesKHR
  , getPhysicalDeviceSurfacePresentModesKHR
  , getAllPhysicalDeviceSurfacePresentModesKHR
  , getPhysicalDeviceSurfaceSupportKHR
  , pattern KHR_SURFACE_EXTENSION_NAME
  , pattern KHR_SURFACE_SPEC_VERSION
  , pattern ERROR_SURFACE_LOST_KHR
  , pattern ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern OBJECT_TYPE_SURFACE_KHR
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
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
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( pattern VK_COLOR_SPACE_DISPLAY_NATIVE_AMD
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_swapchain_colorspace
  ( pattern VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT
  , pattern VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_BT2020_LINEAR_EXT
  , pattern VK_COLOR_SPACE_BT709_LINEAR_EXT
  , pattern VK_COLOR_SPACE_BT709_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DCI_P3_LINEAR_EXT
  , pattern VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DOLBYVISION_EXT
  , pattern VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
  , pattern VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_HDR10_HLG_EXT
  , pattern VK_COLOR_SPACE_HDR10_ST2084_EXT
  , pattern VK_COLOR_SPACE_PASS_THROUGH_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  , VkCompositeAlphaFlagBitsKHR(..)
  , VkPresentModeKHR(..)
  , VkSurfaceTransformFlagBitsKHR(..)
  , VkSurfaceKHR
  , vkDestroySurfaceKHR
  , vkGetPhysicalDeviceSurfacePresentModesKHR
  , vkGetPhysicalDeviceSurfaceSupportKHR
  , pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  , pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_PRESENT_MODE_FIFO_KHR
  , pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR
  , pattern VK_PRESENT_MODE_IMMEDIATE_KHR
  , pattern VK_PRESENT_MODE_MAILBOX_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( vkGetPhysicalDeviceSurfaceFormatsKHR
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  , ImageUsageFlags
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern ERROR_SURFACE_LOST_KHR
  , pattern OBJECT_TYPE_SURFACE_KHR
  )


-- No documentation found for TopLevel "ColorSpaceKHR"
type ColorSpaceKHR = VkColorSpaceKHR


{-# complete COLOR_SPACE_SRGB_NONLINEAR_KHR, COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT, COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT, COLOR_SPACE_DCI_P3_LINEAR_EXT, COLOR_SPACE_DCI_P3_NONLINEAR_EXT, COLOR_SPACE_BT709_LINEAR_EXT, COLOR_SPACE_BT709_NONLINEAR_EXT, COLOR_SPACE_BT2020_LINEAR_EXT, COLOR_SPACE_HDR10_ST2084_EXT, COLOR_SPACE_DOLBYVISION_EXT, COLOR_SPACE_HDR10_HLG_EXT, COLOR_SPACE_ADOBERGB_LINEAR_EXT, COLOR_SPACE_ADOBERGB_NONLINEAR_EXT, COLOR_SPACE_PASS_THROUGH_EXT, COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT, COLOR_SPACE_DISPLAY_NATIVE_AMD :: ColorSpaceKHR #-}


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_SRGB_NONLINEAR_KHR"
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR = VK_COLOR_SPACE_SRGB_NONLINEAR_KHR


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT"
pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT = VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT"
pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT = VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DCI_P3_LINEAR_EXT"
pattern COLOR_SPACE_DCI_P3_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DCI_P3_LINEAR_EXT = VK_COLOR_SPACE_DCI_P3_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DCI_P3_NONLINEAR_EXT"
pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT = VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_BT709_LINEAR_EXT"
pattern COLOR_SPACE_BT709_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_BT709_LINEAR_EXT = VK_COLOR_SPACE_BT709_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_BT709_NONLINEAR_EXT"
pattern COLOR_SPACE_BT709_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_BT709_NONLINEAR_EXT = VK_COLOR_SPACE_BT709_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_BT2020_LINEAR_EXT"
pattern COLOR_SPACE_BT2020_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_BT2020_LINEAR_EXT = VK_COLOR_SPACE_BT2020_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_HDR10_ST2084_EXT"
pattern COLOR_SPACE_HDR10_ST2084_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_HDR10_ST2084_EXT = VK_COLOR_SPACE_HDR10_ST2084_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DOLBYVISION_EXT"
pattern COLOR_SPACE_DOLBYVISION_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DOLBYVISION_EXT = VK_COLOR_SPACE_DOLBYVISION_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_HDR10_HLG_EXT"
pattern COLOR_SPACE_HDR10_HLG_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_HDR10_HLG_EXT = VK_COLOR_SPACE_HDR10_HLG_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_ADOBERGB_LINEAR_EXT"
pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT = VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_ADOBERGB_NONLINEAR_EXT"
pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT = VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_PASS_THROUGH_EXT"
pattern COLOR_SPACE_PASS_THROUGH_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_PASS_THROUGH_EXT = VK_COLOR_SPACE_PASS_THROUGH_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT"
pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT = VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT


-- No documentation found for Nested "ColorSpaceKHR" "COLOR_SPACE_DISPLAY_NATIVE_AMD"
pattern COLOR_SPACE_DISPLAY_NATIVE_AMD :: (a ~ ColorSpaceKHR) => a
pattern COLOR_SPACE_DISPLAY_NATIVE_AMD = VK_COLOR_SPACE_DISPLAY_NATIVE_AMD

-- No documentation found for TopLevel "CompositeAlphaFlagBitsKHR"
type CompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR


{-# complete COMPOSITE_ALPHA_OPAQUE_BIT_KHR, COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR, COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR, COMPOSITE_ALPHA_INHERIT_BIT_KHR :: CompositeAlphaFlagBitsKHR #-}


-- No documentation found for Nested "CompositeAlphaFlagBitsKHR" "COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR :: (a ~ CompositeAlphaFlagBitsKHR) => a
pattern COMPOSITE_ALPHA_OPAQUE_BIT_KHR = VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR


-- No documentation found for Nested "CompositeAlphaFlagBitsKHR" "COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR :: (a ~ CompositeAlphaFlagBitsKHR) => a
pattern COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR


-- No documentation found for Nested "CompositeAlphaFlagBitsKHR" "COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR :: (a ~ CompositeAlphaFlagBitsKHR) => a
pattern COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR


-- No documentation found for Nested "CompositeAlphaFlagBitsKHR" "COMPOSITE_ALPHA_INHERIT_BIT_KHR"
pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR :: (a ~ CompositeAlphaFlagBitsKHR) => a
pattern COMPOSITE_ALPHA_INHERIT_BIT_KHR = VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR

-- No documentation found for TopLevel "CompositeAlphaFlagsKHR"
type CompositeAlphaFlagsKHR = CompositeAlphaFlagBitsKHR

-- No documentation found for TopLevel "PresentModeKHR"
type PresentModeKHR = VkPresentModeKHR


{-# complete PRESENT_MODE_IMMEDIATE_KHR, PRESENT_MODE_MAILBOX_KHR, PRESENT_MODE_FIFO_KHR, PRESENT_MODE_FIFO_RELAXED_KHR, PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR, PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR :: PresentModeKHR #-}


-- No documentation found for Nested "PresentModeKHR" "PRESENT_MODE_IMMEDIATE_KHR"
pattern PRESENT_MODE_IMMEDIATE_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_IMMEDIATE_KHR = VK_PRESENT_MODE_IMMEDIATE_KHR


-- No documentation found for Nested "PresentModeKHR" "PRESENT_MODE_MAILBOX_KHR"
pattern PRESENT_MODE_MAILBOX_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_MAILBOX_KHR = VK_PRESENT_MODE_MAILBOX_KHR


-- No documentation found for Nested "PresentModeKHR" "PRESENT_MODE_FIFO_KHR"
pattern PRESENT_MODE_FIFO_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_FIFO_KHR = VK_PRESENT_MODE_FIFO_KHR


-- No documentation found for Nested "PresentModeKHR" "PRESENT_MODE_FIFO_RELAXED_KHR"
pattern PRESENT_MODE_FIFO_RELAXED_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_FIFO_RELAXED_KHR = VK_PRESENT_MODE_FIFO_RELAXED_KHR


-- No documentation found for Nested "PresentModeKHR" "PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR"
pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR = VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR


-- No documentation found for Nested "PresentModeKHR" "PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR"
pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR :: (a ~ PresentModeKHR) => a
pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR


-- No documentation found for TopLevel "VkSurfaceCapabilitiesKHR"
data SurfaceCapabilitiesKHR = SurfaceCapabilitiesKHR
  { -- No documentation found for Nested "SurfaceCapabilitiesKHR" "minImageCount"
  minImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageCount"
  maxImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "currentExtent"
  currentExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "minImageExtent"
  minImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageExtent"
  maxImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageArrayLayers"
  maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedTransforms"
  supportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "currentTransform"
  currentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedCompositeAlpha"
  supportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedUsageFlags"
  supportedUsageFlags :: ImageUsageFlags
  }
  deriving (Show, Eq)

instance Zero SurfaceCapabilitiesKHR where
  zero = SurfaceCapabilitiesKHR zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero



-- No documentation found for TopLevel "VkSurfaceFormatKHR"
data SurfaceFormatKHR = SurfaceFormatKHR
  { -- No documentation found for Nested "SurfaceFormatKHR" "format"
  format :: Format
  , -- No documentation found for Nested "SurfaceFormatKHR" "colorSpace"
  colorSpace :: ColorSpaceKHR
  }
  deriving (Show, Eq)

instance Zero SurfaceFormatKHR where
  zero = SurfaceFormatKHR zero
                          zero


-- No documentation found for TopLevel "SurfaceKHR"
type SurfaceKHR = VkSurfaceKHR

-- No documentation found for TopLevel "SurfaceTransformFlagBitsKHR"
type SurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR


{-# complete SURFACE_TRANSFORM_IDENTITY_BIT_KHR, SURFACE_TRANSFORM_ROTATE_90_BIT_KHR, SURFACE_TRANSFORM_ROTATE_180_BIT_KHR, SURFACE_TRANSFORM_ROTATE_270_BIT_KHR, SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR, SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR, SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR, SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR, SURFACE_TRANSFORM_INHERIT_BIT_KHR :: SurfaceTransformFlagBitsKHR #-}


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
pattern SURFACE_TRANSFORM_IDENTITY_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_IDENTITY_BIT_KHR = VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
pattern SURFACE_TRANSFORM_ROTATE_90_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
pattern SURFACE_TRANSFORM_ROTATE_180_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
pattern SURFACE_TRANSFORM_ROTATE_270_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR


-- No documentation found for Nested "SurfaceTransformFlagBitsKHR" "SURFACE_TRANSFORM_INHERIT_BIT_KHR"
pattern SURFACE_TRANSFORM_INHERIT_BIT_KHR :: (a ~ SurfaceTransformFlagBitsKHR) => a
pattern SURFACE_TRANSFORM_INHERIT_BIT_KHR = VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR

-- No documentation found for TopLevel "SurfaceTransformFlagsKHR"
type SurfaceTransformFlagsKHR = SurfaceTransformFlagBitsKHR


-- No documentation found for TopLevel "vkDestroySurfaceKHR"
destroySurfaceKHR :: Instance ->  SurfaceKHR ->  Maybe AllocationCallbacks ->  IO ()
destroySurfaceKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
getPhysicalDeviceSurfaceCapabilitiesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (SurfaceCapabilitiesKHR)
getPhysicalDeviceSurfaceCapabilitiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceFormatsKHR"
getNumPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfaceFormatsKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceFormatsKHR"
getPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector SurfaceFormatKHR)
getPhysicalDeviceSurfaceFormatsKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceSurfaceFormatsKHR'.
getAllPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector SurfaceFormatKHR)
getAllPhysicalDeviceSurfaceFormatsKHR physicalDevice' surface' =
  snd <$> getNumPhysicalDeviceSurfaceFormatsKHR physicalDevice' surface'
    >>= \num -> snd <$> getPhysicalDeviceSurfaceFormatsKHR physicalDevice' surface' num

#endif


-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfacePresentModesKHR"
getNumPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfacePresentModesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfacePresentModesKHR"
getPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector PresentModeKHR)
getPhysicalDeviceSurfacePresentModesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceSurfacePresentModesKHR'.
getAllPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector PresentModeKHR)
getAllPhysicalDeviceSurfacePresentModesKHR physicalDevice' surface' =
  snd <$> getNumPhysicalDeviceSurfacePresentModesKHR physicalDevice' surface'
    >>= \num -> snd <$> getPhysicalDeviceSurfacePresentModesKHR physicalDevice' surface' num



-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceSupportKHR"
getPhysicalDeviceSurfaceSupportKHR :: PhysicalDevice ->  Word32 ->  SurfaceKHR ->  IO (Bool)
getPhysicalDeviceSurfaceSupportKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_SURFACE_EXTENSION_NAME"
pattern KHR_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SURFACE_EXTENSION_NAME = VK_KHR_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SURFACE_SPEC_VERSION"
pattern KHR_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_SURFACE_SPEC_VERSION = VK_KHR_SURFACE_SPEC_VERSION
