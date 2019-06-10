{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  , pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
  , VkCompositeAlphaFlagBitsKHR(..)
  , pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR
  , pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR
  , VkCompositeAlphaFlagsKHR
  , VkPresentModeKHR(..)
  , pattern VK_PRESENT_MODE_IMMEDIATE_KHR
  , pattern VK_PRESENT_MODE_MAILBOX_KHR
  , pattern VK_PRESENT_MODE_FIFO_KHR
  , pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  , VkSurfaceKHR
  , VkSurfaceTransformFlagBitsKHR(..)
  , pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR
  , pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR
  , VkSurfaceTransformFlagsKHR
  , FN_vkDestroySurfaceKHR
  , PFN_vkDestroySurfaceKHR
  , vkDestroySurfaceKHR
  , FN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , FN_vkGetPhysicalDeviceSurfaceFormatsKHR
  , PFN_vkGetPhysicalDeviceSurfaceFormatsKHR
  , vkGetPhysicalDeviceSurfaceFormatsKHR
  , FN_vkGetPhysicalDeviceSurfacePresentModesKHR
  , PFN_vkGetPhysicalDeviceSurfacePresentModesKHR
  , vkGetPhysicalDeviceSurfacePresentModesKHR
  , FN_vkGetPhysicalDeviceSurfaceSupportKHR
  , PFN_vkGetPhysicalDeviceSurfaceSupportKHR
  , vkGetPhysicalDeviceSurfaceSupportKHR
  , pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern VK_ERROR_SURFACE_LOST_KHR
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_SURFACE_KHR
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
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
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkImageUsageFlags
  , VkInstance
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkColorSpaceKHR

-- No documentation found for TopLevel "VkColorSpaceKHR"
newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkColorSpaceKHR where
  showsPrec _ VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = showString "VK_COLOR_SPACE_SRGB_NONLINEAR_KHR"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkColorSpaceKHR 1000104001) = showString "VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104002) = showString "VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104003) = showString "VK_COLOR_SPACE_DCI_P3_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104004) = showString "VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104005) = showString "VK_COLOR_SPACE_BT709_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104006) = showString "VK_COLOR_SPACE_BT709_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104007) = showString "VK_COLOR_SPACE_BT2020_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104008) = showString "VK_COLOR_SPACE_HDR10_ST2084_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104009) = showString "VK_COLOR_SPACE_DOLBYVISION_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104010) = showString "VK_COLOR_SPACE_HDR10_HLG_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104011) = showString "VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104012) = showString "VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104013) = showString "VK_COLOR_SPACE_PASS_THROUGH_EXT"
  showsPrec _ (VkColorSpaceKHR 1000104014) = showString "VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT"
  showsPrec _ (VkColorSpaceKHR 1000213000) = showString "VK_COLOR_SPACE_DISPLAY_NATIVE_AMD"
  showsPrec p (VkColorSpaceKHR x) = showParen (p >= 11) (showString "VkColorSpaceKHR " . showsPrec 11 x)

instance Read VkColorSpaceKHR where
  readPrec = parens ( choose [ ("VK_COLOR_SPACE_SRGB_NONLINEAR_KHR", pure VK_COLOR_SPACE_SRGB_NONLINEAR_KHR)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT",    pure (VkColorSpaceKHR 1000104001))
                             , ("VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT",    pure (VkColorSpaceKHR 1000104002))
                             , ("VK_COLOR_SPACE_DCI_P3_LINEAR_EXT",           pure (VkColorSpaceKHR 1000104003))
                             , ("VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT",        pure (VkColorSpaceKHR 1000104004))
                             , ("VK_COLOR_SPACE_BT709_LINEAR_EXT",            pure (VkColorSpaceKHR 1000104005))
                             , ("VK_COLOR_SPACE_BT709_NONLINEAR_EXT",         pure (VkColorSpaceKHR 1000104006))
                             , ("VK_COLOR_SPACE_BT2020_LINEAR_EXT",           pure (VkColorSpaceKHR 1000104007))
                             , ("VK_COLOR_SPACE_HDR10_ST2084_EXT",            pure (VkColorSpaceKHR 1000104008))
                             , ("VK_COLOR_SPACE_DOLBYVISION_EXT",             pure (VkColorSpaceKHR 1000104009))
                             , ("VK_COLOR_SPACE_HDR10_HLG_EXT",               pure (VkColorSpaceKHR 1000104010))
                             , ("VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT",         pure (VkColorSpaceKHR 1000104011))
                             , ("VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT",      pure (VkColorSpaceKHR 1000104012))
                             , ("VK_COLOR_SPACE_PASS_THROUGH_EXT",            pure (VkColorSpaceKHR 1000104013))
                             , ("VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT", pure (VkColorSpaceKHR 1000104014))
                             , ("VK_COLOR_SPACE_DISPLAY_NATIVE_AMD",          pure (VkColorSpaceKHR 1000213000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkColorSpaceKHR")
                        v <- step readPrec
                        pure (VkColorSpaceKHR v)
                        )
                    )

-- No documentation found for Nested "VkColorSpaceKHR" "VK_COLOR_SPACE_SRGB_NONLINEAR_KHR"
pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0

-- ** VkCompositeAlphaFlagBitsKHR

-- No documentation found for TopLevel "VkCompositeAlphaFlagBitsKHR"
newtype VkCompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkCompositeAlphaFlagBitsKHR where
  showsPrec _ VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = showString "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = showString "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
  showsPrec p (VkCompositeAlphaFlagBitsKHR x) = showParen (p >= 11) (showString "VkCompositeAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkCompositeAlphaFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR",          pure VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR",  pure VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR", pure VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR",         pure VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCompositeAlphaFlagBitsKHR")
                        v <- step readPrec
                        pure (VkCompositeAlphaFlagBitsKHR v)
                        )
                    )

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR :: VkCompositeAlphaFlagBitsKHR
pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR :: VkCompositeAlphaFlagBitsKHR
pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR :: VkCompositeAlphaFlagBitsKHR
pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x00000004

-- No documentation found for Nested "VkCompositeAlphaFlagBitsKHR" "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR :: VkCompositeAlphaFlagBitsKHR
pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x00000008

-- No documentation found for TopLevel "VkCompositeAlphaFlagsKHR"
type VkCompositeAlphaFlagsKHR = VkCompositeAlphaFlagBitsKHR

-- ** VkPresentModeKHR

-- No documentation found for TopLevel "VkPresentModeKHR"
newtype VkPresentModeKHR = VkPresentModeKHR Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkPresentModeKHR where
  showsPrec _ VK_PRESENT_MODE_IMMEDIATE_KHR = showString "VK_PRESENT_MODE_IMMEDIATE_KHR"
  showsPrec _ VK_PRESENT_MODE_MAILBOX_KHR = showString "VK_PRESENT_MODE_MAILBOX_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_KHR = showString "VK_PRESENT_MODE_FIFO_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_RELAXED_KHR = showString "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkPresentModeKHR 1000111000) = showString "VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR"
  showsPrec _ (VkPresentModeKHR 1000111001) = showString "VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR"
  showsPrec p (VkPresentModeKHR x) = showParen (p >= 11) (showString "VkPresentModeKHR " . showsPrec 11 x)

instance Read VkPresentModeKHR where
  readPrec = parens ( choose [ ("VK_PRESENT_MODE_IMMEDIATE_KHR",    pure VK_PRESENT_MODE_IMMEDIATE_KHR)
                             , ("VK_PRESENT_MODE_MAILBOX_KHR",      pure VK_PRESENT_MODE_MAILBOX_KHR)
                             , ("VK_PRESENT_MODE_FIFO_KHR",         pure VK_PRESENT_MODE_FIFO_KHR)
                             , ("VK_PRESENT_MODE_FIFO_RELAXED_KHR", pure VK_PRESENT_MODE_FIFO_RELAXED_KHR)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR",     pure (VkPresentModeKHR 1000111000))
                             , ("VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR", pure (VkPresentModeKHR 1000111001))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPresentModeKHR")
                        v <- step readPrec
                        pure (VkPresentModeKHR v)
                        )
                    )

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_IMMEDIATE_KHR"
pattern VK_PRESENT_MODE_IMMEDIATE_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_MAILBOX_KHR"
pattern VK_PRESENT_MODE_MAILBOX_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_FIFO_KHR"
pattern VK_PRESENT_MODE_FIFO_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

-- No documentation found for Nested "VkPresentModeKHR" "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3

-- No documentation found for TopLevel "VkSurfaceCapabilitiesKHR"
data VkSurfaceCapabilitiesKHR = VkSurfaceCapabilitiesKHR
  { -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "minImageCount"
  vkMinImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "maxImageCount"
  vkMaxImageCount :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "currentExtent"
  vkCurrentExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "minImageExtent"
  vkMinImageExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "maxImageExtent"
  vkMaxImageExtent :: VkExtent2D
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "maxImageArrayLayers"
  vkMaxImageArrayLayers :: Word32
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "supportedTransforms"
  vkSupportedTransforms :: VkSurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "currentTransform"
  vkCurrentTransform :: VkSurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "supportedCompositeAlpha"
  vkSupportedCompositeAlpha :: VkCompositeAlphaFlagsKHR
  , -- No documentation found for Nested "VkSurfaceCapabilitiesKHR" "supportedUsageFlags"
  vkSupportedUsageFlags :: VkImageUsageFlags
  }
  deriving (Eq, Show)

instance Storable VkSurfaceCapabilitiesKHR where
  sizeOf ~_ = 52
  alignment ~_ = 4
  peek ptr = VkSurfaceCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMinImageCount (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (vkMaxImageCount (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCurrentExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkMinImageExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 24) (vkMaxImageExtent (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 32) (vkMaxImageArrayLayers (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (vkSupportedTransforms (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 40) (vkCurrentTransform (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (vkSupportedCompositeAlpha (poked :: VkSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 48) (vkSupportedUsageFlags (poked :: VkSurfaceCapabilitiesKHR))

instance Zero VkSurfaceCapabilitiesKHR where
  zero = VkSurfaceCapabilitiesKHR zero
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
data VkSurfaceFormatKHR = VkSurfaceFormatKHR
  { -- No documentation found for Nested "VkSurfaceFormatKHR" "format"
  vkFormat :: VkFormat
  , -- No documentation found for Nested "VkSurfaceFormatKHR" "colorSpace"
  vkColorSpace :: VkColorSpaceKHR
  }
  deriving (Eq, Show)

instance Storable VkSurfaceFormatKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkSurfaceFormatKHR <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFormat (poked :: VkSurfaceFormatKHR))
                *> poke (ptr `plusPtr` 4) (vkColorSpace (poked :: VkSurfaceFormatKHR))

instance Zero VkSurfaceFormatKHR where
  zero = VkSurfaceFormatKHR zero
                            zero

-- | Dummy data to tag the 'Ptr' with
data VkSurfaceKHR_T
-- No documentation found for TopLevel "VkSurfaceKHR"
type VkSurfaceKHR = Ptr VkSurfaceKHR_T

-- ** VkSurfaceTransformFlagBitsKHR

-- No documentation found for TopLevel "VkSurfaceTransformFlagBitsKHR"
newtype VkSurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkSurfaceTransformFlagBitsKHR where
  showsPrec _ VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = showString "VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = showString "VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR"
  showsPrec p (VkSurfaceTransformFlagBitsKHR x) = showParen (p >= 11) (showString "VkSurfaceTransformFlagBitsKHR " . showsPrec 11 x)

instance Read VkSurfaceTransformFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR",                     pure VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR",                    pure VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR",                   pure VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR",                   pure VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR",            pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR",  pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR",                      pure VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSurfaceTransformFlagBitsKHR")
                        v <- step readPrec
                        pure (VkSurfaceTransformFlagBitsKHR v)
                        )
                    )

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000004

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000008

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000010

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000020

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000040

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000080

-- No documentation found for Nested "VkSurfaceTransformFlagBitsKHR" "VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR"
pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR :: VkSurfaceTransformFlagBitsKHR
pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x00000100

-- No documentation found for TopLevel "VkSurfaceTransformFlagsKHR"
type VkSurfaceTransformFlagsKHR = VkSurfaceTransformFlagBitsKHR

-- No documentation found for TopLevel "vkDestroySurfaceKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroySurfaceKHR" vkDestroySurfaceKHR :: ("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroySurfaceKHR :: InstanceCmds -> ("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroySurfaceKHR deviceCmds = mkVkDestroySurfaceKHR (pVkDestroySurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroySurfaceKHR = ("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySurfaceKHR = FunPtr FN_vkDestroySurfaceKHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" vkGetPhysicalDeviceSurfaceCapabilitiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult
#else
vkGetPhysicalDeviceSurfaceCapabilitiesKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult
vkGetPhysicalDeviceSurfaceCapabilitiesKHR deviceCmds = mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR (pVkGetPhysicalDeviceSurfaceCapabilitiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR = FunPtr FN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceFormatsKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSurfaceFormatsKHR" vkGetPhysicalDeviceSurfaceFormatsKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult
#else
vkGetPhysicalDeviceSurfaceFormatsKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult
vkGetPhysicalDeviceSurfaceFormatsKHR deviceCmds = mkVkGetPhysicalDeviceSurfaceFormatsKHR (pVkGetPhysicalDeviceSurfaceFormatsKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceFormatsKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceSurfaceFormatsKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfaceFormatsKHR = FunPtr FN_vkGetPhysicalDeviceSurfaceFormatsKHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfacePresentModesKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSurfacePresentModesKHR" vkGetPhysicalDeviceSurfacePresentModesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
#else
vkGetPhysicalDeviceSurfacePresentModesKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
vkGetPhysicalDeviceSurfacePresentModesKHR deviceCmds = mkVkGetPhysicalDeviceSurfacePresentModesKHR (pVkGetPhysicalDeviceSurfacePresentModesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceSurfacePresentModesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfacePresentModesKHR = FunPtr FN_vkGetPhysicalDeviceSurfacePresentModesKHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceSupportKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceSurfaceSupportKHR" vkGetPhysicalDeviceSurfaceSupportKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult
#else
vkGetPhysicalDeviceSurfaceSupportKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult
vkGetPhysicalDeviceSurfaceSupportKHR deviceCmds = mkVkGetPhysicalDeviceSurfaceSupportKHR (pVkGetPhysicalDeviceSurfaceSupportKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceSupportKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceSurfaceSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfaceSupportKHR = FunPtr FN_vkGetPhysicalDeviceSurfaceSupportKHR

-- No documentation found for Nested "VkResult" "VK_ERROR_NATIVE_WINDOW_IN_USE_KHR"
pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR :: VkResult
pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR = VkResult (-1000000001)

-- No documentation found for Nested "VkResult" "VK_ERROR_SURFACE_LOST_KHR"
pattern VK_ERROR_SURFACE_LOST_KHR :: VkResult
pattern VK_ERROR_SURFACE_LOST_KHR = VkResult (-1000000000)

-- No documentation found for TopLevel "VK_KHR_SURFACE_EXTENSION_NAME"
pattern VK_KHR_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_SURFACE_EXTENSION_NAME = "VK_KHR_surface"

-- No documentation found for TopLevel "VK_KHR_SURFACE_SPEC_VERSION"
pattern VK_KHR_SURFACE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SURFACE_SPEC_VERSION = 25

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SURFACE_KHR"
pattern VK_OBJECT_TYPE_SURFACE_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_SURFACE_KHR = VkObjectType 1000000000
