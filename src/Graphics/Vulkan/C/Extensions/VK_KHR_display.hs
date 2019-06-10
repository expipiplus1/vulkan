{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  , VkDisplayModeCreateFlagsKHR(..)
  , VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModeKHR
  , VkDisplayModeParametersKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneAlphaFlagBitsKHR(..)
  , pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR
  , pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR
  , VkDisplayPlaneAlphaFlagsKHR
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateFlagsKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , FN_vkCreateDisplayModeKHR
  , PFN_vkCreateDisplayModeKHR
  , vkCreateDisplayModeKHR
  , FN_vkCreateDisplayPlaneSurfaceKHR
  , PFN_vkCreateDisplayPlaneSurfaceKHR
  , vkCreateDisplayPlaneSurfaceKHR
  , FN_vkGetDisplayModePropertiesKHR
  , PFN_vkGetDisplayModePropertiesKHR
  , vkGetDisplayModePropertiesKHR
  , FN_vkGetDisplayPlaneCapabilitiesKHR
  , PFN_vkGetDisplayPlaneCapabilitiesKHR
  , vkGetDisplayPlaneCapabilitiesKHR
  , FN_vkGetDisplayPlaneSupportedDisplaysKHR
  , PFN_vkGetDisplayPlaneSupportedDisplaysKHR
  , vkGetDisplayPlaneSupportedDisplaysKHR
  , FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , FN_vkGetPhysicalDeviceDisplayPropertiesKHR
  , PFN_vkGetPhysicalDeviceDisplayPropertiesKHR
  , vkGetPhysicalDeviceDisplayPropertiesKHR
  , pattern VK_KHR_DISPLAY_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DISPLAY_KHR
  , pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
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
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkInstance
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkExtent2D(..)
  , VkOffset2D(..)
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceTransformFlagBitsKHR(..)
  , VkSurfaceKHR
  , VkSurfaceTransformFlagsKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkDisplayKHR_T
-- No documentation found for TopLevel "VkDisplayKHR"
type VkDisplayKHR = Ptr VkDisplayKHR_T

-- ** VkDisplayModeCreateFlagsKHR

-- No documentation found for TopLevel "VkDisplayModeCreateFlagsKHR"
newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDisplayModeCreateFlagsKHR where
  
  showsPrec p (VkDisplayModeCreateFlagsKHR x) = showParen (p >= 11) (showString "VkDisplayModeCreateFlagsKHR " . showsPrec 11 x)

instance Read VkDisplayModeCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayModeCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkDisplayModeCreateFlagsKHR v)
                        )
                    )



-- No documentation found for TopLevel "VkDisplayModeCreateInfoKHR"
data VkDisplayModeCreateInfoKHR = VkDisplayModeCreateInfoKHR
  { -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "flags"
  vkFlags :: VkDisplayModeCreateFlagsKHR
  , -- No documentation found for Nested "VkDisplayModeCreateInfoKHR" "parameters"
  vkParameters :: VkDisplayModeParametersKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayModeCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayModeCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 20) (vkParameters (poked :: VkDisplayModeCreateInfoKHR))

instance Zero VkDisplayModeCreateInfoKHR where
  zero = VkDisplayModeCreateInfoKHR VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
                                    zero
                                    zero
                                    zero

-- | Dummy data to tag the 'Ptr' with
data VkDisplayModeKHR_T
-- No documentation found for TopLevel "VkDisplayModeKHR"
type VkDisplayModeKHR = Ptr VkDisplayModeKHR_T

-- No documentation found for TopLevel "VkDisplayModeParametersKHR"
data VkDisplayModeParametersKHR = VkDisplayModeParametersKHR
  { -- No documentation found for Nested "VkDisplayModeParametersKHR" "visibleRegion"
  vkVisibleRegion :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayModeParametersKHR" "refreshRate"
  vkRefreshRate :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDisplayModeParametersKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDisplayModeParametersKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVisibleRegion (poked :: VkDisplayModeParametersKHR))
                *> poke (ptr `plusPtr` 8) (vkRefreshRate (poked :: VkDisplayModeParametersKHR))

instance Zero VkDisplayModeParametersKHR where
  zero = VkDisplayModeParametersKHR zero
                                    zero

-- No documentation found for TopLevel "VkDisplayModePropertiesKHR"
data VkDisplayModePropertiesKHR = VkDisplayModePropertiesKHR
  { -- No documentation found for Nested "VkDisplayModePropertiesKHR" "displayMode"
  vkDisplayMode :: VkDisplayModeKHR
  , -- No documentation found for Nested "VkDisplayModePropertiesKHR" "parameters"
  vkParameters :: VkDisplayModeParametersKHR
  }
  deriving (Eq, Show)

instance Storable VkDisplayModePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayModePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDisplayMode (poked :: VkDisplayModePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkParameters (poked :: VkDisplayModePropertiesKHR))

instance Zero VkDisplayModePropertiesKHR where
  zero = VkDisplayModePropertiesKHR zero
                                    zero

-- ** VkDisplayPlaneAlphaFlagBitsKHR

-- No documentation found for TopLevel "VkDisplayPlaneAlphaFlagBitsKHR"
newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDisplayPlaneAlphaFlagBitsKHR where
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
  showsPrec p (VkDisplayPlaneAlphaFlagBitsKHR x) = showParen (p >= 11) (showString "VkDisplayPlaneAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkDisplayPlaneAlphaFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR",                  pure VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR",                  pure VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR",               pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayPlaneAlphaFlagBitsKHR")
                        v <- step readPrec
                        pure (VkDisplayPlaneAlphaFlagBitsKHR v)
                        )
                    )

-- No documentation found for Nested "VkDisplayPlaneAlphaFlagBitsKHR" "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR :: VkDisplayPlaneAlphaFlagBitsKHR
pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkDisplayPlaneAlphaFlagBitsKHR" "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR :: VkDisplayPlaneAlphaFlagBitsKHR
pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkDisplayPlaneAlphaFlagBitsKHR" "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR :: VkDisplayPlaneAlphaFlagBitsKHR
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x00000004

-- No documentation found for Nested "VkDisplayPlaneAlphaFlagBitsKHR" "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR :: VkDisplayPlaneAlphaFlagBitsKHR
pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x00000008

-- No documentation found for TopLevel "VkDisplayPlaneAlphaFlagsKHR"
type VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagBitsKHR

-- No documentation found for TopLevel "VkDisplayPlaneCapabilitiesKHR"
data VkDisplayPlaneCapabilitiesKHR = VkDisplayPlaneCapabilitiesKHR
  { -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "supportedAlpha"
  vkSupportedAlpha :: VkDisplayPlaneAlphaFlagsKHR
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minSrcPosition"
  vkMinSrcPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxSrcPosition"
  vkMaxSrcPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minSrcExtent"
  vkMinSrcExtent :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxSrcExtent"
  vkMaxSrcExtent :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minDstPosition"
  vkMinDstPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxDstPosition"
  vkMaxDstPosition :: VkOffset2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "minDstExtent"
  vkMinDstExtent :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPlaneCapabilitiesKHR" "maxDstExtent"
  vkMaxDstExtent :: VkExtent2D
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlaneCapabilitiesKHR where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkDisplayPlaneCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 4)
                                           <*> peek (ptr `plusPtr` 12)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 28)
                                           <*> peek (ptr `plusPtr` 36)
                                           <*> peek (ptr `plusPtr` 44)
                                           <*> peek (ptr `plusPtr` 52)
                                           <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSupportedAlpha (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (vkMinSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 12) (vkMaxSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 20) (vkMinSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 28) (vkMaxSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (vkMinDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (vkMaxDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 52) (vkMinDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 60) (vkMaxDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))

instance Zero VkDisplayPlaneCapabilitiesKHR where
  zero = VkDisplayPlaneCapabilitiesKHR zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero

-- No documentation found for TopLevel "VkDisplayPlanePropertiesKHR"
data VkDisplayPlanePropertiesKHR = VkDisplayPlanePropertiesKHR
  { -- No documentation found for Nested "VkDisplayPlanePropertiesKHR" "currentDisplay"
  vkCurrentDisplay :: VkDisplayKHR
  , -- No documentation found for Nested "VkDisplayPlanePropertiesKHR" "currentStackIndex"
  vkCurrentStackIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPlanePropertiesKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkDisplayPlanePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkCurrentDisplay (poked :: VkDisplayPlanePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkCurrentStackIndex (poked :: VkDisplayPlanePropertiesKHR))

instance Zero VkDisplayPlanePropertiesKHR where
  zero = VkDisplayPlanePropertiesKHR zero
                                     zero

-- No documentation found for TopLevel "VkDisplayPropertiesKHR"
data VkDisplayPropertiesKHR = VkDisplayPropertiesKHR
  { -- No documentation found for Nested "VkDisplayPropertiesKHR" "display"
  vkDisplay :: VkDisplayKHR
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "displayName"
  vkDisplayName :: Ptr CChar
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "physicalDimensions"
  vkPhysicalDimensions :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "physicalResolution"
  vkPhysicalResolution :: VkExtent2D
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "supportedTransforms"
  vkSupportedTransforms :: VkSurfaceTransformFlagsKHR
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "planeReorderPossible"
  vkPlaneReorderPossible :: VkBool32
  , -- No documentation found for Nested "VkDisplayPropertiesKHR" "persistentContent"
  vkPersistentContent :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPropertiesKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkDisplayPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 36)
                                    <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDisplay (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkDisplayName (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDimensions (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkPhysicalResolution (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 32) (vkSupportedTransforms (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 36) (vkPlaneReorderPossible (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 40) (vkPersistentContent (poked :: VkDisplayPropertiesKHR))

instance Zero VkDisplayPropertiesKHR where
  zero = VkDisplayPropertiesKHR zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero

-- ** VkDisplaySurfaceCreateFlagsKHR

-- No documentation found for TopLevel "VkDisplaySurfaceCreateFlagsKHR"
newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDisplaySurfaceCreateFlagsKHR where
  
  showsPrec p (VkDisplaySurfaceCreateFlagsKHR x) = showParen (p >= 11) (showString "VkDisplaySurfaceCreateFlagsKHR " . showsPrec 11 x)

instance Read VkDisplaySurfaceCreateFlagsKHR where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplaySurfaceCreateFlagsKHR")
                        v <- step readPrec
                        pure (VkDisplaySurfaceCreateFlagsKHR v)
                        )
                    )



-- No documentation found for TopLevel "VkDisplaySurfaceCreateInfoKHR"
data VkDisplaySurfaceCreateInfoKHR = VkDisplaySurfaceCreateInfoKHR
  { -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "flags"
  vkFlags :: VkDisplaySurfaceCreateFlagsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "displayMode"
  vkDisplayMode :: VkDisplayModeKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "planeIndex"
  vkPlaneIndex :: Word32
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "planeStackIndex"
  vkPlaneStackIndex :: Word32
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "transform"
  vkTransform :: VkSurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "globalAlpha"
  vkGlobalAlpha :: CFloat
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "alphaMode"
  vkAlphaMode :: VkDisplayPlaneAlphaFlagBitsKHR
  , -- No documentation found for Nested "VkDisplaySurfaceCreateInfoKHR" "imageExtent"
  vkImageExtent :: VkExtent2D
  }
  deriving (Eq, Show)

instance Storable VkDisplaySurfaceCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDisplaySurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
                                           <*> peek (ptr `plusPtr` 36)
                                           <*> peek (ptr `plusPtr` 40)
                                           <*> peek (ptr `plusPtr` 44)
                                           <*> peek (ptr `plusPtr` 48)
                                           <*> peek (ptr `plusPtr` 52)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (vkDisplayMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkPlaneIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (vkPlaneStackIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (vkTransform (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (vkGlobalAlpha (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkAlphaMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (vkImageExtent (poked :: VkDisplaySurfaceCreateInfoKHR))

instance Zero VkDisplaySurfaceCreateInfoKHR where
  zero = VkDisplaySurfaceCreateInfoKHR VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero

-- No documentation found for TopLevel "vkCreateDisplayModeKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDisplayModeKHR" vkCreateDisplayModeKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult
#else
vkCreateDisplayModeKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult
vkCreateDisplayModeKHR deviceCmds = mkVkCreateDisplayModeKHR (pVkCreateDisplayModeKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDisplayModeKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult)
#endif

type FN_vkCreateDisplayModeKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult
type PFN_vkCreateDisplayModeKHR = FunPtr FN_vkCreateDisplayModeKHR

-- No documentation found for TopLevel "vkCreateDisplayPlaneSurfaceKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDisplayPlaneSurfaceKHR" vkCreateDisplayPlaneSurfaceKHR :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
#else
vkCreateDisplayPlaneSurfaceKHR :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
vkCreateDisplayPlaneSurfaceKHR deviceCmds = mkVkCreateDisplayPlaneSurfaceKHR (pVkCreateDisplayPlaneSurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDisplayPlaneSurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif

type FN_vkCreateDisplayPlaneSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateDisplayPlaneSurfaceKHR = FunPtr FN_vkCreateDisplayPlaneSurfaceKHR

-- No documentation found for TopLevel "vkGetDisplayModePropertiesKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayModePropertiesKHR" vkGetDisplayModePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult
#else
vkGetDisplayModePropertiesKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult
vkGetDisplayModePropertiesKHR deviceCmds = mkVkGetDisplayModePropertiesKHR (pVkGetDisplayModePropertiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModePropertiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult)
#endif

type FN_vkGetDisplayModePropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult
type PFN_vkGetDisplayModePropertiesKHR = FunPtr FN_vkGetDisplayModePropertiesKHR

-- No documentation found for TopLevel "vkGetDisplayPlaneCapabilitiesKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayPlaneCapabilitiesKHR" vkGetDisplayPlaneCapabilitiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult
#else
vkGetDisplayPlaneCapabilitiesKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult
vkGetDisplayPlaneCapabilitiesKHR deviceCmds = mkVkGetDisplayPlaneCapabilitiesKHR (pVkGetDisplayPlaneCapabilitiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilitiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult)
#endif

type FN_vkGetDisplayPlaneCapabilitiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult
type PFN_vkGetDisplayPlaneCapabilitiesKHR = FunPtr FN_vkGetDisplayPlaneCapabilitiesKHR

-- No documentation found for TopLevel "vkGetDisplayPlaneSupportedDisplaysKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDisplayPlaneSupportedDisplaysKHR" vkGetDisplayPlaneSupportedDisplaysKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult
#else
vkGetDisplayPlaneSupportedDisplaysKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult
vkGetDisplayPlaneSupportedDisplaysKHR deviceCmds = mkVkGetDisplayPlaneSupportedDisplaysKHR (pVkGetDisplayPlaneSupportedDisplaysKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneSupportedDisplaysKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult)
#endif

type FN_vkGetDisplayPlaneSupportedDisplaysKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult
type PFN_vkGetDisplayPlaneSupportedDisplaysKHR = FunPtr FN_vkGetDisplayPlaneSupportedDisplaysKHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" vkGetPhysicalDeviceDisplayPlanePropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult
#else
vkGetPhysicalDeviceDisplayPlanePropertiesKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult
vkGetPhysicalDeviceDisplayPlanePropertiesKHR deviceCmds = mkVkGetPhysicalDeviceDisplayPlanePropertiesKHR (pVkGetPhysicalDeviceDisplayPlanePropertiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlanePropertiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR = FunPtr FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR

-- No documentation found for TopLevel "vkGetPhysicalDeviceDisplayPropertiesKHR"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceDisplayPropertiesKHR" vkGetPhysicalDeviceDisplayPropertiesKHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult
#else
vkGetPhysicalDeviceDisplayPropertiesKHR :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult
vkGetPhysicalDeviceDisplayPropertiesKHR deviceCmds = mkVkGetPhysicalDeviceDisplayPropertiesKHR (pVkGetPhysicalDeviceDisplayPropertiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPropertiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceDisplayPropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPropertiesKHR = FunPtr FN_vkGetPhysicalDeviceDisplayPropertiesKHR

-- No documentation found for TopLevel "VK_KHR_DISPLAY_EXTENSION_NAME"
pattern VK_KHR_DISPLAY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_DISPLAY_EXTENSION_NAME = "VK_KHR_display"

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SPEC_VERSION"
pattern VK_KHR_DISPLAY_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DISPLAY_SPEC_VERSION = 21

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_KHR"
pattern VK_OBJECT_TYPE_DISPLAY_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_DISPLAY_KHR = VkObjectType 1000002000

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DISPLAY_MODE_KHR"
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR :: VkObjectType
pattern VK_OBJECT_TYPE_DISPLAY_MODE_KHR = VkObjectType 1000002001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR = VkStructureType 1000002000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR = VkStructureType 1000002001
