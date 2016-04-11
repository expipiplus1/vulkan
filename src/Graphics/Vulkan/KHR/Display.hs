{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Display where

import Graphics.Vulkan.Device( PhysicalDevice(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.Surface( VkSurfaceTransformFlagBitsKHR(..)
                                  , VkSurfaceTransformFlagsKHR(..)
                                  , SurfaceKHR(..)
                                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.DeviceInitialization( Instance(..)
                                           )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkBool32(..)
                           , VkExtent2D(..)
                           , VkFlags(..)
                           , VkOffset2D(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CFloat
                      , CFloat(..)
                      , CChar
                      , CSize(..)
                      )


data VkDisplaySurfaceCreateInfoKHR =
  VkDisplaySurfaceCreateInfoKHR{ sType :: VkStructureType 
                               , pNext :: Ptr Void 
                               , flags :: VkDisplaySurfaceCreateFlagsKHR 
                               , displayMode :: DisplayModeKHR 
                               , planeIndex :: Word32 
                               , planeStackIndex :: Word32 
                               , transform :: VkSurfaceTransformFlagBitsKHR 
                               , globalAlpha :: CFloat 
                               , alphaMode :: VkDisplayPlaneAlphaFlagBitsKHR 
                               , imageExtent :: VkExtent2D 
                               }
  deriving (Eq)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (displayMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (planeIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (planeStackIndex (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (transform (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (globalAlpha (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 48) (alphaMode (poked :: VkDisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (imageExtent (poked :: VkDisplaySurfaceCreateInfoKHR))



data VkDisplayPlaneCapabilitiesKHR =
  VkDisplayPlaneCapabilitiesKHR{ supportedAlpha :: VkDisplayPlaneAlphaFlagsKHR 
                               , minSrcPosition :: VkOffset2D 
                               , maxSrcPosition :: VkOffset2D 
                               , minSrcExtent :: VkExtent2D 
                               , maxSrcExtent :: VkExtent2D 
                               , minDstPosition :: VkOffset2D 
                               , maxDstPosition :: VkOffset2D 
                               , minDstExtent :: VkExtent2D 
                               , maxDstExtent :: VkExtent2D 
                               }
  deriving (Eq)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (supportedAlpha (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (minSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 12) (maxSrcPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 20) (minSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 28) (maxSrcExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (minDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (maxDstPosition (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 52) (minDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 60) (maxDstExtent (poked :: VkDisplayPlaneCapabilitiesKHR))


-- ** vkGetDisplayModePropertiesKHR
foreign import ccall "vkGetDisplayModePropertiesKHR" vkGetDisplayModePropertiesKHR ::
  PhysicalDevice ->
  DisplayKHR ->
    Ptr Word32 -> Ptr VkDisplayModePropertiesKHR -> IO VkResult


data VkDisplayPropertiesKHR =
  VkDisplayPropertiesKHR{ display :: DisplayKHR 
                        , displayName :: Ptr CChar 
                        , physicalDimensions :: VkExtent2D 
                        , physicalResolution :: VkExtent2D 
                        , supportedTransforms :: VkSurfaceTransformFlagsKHR 
                        , planeReorderPossible :: VkBool32 
                        , persistentContent :: VkBool32 
                        }
  deriving (Eq)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (display (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (displayName (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (physicalDimensions (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 24) (physicalResolution (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 32) (supportedTransforms (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 36) (planeReorderPossible (poked :: VkDisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 40) (persistentContent (poked :: VkDisplayPropertiesKHR))


-- ** vkGetDisplayPlaneSupportedDisplaysKHR
foreign import ccall "vkGetDisplayPlaneSupportedDisplaysKHR" vkGetDisplayPlaneSupportedDisplaysKHR ::
  PhysicalDevice ->
  Word32 -> Ptr Word32 -> Ptr DisplayKHR -> IO VkResult

-- ** vkCreateDisplayModeKHR
foreign import ccall "vkCreateDisplayModeKHR" vkCreateDisplayModeKHR ::
  PhysicalDevice ->
  DisplayKHR ->
    Ptr VkDisplayModeCreateInfoKHR ->
      Ptr VkAllocationCallbacks -> Ptr DisplayModeKHR -> IO VkResult


data VkDisplayPlanePropertiesKHR =
  VkDisplayPlanePropertiesKHR{ currentDisplay :: DisplayKHR 
                             , currentStackIndex :: Word32 
                             }
  deriving (Eq)

instance Storable VkDisplayPlanePropertiesKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkDisplayPlanePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (currentDisplay (poked :: VkDisplayPlanePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (currentStackIndex (poked :: VkDisplayPlanePropertiesKHR))


-- ** vkGetDisplayPlaneCapabilitiesKHR
foreign import ccall "vkGetDisplayPlaneCapabilitiesKHR" vkGetDisplayPlaneCapabilitiesKHR ::
  PhysicalDevice ->
  DisplayModeKHR ->
    Word32 -> Ptr VkDisplayPlaneCapabilitiesKHR -> IO VkResult


data VkDisplayModePropertiesKHR =
  VkDisplayModePropertiesKHR{ displayMode :: DisplayModeKHR 
                            , parameters :: VkDisplayModeParametersKHR 
                            }
  deriving (Eq)

instance Storable VkDisplayModePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDisplayModePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (displayMode (poked :: VkDisplayModePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (parameters (poked :: VkDisplayModePropertiesKHR))


-- ** VkDisplayPlaneAlphaFlagsKHR

newtype VkDisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkDisplayPlaneAlphaFlagBitsKHR
type VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagBitsKHR

instance Show VkDisplayPlaneAlphaFlagBitsKHR where
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
  
  showsPrec p (VkDisplayPlaneAlphaFlagBitsKHR x) = showParen (p >= 11) (showString "VkDisplayPlaneAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkDisplayPlaneAlphaFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayPlaneAlphaFlagBitsKHR")
                        v <- step readPrec
                        pure (VkDisplayPlaneAlphaFlagBitsKHR v)
                        )
                    )


pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x1

pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x2

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x4

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VkDisplayPlaneAlphaFlagBitsKHR 0x8


-- ** VkDisplayModeCreateFlagsKHR
-- | Opaque flag
newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
  deriving (Eq, Storable)


data VkDisplayModeCreateInfoKHR =
  VkDisplayModeCreateInfoKHR{ sType :: VkStructureType 
                            , pNext :: Ptr Void 
                            , flags :: VkDisplayModeCreateFlagsKHR 
                            , parameters :: VkDisplayModeParametersKHR 
                            }
  deriving (Eq)

instance Storable VkDisplayModeCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDisplayModeCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkDisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 20) (parameters (poked :: VkDisplayModeCreateInfoKHR))


-- ** vkGetPhysicalDeviceDisplayPlanePropertiesKHR
foreign import ccall "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" vkGetPhysicalDeviceDisplayPlanePropertiesKHR ::
  PhysicalDevice ->
  Ptr Word32 -> Ptr VkDisplayPlanePropertiesKHR -> IO VkResult

newtype DisplayModeKHR = DisplayModeKHR Word64
  deriving (Eq, Storable)


data VkDisplayModeParametersKHR =
  VkDisplayModeParametersKHR{ visibleRegion :: VkExtent2D 
                            , refreshRate :: Word32 
                            }
  deriving (Eq)

instance Storable VkDisplayModeParametersKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDisplayModeParametersKHR <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (visibleRegion (poked :: VkDisplayModeParametersKHR))
                *> poke (ptr `plusPtr` 8) (refreshRate (poked :: VkDisplayModeParametersKHR))


-- ** VkDisplaySurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

newtype DisplayKHR = DisplayKHR Word64
  deriving (Eq, Storable)

-- ** vkGetPhysicalDeviceDisplayPropertiesKHR
foreign import ccall "vkGetPhysicalDeviceDisplayPropertiesKHR" vkGetPhysicalDeviceDisplayPropertiesKHR ::
  PhysicalDevice ->
  Ptr Word32 -> Ptr VkDisplayPropertiesKHR -> IO VkResult

-- ** vkCreateDisplayPlaneSurfaceKHR
foreign import ccall "vkCreateDisplayPlaneSurfaceKHR" vkCreateDisplayPlaneSurfaceKHR ::
  Instance ->
  Ptr VkDisplaySurfaceCreateInfoKHR ->
    Ptr VkAllocationCallbacks -> Ptr SurfaceKHR -> IO VkResult

