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
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Graphics.Vulkan.KHR.Surface( SurfaceKHR(..)
                                  , VkSurfaceTransformFlagsKHR(..)
                                  )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
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
import Graphics.Vulkan.Core( VkStructureType(..)
                           , Offset2D(..)
                           , VkFlags(..)
                           , VkBool32(..)
                           , VkResult(..)
                           , Extent2D(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CChar(..)
                      )


data DisplaySurfaceCreateInfoKHR =
  DisplaySurfaceCreateInfoKHR{ sType :: VkStructureType 
                             , pNext :: Ptr Void 
                             , flags :: VkDisplaySurfaceCreateFlagsKHR 
                             , displayMode :: DisplayModeKHR 
                             , planeIndex :: Word32 
                             , planeStackIndex :: Word32 
                             , transform :: VkSurfaceTransformFlagsKHR 
                             , globalAlpha :: CFloat 
                             , alphaMode :: VkDisplayPlaneAlphaFlagsKHR 
                             , imageExtent :: Extent2D 
                             }
  deriving (Eq)

instance Storable DisplaySurfaceCreateInfoKHR where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = DisplaySurfaceCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
                                         <*> peek (ptr `plusPtr` 36)
                                         <*> peek (ptr `plusPtr` 40)
                                         <*> peek (ptr `plusPtr` 44)
                                         <*> peek (ptr `plusPtr` 48)
                                         <*> peek (ptr `plusPtr` 52)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 24) (displayMode (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 32) (planeIndex (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 36) (planeStackIndex (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 40) (transform (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 44) (globalAlpha (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 48) (alphaMode (poked :: DisplaySurfaceCreateInfoKHR))
                *> poke (ptr `plusPtr` 52) (imageExtent (poked :: DisplaySurfaceCreateInfoKHR))



data DisplayPlaneCapabilitiesKHR =
  DisplayPlaneCapabilitiesKHR{ supportedAlpha :: VkDisplayPlaneAlphaFlagsKHR 
                             , minSrcPosition :: Offset2D 
                             , maxSrcPosition :: Offset2D 
                             , minSrcExtent :: Extent2D 
                             , maxSrcExtent :: Extent2D 
                             , minDstPosition :: Offset2D 
                             , maxDstPosition :: Offset2D 
                             , minDstExtent :: Extent2D 
                             , maxDstExtent :: Extent2D 
                             }
  deriving (Eq)

instance Storable DisplayPlaneCapabilitiesKHR where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = DisplayPlaneCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 4)
                                         <*> peek (ptr `plusPtr` 12)
                                         <*> peek (ptr `plusPtr` 20)
                                         <*> peek (ptr `plusPtr` 28)
                                         <*> peek (ptr `plusPtr` 36)
                                         <*> peek (ptr `plusPtr` 44)
                                         <*> peek (ptr `plusPtr` 52)
                                         <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (supportedAlpha (poked :: DisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (minSrcPosition (poked :: DisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 12) (maxSrcPosition (poked :: DisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 20) (minSrcExtent (poked :: DisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 28) (maxSrcExtent (poked :: DisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (minDstPosition (poked :: DisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (maxDstPosition (poked :: DisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 52) (minDstExtent (poked :: DisplayPlaneCapabilitiesKHR))
                *> poke (ptr `plusPtr` 60) (maxDstExtent (poked :: DisplayPlaneCapabilitiesKHR))


-- ** vkGetDisplayModePropertiesKHR
foreign import ccall "vkGetDisplayModePropertiesKHR" vkGetDisplayModePropertiesKHR ::
  PhysicalDevice ->
  DisplayKHR ->
    Ptr Word32 -> Ptr DisplayModePropertiesKHR -> IO VkResult


data DisplayPropertiesKHR =
  DisplayPropertiesKHR{ display :: DisplayKHR 
                      , displayName :: Ptr CChar 
                      , physicalDimensions :: Extent2D 
                      , physicalResolution :: Extent2D 
                      , supportedTransforms :: VkSurfaceTransformFlagsKHR 
                      , planeReorderPossible :: VkBool32 
                      , persistentContent :: VkBool32 
                      }
  deriving (Eq)

instance Storable DisplayPropertiesKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = DisplayPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (display (poked :: DisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (displayName (poked :: DisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (physicalDimensions (poked :: DisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 24) (physicalResolution (poked :: DisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 32) (supportedTransforms (poked :: DisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 36) (planeReorderPossible (poked :: DisplayPropertiesKHR))
                *> poke (ptr `plusPtr` 40) (persistentContent (poked :: DisplayPropertiesKHR))


-- ** vkGetDisplayPlaneSupportedDisplaysKHR
foreign import ccall "vkGetDisplayPlaneSupportedDisplaysKHR" vkGetDisplayPlaneSupportedDisplaysKHR ::
  PhysicalDevice ->
  Word32 -> Ptr Word32 -> Ptr DisplayKHR -> IO VkResult

-- ** vkCreateDisplayModeKHR
foreign import ccall "vkCreateDisplayModeKHR" vkCreateDisplayModeKHR ::
  PhysicalDevice ->
  DisplayKHR ->
    Ptr DisplayModeCreateInfoKHR ->
      Ptr AllocationCallbacks -> Ptr DisplayModeKHR -> IO VkResult


data DisplayPlanePropertiesKHR =
  DisplayPlanePropertiesKHR{ currentDisplay :: DisplayKHR 
                           , currentStackIndex :: Word32 
                           }
  deriving (Eq)

instance Storable DisplayPlanePropertiesKHR where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = DisplayPlanePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (currentDisplay (poked :: DisplayPlanePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (currentStackIndex (poked :: DisplayPlanePropertiesKHR))


-- ** vkGetDisplayPlaneCapabilitiesKHR
foreign import ccall "vkGetDisplayPlaneCapabilitiesKHR" vkGetDisplayPlaneCapabilitiesKHR ::
  PhysicalDevice ->
  DisplayModeKHR ->
    Word32 -> Ptr DisplayPlaneCapabilitiesKHR -> IO VkResult


data DisplayModePropertiesKHR =
  DisplayModePropertiesKHR{ displayMode :: DisplayModeKHR 
                          , parameters :: DisplayModeParametersKHR 
                          }
  deriving (Eq)

instance Storable DisplayModePropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = DisplayModePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (displayMode (poked :: DisplayModePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (parameters (poked :: DisplayModePropertiesKHR))


-- ** VkDisplayPlaneAlphaFlagsKHR

newtype VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagsKHR VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show VkDisplayPlaneAlphaFlagsKHR where
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR"
  showsPrec _ VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = showString "VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR"
  
  showsPrec p (VkDisplayPlaneAlphaFlagsKHR x) = showParen (p >= 11) (showString "VkDisplayPlaneAlphaFlagsKHR " . showsPrec 11 x)

instance Read VkDisplayPlaneAlphaFlagsKHR where
  readPrec = parens ( choose [ ("VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR)
                             , ("VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR", pure VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDisplayPlaneAlphaFlagsKHR")
                        v <- step readPrec
                        pure (VkDisplayPlaneAlphaFlagsKHR v)
                        )
                    )


pattern VK_DISPLAY_PLANE_ALPHA_OPAQUE_BIT_KHR = VkDisplayPlaneAlphaFlagsKHR 0x1

pattern VK_DISPLAY_PLANE_ALPHA_GLOBAL_BIT_KHR = VkDisplayPlaneAlphaFlagsKHR 0x2

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_BIT_KHR = VkDisplayPlaneAlphaFlagsKHR 0x4

pattern VK_DISPLAY_PLANE_ALPHA_PER_PIXEL_PREMULTIPLIED_BIT_KHR = VkDisplayPlaneAlphaFlagsKHR 0x8


-- ** VkDisplayModeCreateFlagsKHR
-- | Opaque flag
newtype VkDisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR VkFlags
  deriving (Eq, Storable)


data DisplayModeCreateInfoKHR =
  DisplayModeCreateInfoKHR{ sType :: VkStructureType 
                          , pNext :: Ptr Void 
                          , flags :: VkDisplayModeCreateFlagsKHR 
                          , parameters :: DisplayModeParametersKHR 
                          }
  deriving (Eq)

instance Storable DisplayModeCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = DisplayModeCreateInfoKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DisplayModeCreateInfoKHR))
                *> poke (ptr `plusPtr` 20) (parameters (poked :: DisplayModeCreateInfoKHR))


-- ** vkGetPhysicalDeviceDisplayPlanePropertiesKHR
foreign import ccall "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" vkGetPhysicalDeviceDisplayPlanePropertiesKHR ::
  PhysicalDevice ->
  Ptr Word32 -> Ptr DisplayPlanePropertiesKHR -> IO VkResult

newtype DisplayModeKHR = DisplayModeKHR Word64
  deriving (Eq, Storable)


data DisplayModeParametersKHR =
  DisplayModeParametersKHR{ visibleRegion :: Extent2D 
                          , refreshRate :: Word32 
                          }
  deriving (Eq)

instance Storable DisplayModeParametersKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = DisplayModeParametersKHR <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (visibleRegion (poked :: DisplayModeParametersKHR))
                *> poke (ptr `plusPtr` 8) (refreshRate (poked :: DisplayModeParametersKHR))


-- ** VkDisplaySurfaceCreateFlagsKHR
-- | Opaque flag
newtype VkDisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR VkFlags
  deriving (Eq, Storable)

newtype DisplayKHR = DisplayKHR Word64
  deriving (Eq, Storable)

-- ** vkGetPhysicalDeviceDisplayPropertiesKHR
foreign import ccall "vkGetPhysicalDeviceDisplayPropertiesKHR" vkGetPhysicalDeviceDisplayPropertiesKHR ::
  PhysicalDevice ->
  Ptr Word32 -> Ptr DisplayPropertiesKHR -> IO VkResult

-- ** vkCreateDisplayPlaneSurfaceKHR
foreign import ccall "vkCreateDisplayPlaneSurfaceKHR" vkCreateDisplayPlaneSurfaceKHR ::
  Instance ->
  Ptr DisplaySurfaceCreateInfoKHR ->
    Ptr AllocationCallbacks -> Ptr SurfaceKHR -> IO VkResult

