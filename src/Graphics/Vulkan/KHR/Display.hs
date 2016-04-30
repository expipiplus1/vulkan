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
import Graphics.Vulkan.KHR.Surface( Surface(..)
                                  , SurfaceTransformFlags(..)
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
import Graphics.Vulkan.Core( Offset2D(..)
                           , Bool32(..)
                           , StructureType(..)
                           , Result(..)
                           , Flags(..)
                           , Extent2D(..)
                           )
import Foreign.C.Types( CFloat(..)
                      , CChar(..)
                      )


data DisplaySurfaceCreateInfo =
  DisplaySurfaceCreateInfo{ sType :: StructureType 
                          , pNext :: Ptr Void 
                          , flags :: DisplaySurfaceCreateFlags 
                          , displayMode :: DisplayMode 
                          , planeIndex :: Word32 
                          , planeStackIndex :: Word32 
                          , transform :: SurfaceTransformFlags 
                          , globalAlpha :: CFloat 
                          , alphaMode :: DisplayPlaneAlphaFlags 
                          , imageExtent :: Extent2D 
                          }
  deriving (Eq, Ord)

instance Storable DisplaySurfaceCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = DisplaySurfaceCreateInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 48)
                                      <*> peek (ptr `plusPtr` 52)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 24) (displayMode (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 32) (planeIndex (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 36) (planeStackIndex (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 40) (transform (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 44) (globalAlpha (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 48) (alphaMode (poked :: DisplaySurfaceCreateInfo))
                *> poke (ptr `plusPtr` 52) (imageExtent (poked :: DisplaySurfaceCreateInfo))



data DisplayPlaneCapabilities =
  DisplayPlaneCapabilities{ supportedAlpha :: DisplayPlaneAlphaFlags 
                          , minSrcPosition :: Offset2D 
                          , maxSrcPosition :: Offset2D 
                          , minSrcExtent :: Extent2D 
                          , maxSrcExtent :: Extent2D 
                          , minDstPosition :: Offset2D 
                          , maxDstPosition :: Offset2D 
                          , minDstExtent :: Extent2D 
                          , maxDstExtent :: Extent2D 
                          }
  deriving (Eq, Ord)

instance Storable DisplayPlaneCapabilities where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = DisplayPlaneCapabilities <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 12)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 28)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 52)
                                      <*> peek (ptr `plusPtr` 60)
  poke ptr poked = poke (ptr `plusPtr` 0) (supportedAlpha (poked :: DisplayPlaneCapabilities))
                *> poke (ptr `plusPtr` 4) (minSrcPosition (poked :: DisplayPlaneCapabilities))
                *> poke (ptr `plusPtr` 12) (maxSrcPosition (poked :: DisplayPlaneCapabilities))
                *> poke (ptr `plusPtr` 20) (minSrcExtent (poked :: DisplayPlaneCapabilities))
                *> poke (ptr `plusPtr` 28) (maxSrcExtent (poked :: DisplayPlaneCapabilities))
                *> poke (ptr `plusPtr` 36) (minDstPosition (poked :: DisplayPlaneCapabilities))
                *> poke (ptr `plusPtr` 44) (maxDstPosition (poked :: DisplayPlaneCapabilities))
                *> poke (ptr `plusPtr` 52) (minDstExtent (poked :: DisplayPlaneCapabilities))
                *> poke (ptr `plusPtr` 60) (maxDstExtent (poked :: DisplayPlaneCapabilities))


-- ** getDisplayModeProperties
foreign import ccall "vkGetDisplayModePropertiesKHR" getDisplayModeProperties ::
  PhysicalDevice ->
  Display -> Ptr Word32 -> Ptr DisplayModeProperties -> IO Result


data DisplayProperties =
  DisplayProperties{ display :: Display 
                   , displayName :: Ptr CChar 
                   , physicalDimensions :: Extent2D 
                   , physicalResolution :: Extent2D 
                   , supportedTransforms :: SurfaceTransformFlags 
                   , planeReorderPossible :: Bool32 
                   , persistentContent :: Bool32 
                   }
  deriving (Eq, Ord)

instance Storable DisplayProperties where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = DisplayProperties <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 36)
                               <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (display (poked :: DisplayProperties))
                *> poke (ptr `plusPtr` 8) (displayName (poked :: DisplayProperties))
                *> poke (ptr `plusPtr` 16) (physicalDimensions (poked :: DisplayProperties))
                *> poke (ptr `plusPtr` 24) (physicalResolution (poked :: DisplayProperties))
                *> poke (ptr `plusPtr` 32) (supportedTransforms (poked :: DisplayProperties))
                *> poke (ptr `plusPtr` 36) (planeReorderPossible (poked :: DisplayProperties))
                *> poke (ptr `plusPtr` 40) (persistentContent (poked :: DisplayProperties))


-- ** getDisplayPlaneSupportedDisplays
foreign import ccall "vkGetDisplayPlaneSupportedDisplaysKHR" getDisplayPlaneSupportedDisplays ::
  PhysicalDevice -> Word32 -> Ptr Word32 -> Ptr Display -> IO Result

-- ** createDisplayMode
foreign import ccall "vkCreateDisplayModeKHR" createDisplayMode ::
  PhysicalDevice ->
  Display ->
    Ptr DisplayModeCreateInfo ->
      Ptr AllocationCallbacks -> Ptr DisplayMode -> IO Result


data DisplayPlaneProperties =
  DisplayPlaneProperties{ currentDisplay :: Display 
                        , currentStackIndex :: Word32 
                        }
  deriving (Eq, Ord)

instance Storable DisplayPlaneProperties where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = DisplayPlaneProperties <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (currentDisplay (poked :: DisplayPlaneProperties))
                *> poke (ptr `plusPtr` 8) (currentStackIndex (poked :: DisplayPlaneProperties))


-- ** getDisplayPlaneCapabilities
foreign import ccall "vkGetDisplayPlaneCapabilitiesKHR" getDisplayPlaneCapabilities ::
  PhysicalDevice ->
  DisplayMode -> Word32 -> Ptr DisplayPlaneCapabilities -> IO Result


data DisplayModeProperties =
  DisplayModeProperties{ displayMode :: DisplayMode 
                       , parameters :: DisplayModeParameters 
                       }
  deriving (Eq, Ord)

instance Storable DisplayModeProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = DisplayModeProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (displayMode (poked :: DisplayModeProperties))
                *> poke (ptr `plusPtr` 8) (parameters (poked :: DisplayModeProperties))


-- ** DisplayPlaneAlphaFlags

newtype DisplayPlaneAlphaFlags = DisplayPlaneAlphaFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show DisplayPlaneAlphaFlags where
  showsPrec _ DisplayPlaneAlphaOpaqueBit = showString "DisplayPlaneAlphaOpaqueBit"
  showsPrec _ DisplayPlaneAlphaGlobalBit = showString "DisplayPlaneAlphaGlobalBit"
  showsPrec _ DisplayPlaneAlphaPerPixelBit = showString "DisplayPlaneAlphaPerPixelBit"
  showsPrec _ DisplayPlaneAlphaPerPixelPremultipliedBit = showString "DisplayPlaneAlphaPerPixelPremultipliedBit"
  
  showsPrec p (DisplayPlaneAlphaFlags x) = showParen (p >= 11) (showString "DisplayPlaneAlphaFlags " . showsPrec 11 x)

instance Read DisplayPlaneAlphaFlags where
  readPrec = parens ( choose [ ("DisplayPlaneAlphaOpaqueBit", pure DisplayPlaneAlphaOpaqueBit)
                             , ("DisplayPlaneAlphaGlobalBit", pure DisplayPlaneAlphaGlobalBit)
                             , ("DisplayPlaneAlphaPerPixelBit", pure DisplayPlaneAlphaPerPixelBit)
                             , ("DisplayPlaneAlphaPerPixelPremultipliedBit", pure DisplayPlaneAlphaPerPixelPremultipliedBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DisplayPlaneAlphaFlags")
                        v <- step readPrec
                        pure (DisplayPlaneAlphaFlags v)
                        )
                    )


pattern DisplayPlaneAlphaOpaqueBit = DisplayPlaneAlphaFlags 0x1

pattern DisplayPlaneAlphaGlobalBit = DisplayPlaneAlphaFlags 0x2

pattern DisplayPlaneAlphaPerPixelBit = DisplayPlaneAlphaFlags 0x4

pattern DisplayPlaneAlphaPerPixelPremultipliedBit = DisplayPlaneAlphaFlags 0x8


-- ** DisplayModeCreateFlags
-- | Opaque flag
newtype DisplayModeCreateFlags = DisplayModeCreateFlags Flags
  deriving (Eq, Ord, Storable)


data DisplayModeCreateInfo =
  DisplayModeCreateInfo{ sType :: StructureType 
                       , pNext :: Ptr Void 
                       , flags :: DisplayModeCreateFlags 
                       , parameters :: DisplayModeParameters 
                       }
  deriving (Eq, Ord)

instance Storable DisplayModeCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = DisplayModeCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DisplayModeCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DisplayModeCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DisplayModeCreateInfo))
                *> poke (ptr `plusPtr` 20) (parameters (poked :: DisplayModeCreateInfo))


-- ** getPhysicalDeviceDisplayPlaneProperties
foreign import ccall "vkGetPhysicalDeviceDisplayPlanePropertiesKHR" getPhysicalDeviceDisplayPlaneProperties ::
  PhysicalDevice ->
  Ptr Word32 -> Ptr DisplayPlaneProperties -> IO Result

newtype DisplayMode = DisplayMode Word64
  deriving (Eq, Ord, Storable)


data DisplayModeParameters =
  DisplayModeParameters{ visibleRegion :: Extent2D 
                       , refreshRate :: Word32 
                       }
  deriving (Eq, Ord)

instance Storable DisplayModeParameters where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = DisplayModeParameters <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (visibleRegion (poked :: DisplayModeParameters))
                *> poke (ptr `plusPtr` 8) (refreshRate (poked :: DisplayModeParameters))


-- ** DisplaySurfaceCreateFlags
-- | Opaque flag
newtype DisplaySurfaceCreateFlags = DisplaySurfaceCreateFlags Flags
  deriving (Eq, Ord, Storable)

newtype Display = Display Word64
  deriving (Eq, Ord, Storable)

-- ** getPhysicalDeviceDisplayProperties
foreign import ccall "vkGetPhysicalDeviceDisplayPropertiesKHR" getPhysicalDeviceDisplayProperties ::
  PhysicalDevice -> Ptr Word32 -> Ptr DisplayProperties -> IO Result

-- ** createDisplayPlaneSurface
foreign import ccall "vkCreateDisplayPlaneSurfaceKHR" createDisplayPlaneSurface ::
  Instance ->
  Ptr DisplaySurfaceCreateInfo ->
    Ptr AllocationCallbacks -> Ptr Surface -> IO Result

