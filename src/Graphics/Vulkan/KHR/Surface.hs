{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Surface where

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
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
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
import Graphics.Vulkan.Image( ImageUsageFlags(..)
                            )
import Graphics.Vulkan.DeviceInitialization( Instance(..)
                                           )
import Graphics.Vulkan.Core( Bool32(..)
                           , Format(..)
                           , Result(..)
                           , Flags(..)
                           , Extent2D(..)
                           )

-- ** getPhysicalDeviceSurfaceFormats
foreign import ccall "vkGetPhysicalDeviceSurfaceFormatsKHR" getPhysicalDeviceSurfaceFormats ::
  PhysicalDevice ->
  Surface -> Ptr Word32 -> Ptr SurfaceFormat -> IO Result

-- ** getPhysicalDeviceSurfaceCapabilities
foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" getPhysicalDeviceSurfaceCapabilities ::
  PhysicalDevice -> Surface -> Ptr SurfaceCapabilities -> IO Result

-- ** CompositeAlphaFlags

newtype CompositeAlphaFlags = CompositeAlphaFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show CompositeAlphaFlags where
  showsPrec _ CompositeAlphaOpaqueBit = showString "CompositeAlphaOpaqueBit"
  showsPrec _ CompositeAlphaPreMultipliedBit = showString "CompositeAlphaPreMultipliedBit"
  showsPrec _ CompositeAlphaPostMultipliedBit = showString "CompositeAlphaPostMultipliedBit"
  showsPrec _ CompositeAlphaInheritBit = showString "CompositeAlphaInheritBit"
  
  showsPrec p (CompositeAlphaFlags x) = showParen (p >= 11) (showString "CompositeAlphaFlags " . showsPrec 11 x)

instance Read CompositeAlphaFlags where
  readPrec = parens ( choose [ ("CompositeAlphaOpaqueBit", pure CompositeAlphaOpaqueBit)
                             , ("CompositeAlphaPreMultipliedBit", pure CompositeAlphaPreMultipliedBit)
                             , ("CompositeAlphaPostMultipliedBit", pure CompositeAlphaPostMultipliedBit)
                             , ("CompositeAlphaInheritBit", pure CompositeAlphaInheritBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CompositeAlphaFlags")
                        v <- step readPrec
                        pure (CompositeAlphaFlags v)
                        )
                    )


pattern CompositeAlphaOpaqueBit = CompositeAlphaFlags 0x1

pattern CompositeAlphaPreMultipliedBit = CompositeAlphaFlags 0x2

pattern CompositeAlphaPostMultipliedBit = CompositeAlphaFlags 0x4

pattern CompositeAlphaInheritBit = CompositeAlphaFlags 0x8


-- ** PresentMode

newtype PresentMode = PresentMode Int32
  deriving (Eq, Ord, Storable)

instance Show PresentMode where
  showsPrec _ PresentModeImmediate = showString "PresentModeImmediate"
  showsPrec _ PresentModeMailbox = showString "PresentModeMailbox"
  showsPrec _ PresentModeFifo = showString "PresentModeFifo"
  showsPrec _ PresentModeFifoRelaxed = showString "PresentModeFifoRelaxed"
  showsPrec p (PresentMode x) = showParen (p >= 11) (showString "PresentMode " . showsPrec 11 x)

instance Read PresentMode where
  readPrec = parens ( choose [ ("PresentModeImmediate", pure PresentModeImmediate)
                             , ("PresentModeMailbox", pure PresentModeMailbox)
                             , ("PresentModeFifo", pure PresentModeFifo)
                             , ("PresentModeFifoRelaxed", pure PresentModeFifoRelaxed)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PresentMode")
                        v <- step readPrec
                        pure (PresentMode v)
                        )
                    )


pattern PresentModeImmediate = PresentMode 0

pattern PresentModeMailbox = PresentMode 1

pattern PresentModeFifo = PresentMode 2

pattern PresentModeFifoRelaxed = PresentMode 3

newtype Surface = Surface Word64
  deriving (Eq, Ord, Storable)

-- ** getPhysicalDeviceSurfaceSupport
foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR" getPhysicalDeviceSurfaceSupport ::
  PhysicalDevice -> Word32 -> Surface -> Ptr Bool32 -> IO Result


data SurfaceFormat =
  SurfaceFormat{ format :: Format 
               , colorSpace :: ColorSpace 
               }
  deriving (Eq, Ord)

instance Storable SurfaceFormat where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = SurfaceFormat <$> peek (ptr `plusPtr` 0)
                           <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (format (poked :: SurfaceFormat))
                *> poke (ptr `plusPtr` 4) (colorSpace (poked :: SurfaceFormat))


-- ** destroySurface
foreign import ccall "vkDestroySurfaceKHR" destroySurface ::
  Instance -> Surface -> Ptr AllocationCallbacks -> IO ()

-- ** ColorSpace

newtype ColorSpace = ColorSpace Int32
  deriving (Eq, Ord, Storable)

instance Show ColorSpace where
  showsPrec _ ColorspaceSrgbNonlinear = showString "ColorspaceSrgbNonlinear"
  showsPrec p (ColorSpace x) = showParen (p >= 11) (showString "ColorSpace " . showsPrec 11 x)

instance Read ColorSpace where
  readPrec = parens ( choose [ ("ColorspaceSrgbNonlinear", pure ColorspaceSrgbNonlinear)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ColorSpace")
                        v <- step readPrec
                        pure (ColorSpace v)
                        )
                    )


pattern ColorspaceSrgbNonlinear = ColorSpace 0

-- ** getPhysicalDeviceSurfacePresentModes
foreign import ccall "vkGetPhysicalDeviceSurfacePresentModesKHR" getPhysicalDeviceSurfacePresentModes ::
  PhysicalDevice ->
  Surface -> Ptr Word32 -> Ptr PresentMode -> IO Result

-- ** SurfaceTransformFlags

newtype SurfaceTransformFlags = SurfaceTransformFlags Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show SurfaceTransformFlags where
  showsPrec _ SurfaceTransformIdentityBit = showString "SurfaceTransformIdentityBit"
  showsPrec _ SurfaceTransformRotate90Bit = showString "SurfaceTransformRotate90Bit"
  showsPrec _ SurfaceTransformRotate180Bit = showString "SurfaceTransformRotate180Bit"
  showsPrec _ SurfaceTransformRotate270Bit = showString "SurfaceTransformRotate270Bit"
  showsPrec _ SurfaceTransformHorizontalMirrorBit = showString "SurfaceTransformHorizontalMirrorBit"
  showsPrec _ SurfaceTransformHorizontalMirrorRotate90Bit = showString "SurfaceTransformHorizontalMirrorRotate90Bit"
  showsPrec _ SurfaceTransformHorizontalMirrorRotate180Bit = showString "SurfaceTransformHorizontalMirrorRotate180Bit"
  showsPrec _ SurfaceTransformHorizontalMirrorRotate270Bit = showString "SurfaceTransformHorizontalMirrorRotate270Bit"
  showsPrec _ SurfaceTransformInheritBit = showString "SurfaceTransformInheritBit"
  
  showsPrec p (SurfaceTransformFlags x) = showParen (p >= 11) (showString "SurfaceTransformFlags " . showsPrec 11 x)

instance Read SurfaceTransformFlags where
  readPrec = parens ( choose [ ("SurfaceTransformIdentityBit", pure SurfaceTransformIdentityBit)
                             , ("SurfaceTransformRotate90Bit", pure SurfaceTransformRotate90Bit)
                             , ("SurfaceTransformRotate180Bit", pure SurfaceTransformRotate180Bit)
                             , ("SurfaceTransformRotate270Bit", pure SurfaceTransformRotate270Bit)
                             , ("SurfaceTransformHorizontalMirrorBit", pure SurfaceTransformHorizontalMirrorBit)
                             , ("SurfaceTransformHorizontalMirrorRotate90Bit", pure SurfaceTransformHorizontalMirrorRotate90Bit)
                             , ("SurfaceTransformHorizontalMirrorRotate180Bit", pure SurfaceTransformHorizontalMirrorRotate180Bit)
                             , ("SurfaceTransformHorizontalMirrorRotate270Bit", pure SurfaceTransformHorizontalMirrorRotate270Bit)
                             , ("SurfaceTransformInheritBit", pure SurfaceTransformInheritBit)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SurfaceTransformFlags")
                        v <- step readPrec
                        pure (SurfaceTransformFlags v)
                        )
                    )


pattern SurfaceTransformIdentityBit = SurfaceTransformFlags 0x1

pattern SurfaceTransformRotate90Bit = SurfaceTransformFlags 0x2

pattern SurfaceTransformRotate180Bit = SurfaceTransformFlags 0x4

pattern SurfaceTransformRotate270Bit = SurfaceTransformFlags 0x8

pattern SurfaceTransformHorizontalMirrorBit = SurfaceTransformFlags 0x10

pattern SurfaceTransformHorizontalMirrorRotate90Bit = SurfaceTransformFlags 0x20

pattern SurfaceTransformHorizontalMirrorRotate180Bit = SurfaceTransformFlags 0x40

pattern SurfaceTransformHorizontalMirrorRotate270Bit = SurfaceTransformFlags 0x80

pattern SurfaceTransformInheritBit = SurfaceTransformFlags 0x100



data SurfaceCapabilities =
  SurfaceCapabilities{ minImageCount :: Word32 
                     , maxImageCount :: Word32 
                     , currentExtent :: Extent2D 
                     , minImageExtent :: Extent2D 
                     , maxImageExtent :: Extent2D 
                     , maxImageArrayLayers :: Word32 
                     , supportedTransforms :: SurfaceTransformFlags 
                     , currentTransform :: SurfaceTransformFlags 
                     , supportedCompositeAlpha :: CompositeAlphaFlags 
                     , supportedUsageFlags :: ImageUsageFlags 
                     }
  deriving (Eq, Ord)

instance Storable SurfaceCapabilities where
  sizeOf ~_ = 52
  alignment ~_ = 4
  peek ptr = SurfaceCapabilities <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 36)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 44)
                                 <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (minImageCount (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 4) (maxImageCount (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 8) (currentExtent (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 16) (minImageExtent (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 24) (maxImageExtent (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 32) (maxImageArrayLayers (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 36) (supportedTransforms (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 40) (currentTransform (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 44) (supportedCompositeAlpha (poked :: SurfaceCapabilities))
                *> poke (ptr `plusPtr` 48) (supportedUsageFlags (poked :: SurfaceCapabilities))


