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

-- ** getPhysicalDeviceSurfaceFormatsKHR
foreign import ccall "vkGetPhysicalDeviceSurfaceFormatsKHR" getPhysicalDeviceSurfaceFormatsKHR ::
  PhysicalDevice ->
  SurfaceKHR -> Ptr Word32 -> Ptr SurfaceFormatKHR -> IO Result

-- ** getPhysicalDeviceSurfaceCapabilitiesKHR
foreign import ccall "vkGetPhysicalDeviceSurfaceCapabilitiesKHR" getPhysicalDeviceSurfaceCapabilitiesKHR ::
  PhysicalDevice ->
  SurfaceKHR -> Ptr SurfaceCapabilitiesKHR -> IO Result

-- ** CompositeAlphaFlagsKHR

newtype CompositeAlphaFlagsKHR = CompositeAlphaFlagsKHR Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show CompositeAlphaFlagsKHR where
  showsPrec _ CompositeAlphaOpaqueBitKhr = showString "CompositeAlphaOpaqueBitKhr"
  showsPrec _ CompositeAlphaPreMultipliedBitKhr = showString "CompositeAlphaPreMultipliedBitKhr"
  showsPrec _ CompositeAlphaPostMultipliedBitKhr = showString "CompositeAlphaPostMultipliedBitKhr"
  showsPrec _ CompositeAlphaInheritBitKhr = showString "CompositeAlphaInheritBitKhr"
  
  showsPrec p (CompositeAlphaFlagsKHR x) = showParen (p >= 11) (showString "CompositeAlphaFlagsKHR " . showsPrec 11 x)

instance Read CompositeAlphaFlagsKHR where
  readPrec = parens ( choose [ ("CompositeAlphaOpaqueBitKhr", pure CompositeAlphaOpaqueBitKhr)
                             , ("CompositeAlphaPreMultipliedBitKhr", pure CompositeAlphaPreMultipliedBitKhr)
                             , ("CompositeAlphaPostMultipliedBitKhr", pure CompositeAlphaPostMultipliedBitKhr)
                             , ("CompositeAlphaInheritBitKhr", pure CompositeAlphaInheritBitKhr)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CompositeAlphaFlagsKHR")
                        v <- step readPrec
                        pure (CompositeAlphaFlagsKHR v)
                        )
                    )


pattern CompositeAlphaOpaqueBitKhr = CompositeAlphaFlagsKHR 0x1

pattern CompositeAlphaPreMultipliedBitKhr = CompositeAlphaFlagsKHR 0x2

pattern CompositeAlphaPostMultipliedBitKhr = CompositeAlphaFlagsKHR 0x4

pattern CompositeAlphaInheritBitKhr = CompositeAlphaFlagsKHR 0x8


-- ** PresentModeKHR

newtype PresentModeKHR = PresentModeKHR Int32
  deriving (Eq, Ord, Storable)

instance Show PresentModeKHR where
  showsPrec _ PresentModeImmediateKhr = showString "PresentModeImmediateKhr"
  showsPrec _ PresentModeMailboxKhr = showString "PresentModeMailboxKhr"
  showsPrec _ PresentModeFifoKhr = showString "PresentModeFifoKhr"
  showsPrec _ PresentModeFifoRelaxedKhr = showString "PresentModeFifoRelaxedKhr"
  showsPrec p (PresentModeKHR x) = showParen (p >= 11) (showString "PresentModeKHR " . showsPrec 11 x)

instance Read PresentModeKHR where
  readPrec = parens ( choose [ ("PresentModeImmediateKhr", pure PresentModeImmediateKhr)
                             , ("PresentModeMailboxKhr", pure PresentModeMailboxKhr)
                             , ("PresentModeFifoKhr", pure PresentModeFifoKhr)
                             , ("PresentModeFifoRelaxedKhr", pure PresentModeFifoRelaxedKhr)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PresentModeKHR")
                        v <- step readPrec
                        pure (PresentModeKHR v)
                        )
                    )


pattern PresentModeImmediateKhr = PresentModeKHR 0

pattern PresentModeMailboxKhr = PresentModeKHR 1

pattern PresentModeFifoKhr = PresentModeKHR 2

pattern PresentModeFifoRelaxedKhr = PresentModeKHR 3

newtype SurfaceKHR = SurfaceKHR Word64
  deriving (Eq, Ord, Storable)

-- ** getPhysicalDeviceSurfaceSupportKHR
foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR" getPhysicalDeviceSurfaceSupportKHR ::
  PhysicalDevice -> Word32 -> SurfaceKHR -> Ptr Bool32 -> IO Result


data SurfaceFormatKHR =
  SurfaceFormatKHR{ format :: Format 
                  , colorSpace :: ColorSpaceKHR 
                  }
  deriving (Eq, Ord)

instance Storable SurfaceFormatKHR where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = SurfaceFormatKHR <$> peek (ptr `plusPtr` 0)
                              <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (format (poked :: SurfaceFormatKHR))
                *> poke (ptr `plusPtr` 4) (colorSpace (poked :: SurfaceFormatKHR))


-- ** destroySurfaceKHR
foreign import ccall "vkDestroySurfaceKHR" destroySurfaceKHR ::
  Instance -> SurfaceKHR -> Ptr AllocationCallbacks -> IO ()

-- ** ColorSpaceKHR

newtype ColorSpaceKHR = ColorSpaceKHR Int32
  deriving (Eq, Ord, Storable)

instance Show ColorSpaceKHR where
  showsPrec _ ColorspaceSrgbNonlinearKhr = showString "ColorspaceSrgbNonlinearKhr"
  showsPrec p (ColorSpaceKHR x) = showParen (p >= 11) (showString "ColorSpaceKHR " . showsPrec 11 x)

instance Read ColorSpaceKHR where
  readPrec = parens ( choose [ ("ColorspaceSrgbNonlinearKhr", pure ColorspaceSrgbNonlinearKhr)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ColorSpaceKHR")
                        v <- step readPrec
                        pure (ColorSpaceKHR v)
                        )
                    )


pattern ColorspaceSrgbNonlinearKhr = ColorSpaceKHR 0

-- ** getPhysicalDeviceSurfacePresentModesKHR
foreign import ccall "vkGetPhysicalDeviceSurfacePresentModesKHR" getPhysicalDeviceSurfacePresentModesKHR ::
  PhysicalDevice ->
  SurfaceKHR -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result

-- ** SurfaceTransformFlagsKHR

newtype SurfaceTransformFlagsKHR = SurfaceTransformFlagsKHR Flags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show SurfaceTransformFlagsKHR where
  showsPrec _ SurfaceTransformIdentityBitKhr = showString "SurfaceTransformIdentityBitKhr"
  showsPrec _ SurfaceTransformRotate90BitKhr = showString "SurfaceTransformRotate90BitKhr"
  showsPrec _ SurfaceTransformRotate180BitKhr = showString "SurfaceTransformRotate180BitKhr"
  showsPrec _ SurfaceTransformRotate270BitKhr = showString "SurfaceTransformRotate270BitKhr"
  showsPrec _ SurfaceTransformHorizontalMirrorBitKhr = showString "SurfaceTransformHorizontalMirrorBitKhr"
  showsPrec _ SurfaceTransformHorizontalMirrorRotate90BitKhr = showString "SurfaceTransformHorizontalMirrorRotate90BitKhr"
  showsPrec _ SurfaceTransformHorizontalMirrorRotate180BitKhr = showString "SurfaceTransformHorizontalMirrorRotate180BitKhr"
  showsPrec _ SurfaceTransformHorizontalMirrorRotate270BitKhr = showString "SurfaceTransformHorizontalMirrorRotate270BitKhr"
  showsPrec _ SurfaceTransformInheritBitKhr = showString "SurfaceTransformInheritBitKhr"
  
  showsPrec p (SurfaceTransformFlagsKHR x) = showParen (p >= 11) (showString "SurfaceTransformFlagsKHR " . showsPrec 11 x)

instance Read SurfaceTransformFlagsKHR where
  readPrec = parens ( choose [ ("SurfaceTransformIdentityBitKhr", pure SurfaceTransformIdentityBitKhr)
                             , ("SurfaceTransformRotate90BitKhr", pure SurfaceTransformRotate90BitKhr)
                             , ("SurfaceTransformRotate180BitKhr", pure SurfaceTransformRotate180BitKhr)
                             , ("SurfaceTransformRotate270BitKhr", pure SurfaceTransformRotate270BitKhr)
                             , ("SurfaceTransformHorizontalMirrorBitKhr", pure SurfaceTransformHorizontalMirrorBitKhr)
                             , ("SurfaceTransformHorizontalMirrorRotate90BitKhr", pure SurfaceTransformHorizontalMirrorRotate90BitKhr)
                             , ("SurfaceTransformHorizontalMirrorRotate180BitKhr", pure SurfaceTransformHorizontalMirrorRotate180BitKhr)
                             , ("SurfaceTransformHorizontalMirrorRotate270BitKhr", pure SurfaceTransformHorizontalMirrorRotate270BitKhr)
                             , ("SurfaceTransformInheritBitKhr", pure SurfaceTransformInheritBitKhr)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SurfaceTransformFlagsKHR")
                        v <- step readPrec
                        pure (SurfaceTransformFlagsKHR v)
                        )
                    )


pattern SurfaceTransformIdentityBitKhr = SurfaceTransformFlagsKHR 0x1

pattern SurfaceTransformRotate90BitKhr = SurfaceTransformFlagsKHR 0x2

pattern SurfaceTransformRotate180BitKhr = SurfaceTransformFlagsKHR 0x4

pattern SurfaceTransformRotate270BitKhr = SurfaceTransformFlagsKHR 0x8

pattern SurfaceTransformHorizontalMirrorBitKhr = SurfaceTransformFlagsKHR 0x10

pattern SurfaceTransformHorizontalMirrorRotate90BitKhr = SurfaceTransformFlagsKHR 0x20

pattern SurfaceTransformHorizontalMirrorRotate180BitKhr = SurfaceTransformFlagsKHR 0x40

pattern SurfaceTransformHorizontalMirrorRotate270BitKhr = SurfaceTransformFlagsKHR 0x80

pattern SurfaceTransformInheritBitKhr = SurfaceTransformFlagsKHR 0x100



data SurfaceCapabilitiesKHR =
  SurfaceCapabilitiesKHR{ minImageCount :: Word32 
                        , maxImageCount :: Word32 
                        , currentExtent :: Extent2D 
                        , minImageExtent :: Extent2D 
                        , maxImageExtent :: Extent2D 
                        , maxImageArrayLayers :: Word32 
                        , supportedTransforms :: SurfaceTransformFlagsKHR 
                        , currentTransform :: SurfaceTransformFlagsKHR 
                        , supportedCompositeAlpha :: CompositeAlphaFlagsKHR 
                        , supportedUsageFlags :: ImageUsageFlags 
                        }
  deriving (Eq, Ord)

instance Storable SurfaceCapabilitiesKHR where
  sizeOf ~_ = 52
  alignment ~_ = 4
  peek ptr = SurfaceCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 4)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 36)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 44)
                                    <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (minImageCount (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 4) (maxImageCount (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (currentExtent (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (minImageExtent (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 24) (maxImageExtent (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 32) (maxImageArrayLayers (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 36) (supportedTransforms (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 40) (currentTransform (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 44) (supportedCompositeAlpha (poked :: SurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 48) (supportedUsageFlags (poked :: SurfaceCapabilitiesKHR))


