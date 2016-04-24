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

-- ** VkCompositeAlphaFlagsKHR

newtype CompositeAlphaFlagsKHR = CompositeAlphaFlagsKHR Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show CompositeAlphaFlagsKHR where
  showsPrec _ VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = showString "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = showString "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
  
  showsPrec p (CompositeAlphaFlagsKHR x) = showParen (p >= 11) (showString "CompositeAlphaFlagsKHR " . showsPrec 11 x)

instance Read CompositeAlphaFlagsKHR where
  readPrec = parens ( choose [ ("VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR", pure VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR", pure VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR", pure VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR", pure VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "CompositeAlphaFlagsKHR")
                        v <- step readPrec
                        pure (CompositeAlphaFlagsKHR v)
                        )
                    )


pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = CompositeAlphaFlagsKHR 0x1

pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = CompositeAlphaFlagsKHR 0x2

pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = CompositeAlphaFlagsKHR 0x4

pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = CompositeAlphaFlagsKHR 0x8


-- ** PresentModeKHR

newtype PresentModeKHR = PresentModeKHR Int32
  deriving (Eq, Storable)

instance Show PresentModeKHR where
  showsPrec _ VK_PRESENT_MODE_IMMEDIATE_KHR = showString "VK_PRESENT_MODE_IMMEDIATE_KHR"
  showsPrec _ VK_PRESENT_MODE_MAILBOX_KHR = showString "VK_PRESENT_MODE_MAILBOX_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_KHR = showString "VK_PRESENT_MODE_FIFO_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_RELAXED_KHR = showString "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
  showsPrec p (PresentModeKHR x) = showParen (p >= 11) (showString "PresentModeKHR " . showsPrec 11 x)

instance Read PresentModeKHR where
  readPrec = parens ( choose [ ("VK_PRESENT_MODE_IMMEDIATE_KHR", pure VK_PRESENT_MODE_IMMEDIATE_KHR)
                             , ("VK_PRESENT_MODE_MAILBOX_KHR", pure VK_PRESENT_MODE_MAILBOX_KHR)
                             , ("VK_PRESENT_MODE_FIFO_KHR", pure VK_PRESENT_MODE_FIFO_KHR)
                             , ("VK_PRESENT_MODE_FIFO_RELAXED_KHR", pure VK_PRESENT_MODE_FIFO_RELAXED_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "PresentModeKHR")
                        v <- step readPrec
                        pure (PresentModeKHR v)
                        )
                    )


pattern VK_PRESENT_MODE_IMMEDIATE_KHR = PresentModeKHR 0

pattern VK_PRESENT_MODE_MAILBOX_KHR = PresentModeKHR 1

pattern VK_PRESENT_MODE_FIFO_KHR = PresentModeKHR 2

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = PresentModeKHR 3

newtype SurfaceKHR = SurfaceKHR Word64
  deriving (Eq, Storable)

-- ** getPhysicalDeviceSurfaceSupportKHR
foreign import ccall "vkGetPhysicalDeviceSurfaceSupportKHR" getPhysicalDeviceSurfaceSupportKHR ::
  PhysicalDevice -> Word32 -> SurfaceKHR -> Ptr Bool32 -> IO Result


data SurfaceFormatKHR =
  SurfaceFormatKHR{ format :: Format 
                  , colorSpace :: ColorSpaceKHR 
                  }
  deriving (Eq)

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
  deriving (Eq, Storable)

instance Show ColorSpaceKHR where
  showsPrec _ VK_COLORSPACE_SRGB_NONLINEAR_KHR = showString "VK_COLORSPACE_SRGB_NONLINEAR_KHR"
  showsPrec p (ColorSpaceKHR x) = showParen (p >= 11) (showString "ColorSpaceKHR " . showsPrec 11 x)

instance Read ColorSpaceKHR where
  readPrec = parens ( choose [ ("VK_COLORSPACE_SRGB_NONLINEAR_KHR", pure VK_COLORSPACE_SRGB_NONLINEAR_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "ColorSpaceKHR")
                        v <- step readPrec
                        pure (ColorSpaceKHR v)
                        )
                    )


pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR = ColorSpaceKHR 0

-- ** getPhysicalDeviceSurfacePresentModesKHR
foreign import ccall "vkGetPhysicalDeviceSurfacePresentModesKHR" getPhysicalDeviceSurfacePresentModesKHR ::
  PhysicalDevice ->
  SurfaceKHR -> Ptr Word32 -> Ptr PresentModeKHR -> IO Result

-- ** VkSurfaceTransformFlagsKHR

newtype SurfaceTransformFlagsKHR = SurfaceTransformFlagsKHR Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show SurfaceTransformFlagsKHR where
  showsPrec _ VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = showString "VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = showString "VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = showString "VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR"
  showsPrec _ VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = showString "VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR"
  
  showsPrec p (SurfaceTransformFlagsKHR x) = showParen (p >= 11) (showString "SurfaceTransformFlagsKHR " . showsPrec 11 x)

instance Read SurfaceTransformFlagsKHR where
  readPrec = parens ( choose [ ("VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR", pure VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR", pure VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR", pure VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR", pure VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR", pure VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR)
                             , ("VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR", pure VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SurfaceTransformFlagsKHR")
                        v <- step readPrec
                        pure (SurfaceTransformFlagsKHR v)
                        )
                    )


pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = SurfaceTransformFlagsKHR 0x1

pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = SurfaceTransformFlagsKHR 0x2

pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = SurfaceTransformFlagsKHR 0x4

pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = SurfaceTransformFlagsKHR 0x8

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = SurfaceTransformFlagsKHR 0x10

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = SurfaceTransformFlagsKHR 0x20

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = SurfaceTransformFlagsKHR 0x40

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = SurfaceTransformFlagsKHR 0x80

pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = SurfaceTransformFlagsKHR 0x100



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
  deriving (Eq)

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


