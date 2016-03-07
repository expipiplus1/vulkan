{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.KHR.Surface where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Core( VkFlags(..)
                           )

-- ** VkCompositeAlphaFlagsKHR

newtype VkCompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkCompositeAlphaFlagBitsKHR
type VkCompositeAlphaFlagsKHR = VkCompositeAlphaFlagBitsKHR

instance Show VkCompositeAlphaFlagBitsKHR where
  showsPrec _ VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = showString "VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = showString "VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR"
  showsPrec _ VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = showString "VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR"
  
  showsPrec p (VkCompositeAlphaFlagBitsKHR x) = showParen (p >= 11) (showString "VkCompositeAlphaFlagBitsKHR " . showsPrec 11 x)

instance Read VkCompositeAlphaFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR", pure VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR", pure VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR", pure VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR)
                             , ("VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR", pure VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkCompositeAlphaFlagBitsKHR")
                        v <- step readPrec
                        pure (VkCompositeAlphaFlagBitsKHR v)
                        )
                    )


pattern VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x1

pattern VK_COMPOSITE_ALPHA_PRE_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x2

pattern VK_COMPOSITE_ALPHA_POST_MULTIPLIED_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x4

pattern VK_COMPOSITE_ALPHA_INHERIT_BIT_KHR = VkCompositeAlphaFlagBitsKHR 0x8


-- ** VkPresentModeKHR

newtype VkPresentModeKHR = VkPresentModeKHR Int32
  deriving (Eq, Storable)

instance Show VkPresentModeKHR where
  showsPrec _ VK_PRESENT_MODE_IMMEDIATE_KHR = showString "VK_PRESENT_MODE_IMMEDIATE_KHR"
  showsPrec _ VK_PRESENT_MODE_MAILBOX_KHR = showString "VK_PRESENT_MODE_MAILBOX_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_KHR = showString "VK_PRESENT_MODE_FIFO_KHR"
  showsPrec _ VK_PRESENT_MODE_FIFO_RELAXED_KHR = showString "VK_PRESENT_MODE_FIFO_RELAXED_KHR"
  showsPrec p (VkPresentModeKHR x) = showParen (p >= 11) (showString "VkPresentModeKHR " . showsPrec 11 x)

instance Read VkPresentModeKHR where
  readPrec = parens ( choose [ ("VK_PRESENT_MODE_IMMEDIATE_KHR", pure VK_PRESENT_MODE_IMMEDIATE_KHR)
                             , ("VK_PRESENT_MODE_MAILBOX_KHR", pure VK_PRESENT_MODE_MAILBOX_KHR)
                             , ("VK_PRESENT_MODE_FIFO_KHR", pure VK_PRESENT_MODE_FIFO_KHR)
                             , ("VK_PRESENT_MODE_FIFO_RELAXED_KHR", pure VK_PRESENT_MODE_FIFO_RELAXED_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPresentModeKHR")
                        v <- step readPrec
                        pure (VkPresentModeKHR v)
                        )
                    )


pattern VK_PRESENT_MODE_IMMEDIATE_KHR = VkPresentModeKHR 0

pattern VK_PRESENT_MODE_MAILBOX_KHR = VkPresentModeKHR 1

pattern VK_PRESENT_MODE_FIFO_KHR = VkPresentModeKHR 2

pattern VK_PRESENT_MODE_FIFO_RELAXED_KHR = VkPresentModeKHR 3

newtype VkSurfaceKHR = VkSurfaceKHR Word64
  deriving (Eq, Storable)

-- ** VkColorSpaceKHR

newtype VkColorSpaceKHR = VkColorSpaceKHR Int32
  deriving (Eq, Storable)

instance Show VkColorSpaceKHR where
  showsPrec _ VK_COLORSPACE_SRGB_NONLINEAR_KHR = showString "VK_COLORSPACE_SRGB_NONLINEAR_KHR"
  showsPrec p (VkColorSpaceKHR x) = showParen (p >= 11) (showString "VkColorSpaceKHR " . showsPrec 11 x)

instance Read VkColorSpaceKHR where
  readPrec = parens ( choose [ ("VK_COLORSPACE_SRGB_NONLINEAR_KHR", pure VK_COLORSPACE_SRGB_NONLINEAR_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkColorSpaceKHR")
                        v <- step readPrec
                        pure (VkColorSpaceKHR v)
                        )
                    )


pattern VK_COLORSPACE_SRGB_NONLINEAR_KHR = VkColorSpaceKHR 0

-- ** VkSurfaceTransformFlagsKHR

newtype VkSurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkSurfaceTransformFlagBitsKHR
type VkSurfaceTransformFlagsKHR = VkSurfaceTransformFlagBitsKHR

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
                        expectP (Ident "VkSurfaceTransformFlagBitsKHR")
                        v <- step readPrec
                        pure (VkSurfaceTransformFlagBitsKHR v)
                        )
                    )


pattern VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x1

pattern VK_SURFACE_TRANSFORM_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x2

pattern VK_SURFACE_TRANSFORM_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x4

pattern VK_SURFACE_TRANSFORM_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x8

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x10

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_90_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x20

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_180_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x40

pattern VK_SURFACE_TRANSFORM_HORIZONTAL_MIRROR_ROTATE_270_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x80

pattern VK_SURFACE_TRANSFORM_INHERIT_BIT_KHR = VkSurfaceTransformFlagBitsKHR 0x100


