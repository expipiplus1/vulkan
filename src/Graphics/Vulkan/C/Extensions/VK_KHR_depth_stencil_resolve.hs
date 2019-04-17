{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( VkPhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , VkResolveModeFlagBitsKHR(..)
  , pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR
  , pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR
  , pattern VK_RESOLVE_MODE_MIN_BIT_KHR
  , pattern VK_RESOLVE_MODE_MAX_BIT_KHR
  , pattern VK_RESOLVE_MODE_NONE_KHR
  , VkResolveModeFlagsKHR
  , VkSubpassDescriptionDepthStencilResolveKHR(..)
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
  , pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
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
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkAttachmentReference2KHR(..)
  )


-- | VkPhysicalDeviceDepthStencilResolvePropertiesKHR - Structure describing
-- depth\/stencil resolve properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceDepthStencilResolvePropertiesKHR@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- Unresolved directive in
-- VkPhysicalDeviceDepthStencilResolvePropertiesKHR.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceDepthStencilResolvePropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceDepthStencilResolvePropertiesKHR = VkPhysicalDeviceDepthStencilResolvePropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolvePropertiesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDepthStencilResolvePropertiesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- | @supportedDepthResolveModes@ is a bitmask of 'VkResolveModeFlagBitsKHR'
  -- indicating the set of supported depth resolve modes.
  -- @VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR@ /must/ be included in the set but
  -- implementations /may/ support additional modes.
  vkSupportedDepthResolveModes :: VkResolveModeFlagsKHR
  , -- | @supportedStencilResolveModes@ is a bitmask of
  -- 'VkResolveModeFlagBitsKHR' indicating the set of supported stencil
  -- resolve modes. @VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR@ /must/ be included
  -- in the set but implementations /may/ support additional modes.
  -- @VK_RESOLVE_MODE_AVERAGE_BIT_KHR@ /must/ not be included in the set.
  vkSupportedStencilResolveModes :: VkResolveModeFlagsKHR
  , -- | @independentResolveNone@ is @VK_TRUE@ if the implementation supports
  -- setting the depth and stencil resolve modes to different values when one
  -- of those modes is @VK_RESOLVE_MODE_NONE_KHR@. Otherwise the
  -- implementation only supports setting both modes to the same value.
  vkIndependentResolveNone :: VkBool32
  , -- | @independentResolve@ is @VK_TRUE@ if the implementation supports all
  -- combinations of the supported depth and stencil resolve modes.
  vkIndependentResolve :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDepthStencilResolvePropertiesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDepthStencilResolvePropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                              <*> peek (ptr `plusPtr` 8)
                                                              <*> peek (ptr `plusPtr` 16)
                                                              <*> peek (ptr `plusPtr` 20)
                                                              <*> peek (ptr `plusPtr` 24)
                                                              <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkSupportedDepthResolveModes (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 20) (vkSupportedStencilResolveModes (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 24) (vkIndependentResolveNone (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
                *> poke (ptr `plusPtr` 28) (vkIndependentResolve (poked :: VkPhysicalDeviceDepthStencilResolvePropertiesKHR))

instance Zero VkPhysicalDeviceDepthStencilResolvePropertiesKHR where
  zero = VkPhysicalDeviceDepthStencilResolvePropertiesKHR zero
                                                          zero
                                                          zero
                                                          zero
                                                          zero
                                                          zero
-- ** VkResolveModeFlagBitsKHR

-- | VkResolveModeFlagBitsKHR - Bitmask indicating supported depth and
-- stencil resolve modes
--
-- = See Also
--
-- No cross-references are available
newtype VkResolveModeFlagBitsKHR = VkResolveModeFlagBitsKHR VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkResolveModeFlagBitsKHR where
  showsPrec _ VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR = showString "VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR"
  showsPrec _ VK_RESOLVE_MODE_AVERAGE_BIT_KHR = showString "VK_RESOLVE_MODE_AVERAGE_BIT_KHR"
  showsPrec _ VK_RESOLVE_MODE_MIN_BIT_KHR = showString "VK_RESOLVE_MODE_MIN_BIT_KHR"
  showsPrec _ VK_RESOLVE_MODE_MAX_BIT_KHR = showString "VK_RESOLVE_MODE_MAX_BIT_KHR"
  showsPrec _ VK_RESOLVE_MODE_NONE_KHR = showString "VK_RESOLVE_MODE_NONE_KHR"
  showsPrec p (VkResolveModeFlagBitsKHR x) = showParen (p >= 11) (showString "VkResolveModeFlagBitsKHR " . showsPrec 11 x)

instance Read VkResolveModeFlagBitsKHR where
  readPrec = parens ( choose [ ("VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR", pure VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR)
                             , ("VK_RESOLVE_MODE_AVERAGE_BIT_KHR",     pure VK_RESOLVE_MODE_AVERAGE_BIT_KHR)
                             , ("VK_RESOLVE_MODE_MIN_BIT_KHR",         pure VK_RESOLVE_MODE_MIN_BIT_KHR)
                             , ("VK_RESOLVE_MODE_MAX_BIT_KHR",         pure VK_RESOLVE_MODE_MAX_BIT_KHR)
                             , ("VK_RESOLVE_MODE_NONE_KHR",            pure VK_RESOLVE_MODE_NONE_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkResolveModeFlagBitsKHR")
                        v <- step readPrec
                        pure (VkResolveModeFlagBitsKHR v)
                        )
                    )

-- | @VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR@ indicates that result of the
-- resolve operation is equal to the value of sample 0.
pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR = VkResolveModeFlagBitsKHR 0x00000001

-- | @VK_RESOLVE_MODE_AVERAGE_BIT_KHR@ indicates that result of the resolve
-- operation is the average of the sample values.
pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_AVERAGE_BIT_KHR = VkResolveModeFlagBitsKHR 0x00000002

-- | @VK_RESOLVE_MODE_MIN_BIT_KHR@ indicates that result of the resolve
-- operation is the minimum of the sample values.
pattern VK_RESOLVE_MODE_MIN_BIT_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_MIN_BIT_KHR = VkResolveModeFlagBitsKHR 0x00000004

-- | @VK_RESOLVE_MODE_MAX_BIT_KHR@ indicates that result of the resolve
-- operation is the maximum of the sample values.
pattern VK_RESOLVE_MODE_MAX_BIT_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_MAX_BIT_KHR = VkResolveModeFlagBitsKHR 0x00000008

-- | @VK_RESOLVE_MODE_NONE_KHR@ indicates that no resolve operation is done.
pattern VK_RESOLVE_MODE_NONE_KHR :: VkResolveModeFlagBitsKHR
pattern VK_RESOLVE_MODE_NONE_KHR = VkResolveModeFlagBitsKHR 0x00000000
-- | VkResolveModeFlagsKHR - Bitmask of VkResolveModeFlagBitsKHR
--
-- = Description
--
-- @VkResolveModeFlagsKHR@ is a bitmask type for setting a mask of zero or
-- more 'VkResolveModeFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type VkResolveModeFlagsKHR = VkResolveModeFlagBitsKHR
-- | VkSubpassDescriptionDepthStencilResolveKHR - Structure specifying
-- depth\/stencil resolve operations for a subpass
--
-- == Valid Usage
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value @VK_ATTACHMENT_UNUSED@, @pDepthStencilAttachment@ /must/
--     not have the value @VK_ATTACHMENT_UNUSED@
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value @VK_ATTACHMENT_UNUSED@, @depthResolveMode@ and
--     @stencilResolveMode@ /must/ not both be @VK_RESOLVE_MODE_NONE_KHR@
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value @VK_ATTACHMENT_UNUSED@, @pDepthStencilAttachment@ /must/
--     not have a sample count of @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value @VK_ATTACHMENT_UNUSED@, @pDepthStencilResolveAttachment@
--     /must/ have a sample count of @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If @pDepthStencilResolveAttachment@ is not @NULL@ and does not have
--     the value @VK_ATTACHMENT_UNUSED@ then it /must/ have a format whose
--     features contain @VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilResolveAttachment@ has a depth component, then the
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilAttachment@ /must/ have a depth component with the
--     same number of bits and numerical type
--
-- -   If the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilResolveAttachment@ has a stencil component, then the
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilAttachment@ /must/ have a stencil component with the
--     same number of bits and numerical type
--
-- -   The value of @depthResolveMode@ /must/ be one of the bits set in
--     'VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@supportedDepthResolveModes@
--     or @VK_RESOLVE_MODE_NONE_KHR@
--
-- -   The value of @stencilResolveMode@ /must/ be one of the bits set in
--     'VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@supportedStencilResolveModes@
--     or @VK_RESOLVE_MODE_NONE_KHR@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilResolveAttachment@ has both depth and stencil
--     components,
--     'VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@independentResolve@
--     is @VK_FALSE@, and
--     'VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@independentResolveNone@
--     is @VK_FALSE@, then the values of @depthResolveMode@ and
--     @stencilResolveMode@ /must/ be identical
--
-- -   If the 'Graphics.Vulkan.C.Core10.Core.VkFormat' of
--     @pDepthStencilResolveAttachment@ has both depth and stencil
--     components,
--     'VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@independentResolve@
--     is @VK_FALSE@ and
--     'VkPhysicalDeviceDepthStencilResolvePropertiesKHR'::@independentResolveNone@
--     is @VK_TRUE@, then the values of @depthResolveMode@ and
--     @stencilResolveMode@ /must/ be identical or one of them /must/ be
--     @VK_RESOLVE_MODE_NONE_KHR@
--
-- Unresolved directive in VkSubpassDescriptionDepthStencilResolveKHR.txt -
-- include::..\/validity\/structs\/VkSubpassDescriptionDepthStencilResolveKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkSubpassDescriptionDepthStencilResolveKHR = VkSubpassDescriptionDepthStencilResolveKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @depthResolveMode@ is a bitmask of 'VkResolveModeFlagBitsKHR' describing
  -- the depth resolve mode.
  vkDepthResolveMode :: VkResolveModeFlagBitsKHR
  , -- | @stencilResolveMode@ is a bitmask of 'VkResolveModeFlagBitsKHR'
  -- describing the stencil resolve mode.
  vkStencilResolveMode :: VkResolveModeFlagBitsKHR
  , -- | @pDepthStencilResolveAttachment@ is an optional
  -- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structure defining
  -- the depth\/stencil resolve attachment for this subpass and its layout.
  vkPDepthStencilResolveAttachment :: Ptr VkAttachmentReference2KHR
  }
  deriving (Eq, Show)

instance Storable VkSubpassDescriptionDepthStencilResolveKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkSubpassDescriptionDepthStencilResolveKHR <$> peek (ptr `plusPtr` 0)
                                                        <*> peek (ptr `plusPtr` 8)
                                                        <*> peek (ptr `plusPtr` 16)
                                                        <*> peek (ptr `plusPtr` 20)
                                                        <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
                *> poke (ptr `plusPtr` 16) (vkDepthResolveMode (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
                *> poke (ptr `plusPtr` 20) (vkStencilResolveMode (poked :: VkSubpassDescriptionDepthStencilResolveKHR))
                *> poke (ptr `plusPtr` 24) (vkPDepthStencilResolveAttachment (poked :: VkSubpassDescriptionDepthStencilResolveKHR))

instance Zero VkSubpassDescriptionDepthStencilResolveKHR where
  zero = VkSubpassDescriptionDepthStencilResolveKHR zero
                                                    zero
                                                    zero
                                                    zero
                                                    zero
-- No documentation found for TopLevel "VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME"
pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME = "VK_KHR_depth_stencil_resolve"
-- No documentation found for TopLevel "VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION"
pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR = VkStructureType 1000199000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR"
pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR = VkStructureType 1000199001
