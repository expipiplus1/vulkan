{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkCommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , VkConditionalRenderingBeginInfoEXT(..)
  , VkConditionalRenderingFlagBitsEXT(..)
  , pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT
  , VkConditionalRenderingFlagsEXT
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , FN_vkCmdBeginConditionalRenderingEXT
  , PFN_vkCmdBeginConditionalRenderingEXT
  , vkCmdBeginConditionalRenderingEXT
  , FN_vkCmdEndConditionalRenderingEXT
  , PFN_vkCmdEndConditionalRenderingEXT
  , vkCmdEndConditionalRenderingEXT
  , pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
  , pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME
  , pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION
  , pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferUsageFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAccessFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkCommandBufferInheritanceConditionalRenderingInfoEXT"
data VkCommandBufferInheritanceConditionalRenderingInfoEXT = VkCommandBufferInheritanceConditionalRenderingInfoEXT
  { -- No documentation found for Nested "VkCommandBufferInheritanceConditionalRenderingInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCommandBufferInheritanceConditionalRenderingInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCommandBufferInheritanceConditionalRenderingInfoEXT" "conditionalRenderingEnable"
  vkConditionalRenderingEnable :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkCommandBufferInheritanceConditionalRenderingInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkCommandBufferInheritanceConditionalRenderingInfoEXT <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkConditionalRenderingEnable (poked :: VkCommandBufferInheritanceConditionalRenderingInfoEXT))

instance Zero VkCommandBufferInheritanceConditionalRenderingInfoEXT where
  zero = VkCommandBufferInheritanceConditionalRenderingInfoEXT VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
                                                               zero
                                                               zero

-- No documentation found for TopLevel "VkConditionalRenderingBeginInfoEXT"
data VkConditionalRenderingBeginInfoEXT = VkConditionalRenderingBeginInfoEXT
  { -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "offset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkConditionalRenderingBeginInfoEXT" "flags"
  vkFlags :: VkConditionalRenderingFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkConditionalRenderingBeginInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkConditionalRenderingBeginInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkOffset (poked :: VkConditionalRenderingBeginInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkFlags (poked :: VkConditionalRenderingBeginInfoEXT))

instance Zero VkConditionalRenderingBeginInfoEXT where
  zero = VkConditionalRenderingBeginInfoEXT VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
                                            zero
                                            zero
                                            zero
                                            zero

-- ** VkConditionalRenderingFlagBitsEXT

-- No documentation found for TopLevel "VkConditionalRenderingFlagBitsEXT"
newtype VkConditionalRenderingFlagBitsEXT = VkConditionalRenderingFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkConditionalRenderingFlagBitsEXT where
  showsPrec _ VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT = showString "VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT"
  showsPrec p (VkConditionalRenderingFlagBitsEXT x) = showParen (p >= 11) (showString "VkConditionalRenderingFlagBitsEXT " . showsPrec 11 x)

instance Read VkConditionalRenderingFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT", pure VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkConditionalRenderingFlagBitsEXT")
                        v <- step readPrec
                        pure (VkConditionalRenderingFlagBitsEXT v)
                        )
                    )

-- No documentation found for Nested "VkConditionalRenderingFlagBitsEXT" "VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT"
pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT :: VkConditionalRenderingFlagBitsEXT
pattern VK_CONDITIONAL_RENDERING_INVERTED_BIT_EXT = VkConditionalRenderingFlagBitsEXT 0x00000001

-- No documentation found for TopLevel "VkConditionalRenderingFlagsEXT"
type VkConditionalRenderingFlagsEXT = VkConditionalRenderingFlagBitsEXT

-- No documentation found for TopLevel "VkPhysicalDeviceConditionalRenderingFeaturesEXT"
data VkPhysicalDeviceConditionalRenderingFeaturesEXT = VkPhysicalDeviceConditionalRenderingFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceConditionalRenderingFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceConditionalRenderingFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceConditionalRenderingFeaturesEXT" "conditionalRendering"
  vkConditionalRendering :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceConditionalRenderingFeaturesEXT" "inheritedConditionalRendering"
  vkInheritedConditionalRendering :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceConditionalRenderingFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceConditionalRenderingFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkConditionalRendering (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkInheritedConditionalRendering (poked :: VkPhysicalDeviceConditionalRenderingFeaturesEXT))

instance Zero VkPhysicalDeviceConditionalRenderingFeaturesEXT where
  zero = VkPhysicalDeviceConditionalRenderingFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
                                                         zero
                                                         zero
                                                         zero

-- No documentation found for TopLevel "vkCmdBeginConditionalRenderingEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginConditionalRenderingEXT" vkCmdBeginConditionalRenderingEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()
#else
vkCmdBeginConditionalRenderingEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()
vkCmdBeginConditionalRenderingEXT deviceCmds = mkVkCmdBeginConditionalRenderingEXT (pVkCmdBeginConditionalRenderingEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginConditionalRenderingEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ())
#endif

type FN_vkCmdBeginConditionalRenderingEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()
type PFN_vkCmdBeginConditionalRenderingEXT = FunPtr FN_vkCmdBeginConditionalRenderingEXT

-- No documentation found for TopLevel "vkCmdEndConditionalRenderingEXT"
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndConditionalRenderingEXT" vkCmdEndConditionalRenderingEXT :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
#else
vkCmdEndConditionalRenderingEXT :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> IO ()
vkCmdEndConditionalRenderingEXT deviceCmds = mkVkCmdEndConditionalRenderingEXT (pVkCmdEndConditionalRenderingEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndConditionalRenderingEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
#endif

type FN_vkCmdEndConditionalRenderingEXT = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdEndConditionalRenderingEXT = FunPtr FN_vkCmdEndConditionalRenderingEXT

-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT"
pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT = VkAccessFlagBits 0x00100000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT = VkBufferUsageFlagBits 0x00000200

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME"
pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_CONDITIONAL_RENDERING_EXTENSION_NAME = "VK_EXT_conditional_rendering"

-- No documentation found for TopLevel "VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION"
pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION :: Integral a => a
pattern VK_EXT_CONDITIONAL_RENDERING_SPEC_VERSION = 1

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT = VkPipelineStageFlagBits 0x00040000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT = VkStructureType 1000081000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT"
pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT = VkStructureType 1000081002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT = VkStructureType 1000081001
