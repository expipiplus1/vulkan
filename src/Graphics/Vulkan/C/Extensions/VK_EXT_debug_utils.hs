{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( PFN_vkDebugUtilsMessengerCallbackEXT
  , VkDebugUtilsLabelEXT(..)
  , VkDebugUtilsMessageSeverityFlagBitsEXT(..)
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
  , VkDebugUtilsMessageSeverityFlagsEXT
  , VkDebugUtilsMessageTypeFlagBitsEXT(..)
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
  , pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
  , VkDebugUtilsMessageTypeFlagsEXT
  , VkDebugUtilsMessengerCallbackDataEXT(..)
  , VkDebugUtilsMessengerCallbackDataFlagsEXT(..)
  , VkDebugUtilsMessengerCreateFlagsEXT(..)
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsMessengerEXT
  , VkDebugUtilsObjectNameInfoEXT(..)
  , VkDebugUtilsObjectTagInfoEXT(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdBeginDebugUtilsLabelEXT
#endif
  , FN_vkCmdBeginDebugUtilsLabelEXT
  , PFN_vkCmdBeginDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdEndDebugUtilsLabelEXT
#endif
  , FN_vkCmdEndDebugUtilsLabelEXT
  , PFN_vkCmdEndDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCmdInsertDebugUtilsLabelEXT
#endif
  , FN_vkCmdInsertDebugUtilsLabelEXT
  , PFN_vkCmdInsertDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkCreateDebugUtilsMessengerEXT
#endif
  , FN_vkCreateDebugUtilsMessengerEXT
  , PFN_vkCreateDebugUtilsMessengerEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkDestroyDebugUtilsMessengerEXT
#endif
  , FN_vkDestroyDebugUtilsMessengerEXT
  , PFN_vkDestroyDebugUtilsMessengerEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkQueueBeginDebugUtilsLabelEXT
#endif
  , FN_vkQueueBeginDebugUtilsLabelEXT
  , PFN_vkQueueBeginDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkQueueEndDebugUtilsLabelEXT
#endif
  , FN_vkQueueEndDebugUtilsLabelEXT
  , PFN_vkQueueEndDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkQueueInsertDebugUtilsLabelEXT
#endif
  , FN_vkQueueInsertDebugUtilsLabelEXT
  , PFN_vkQueueInsertDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkSetDebugUtilsObjectNameEXT
#endif
  , FN_vkSetDebugUtilsObjectNameEXT
  , PFN_vkSetDebugUtilsObjectNameEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkSetDebugUtilsObjectTagEXT
#endif
  , FN_vkSetDebugUtilsObjectTagEXT
  , PFN_vkSetDebugUtilsObjectTagEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkSubmitDebugUtilsMessageEXT
#endif
  , FN_vkSubmitDebugUtilsMessageEXT
  , PFN_vkSubmitDebugUtilsMessageEXT
  , pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME
  , pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
  , CSize(..)
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
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkInstance
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  , VkQueue
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "PFN_vkDebugUtilsMessengerCallbackEXT"
type PFN_vkDebugUtilsMessengerCallbackEXT = Ptr (("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageType" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO VkBool32)
-- No documentation found for TopLevel "VkDebugUtilsLabelEXT"
data VkDebugUtilsLabelEXT = VkDebugUtilsLabelEXT
  { -- No documentation found for Nested "VkDebugUtilsLabelEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugUtilsLabelEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugUtilsLabelEXT" "pLabelName"
  vkPLabelName :: Ptr CChar
  , -- No documentation found for Nested "VkDebugUtilsLabelEXT" "color"
  vkColor :: Vector 4 CFloat
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsLabelEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugUtilsLabelEXT <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsLabelEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsLabelEXT))
                *> poke (ptr `plusPtr` 16) (vkPLabelName (poked :: VkDebugUtilsLabelEXT))
                *> poke (ptr `plusPtr` 24) (vkColor (poked :: VkDebugUtilsLabelEXT))
-- ** VkDebugUtilsMessageSeverityFlagBitsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessageSeverityFlagBitsEXT"
newtype VkDebugUtilsMessageSeverityFlagBitsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDebugUtilsMessageSeverityFlagBitsEXT where
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
  showsPrec p (VkDebugUtilsMessageSeverityFlagBitsEXT x) = showParen (p >= 11) (showString "VkDebugUtilsMessageSeverityFlagBitsEXT " . showsPrec 11 x)

instance Read VkDebugUtilsMessageSeverityFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT", pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT",    pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT", pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT",   pure VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugUtilsMessageSeverityFlagBitsEXT")
                        v <- step readPrec
                        pure (VkDebugUtilsMessageSeverityFlagBitsEXT v)
                        )
                    )

-- No documentation found for Nested "VkDebugUtilsMessageSeverityFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT :: VkDebugUtilsMessageSeverityFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = VkDebugUtilsMessageSeverityFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkDebugUtilsMessageSeverityFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT :: VkDebugUtilsMessageSeverityFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = VkDebugUtilsMessageSeverityFlagBitsEXT 0x00000010

-- No documentation found for Nested "VkDebugUtilsMessageSeverityFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT :: VkDebugUtilsMessageSeverityFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = VkDebugUtilsMessageSeverityFlagBitsEXT 0x00000100

-- No documentation found for Nested "VkDebugUtilsMessageSeverityFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT :: VkDebugUtilsMessageSeverityFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = VkDebugUtilsMessageSeverityFlagBitsEXT 0x00001000
-- No documentation found for TopLevel "VkDebugUtilsMessageSeverityFlagsEXT"
type VkDebugUtilsMessageSeverityFlagsEXT = VkDebugUtilsMessageSeverityFlagBitsEXT
-- ** VkDebugUtilsMessageTypeFlagBitsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessageTypeFlagBitsEXT"
newtype VkDebugUtilsMessageTypeFlagBitsEXT = VkDebugUtilsMessageTypeFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDebugUtilsMessageTypeFlagBitsEXT where
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
  showsPrec _ VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = showString "VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
  showsPrec p (VkDebugUtilsMessageTypeFlagBitsEXT x) = showParen (p >= 11) (showString "VkDebugUtilsMessageTypeFlagBitsEXT " . showsPrec 11 x)

instance Read VkDebugUtilsMessageTypeFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT",     pure VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT",  pure VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT)
                             , ("VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT", pure VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugUtilsMessageTypeFlagBitsEXT")
                        v <- step readPrec
                        pure (VkDebugUtilsMessageTypeFlagBitsEXT v)
                        )
                    )

-- No documentation found for Nested "VkDebugUtilsMessageTypeFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT :: VkDebugUtilsMessageTypeFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = VkDebugUtilsMessageTypeFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkDebugUtilsMessageTypeFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT :: VkDebugUtilsMessageTypeFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = VkDebugUtilsMessageTypeFlagBitsEXT 0x00000002

-- No documentation found for Nested "VkDebugUtilsMessageTypeFlagBitsEXT" "VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT :: VkDebugUtilsMessageTypeFlagBitsEXT
pattern VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = VkDebugUtilsMessageTypeFlagBitsEXT 0x00000004
-- No documentation found for TopLevel "VkDebugUtilsMessageTypeFlagsEXT"
type VkDebugUtilsMessageTypeFlagsEXT = VkDebugUtilsMessageTypeFlagBitsEXT
-- No documentation found for TopLevel "VkDebugUtilsMessengerCallbackDataEXT"
data VkDebugUtilsMessengerCallbackDataEXT = VkDebugUtilsMessengerCallbackDataEXT
  { -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "flags"
  vkFlags :: VkDebugUtilsMessengerCallbackDataFlagsEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pMessageIdName"
  vkPMessageIdName :: Ptr CChar
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "messageIdNumber"
  vkMessageIdNumber :: Int32
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pMessage"
  vkPMessage :: Ptr CChar
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "queueLabelCount"
  vkQueueLabelCount :: Word32
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pQueueLabels"
  vkPQueueLabels :: Ptr VkDebugUtilsLabelEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "cmdBufLabelCount"
  vkCmdBufLabelCount :: Word32
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pCmdBufLabels"
  vkPCmdBufLabels :: Ptr VkDebugUtilsLabelEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "objectCount"
  vkObjectCount :: Word32
  , -- No documentation found for Nested "VkDebugUtilsMessengerCallbackDataEXT" "pObjects"
  vkPObjects :: Ptr VkDebugUtilsObjectNameInfoEXT
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsMessengerCallbackDataEXT where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkDebugUtilsMessengerCallbackDataEXT <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 32)
                                                  <*> peek (ptr `plusPtr` 40)
                                                  <*> peek (ptr `plusPtr` 48)
                                                  <*> peek (ptr `plusPtr` 56)
                                                  <*> peek (ptr `plusPtr` 64)
                                                  <*> peek (ptr `plusPtr` 72)
                                                  <*> peek (ptr `plusPtr` 80)
                                                  <*> peek (ptr `plusPtr` 88)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 24) (vkPMessageIdName (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 32) (vkMessageIdNumber (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 40) (vkPMessage (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 48) (vkQueueLabelCount (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 56) (vkPQueueLabels (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 64) (vkCmdBufLabelCount (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 72) (vkPCmdBufLabels (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 80) (vkObjectCount (poked :: VkDebugUtilsMessengerCallbackDataEXT))
                *> poke (ptr `plusPtr` 88) (vkPObjects (poked :: VkDebugUtilsMessengerCallbackDataEXT))
-- ** VkDebugUtilsMessengerCallbackDataFlagsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessengerCallbackDataFlagsEXT"
newtype VkDebugUtilsMessengerCallbackDataFlagsEXT = VkDebugUtilsMessengerCallbackDataFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDebugUtilsMessengerCallbackDataFlagsEXT where
  
  showsPrec p (VkDebugUtilsMessengerCallbackDataFlagsEXT x) = showParen (p >= 11) (showString "VkDebugUtilsMessengerCallbackDataFlagsEXT " . showsPrec 11 x)

instance Read VkDebugUtilsMessengerCallbackDataFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugUtilsMessengerCallbackDataFlagsEXT")
                        v <- step readPrec
                        pure (VkDebugUtilsMessengerCallbackDataFlagsEXT v)
                        )
                    )


-- ** VkDebugUtilsMessengerCreateFlagsEXT

-- No documentation found for TopLevel "VkDebugUtilsMessengerCreateFlagsEXT"
newtype VkDebugUtilsMessengerCreateFlagsEXT = VkDebugUtilsMessengerCreateFlagsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDebugUtilsMessengerCreateFlagsEXT where
  
  showsPrec p (VkDebugUtilsMessengerCreateFlagsEXT x) = showParen (p >= 11) (showString "VkDebugUtilsMessengerCreateFlagsEXT " . showsPrec 11 x)

instance Read VkDebugUtilsMessengerCreateFlagsEXT where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDebugUtilsMessengerCreateFlagsEXT")
                        v <- step readPrec
                        pure (VkDebugUtilsMessengerCreateFlagsEXT v)
                        )
                    )


-- No documentation found for TopLevel "VkDebugUtilsMessengerCreateInfoEXT"
data VkDebugUtilsMessengerCreateInfoEXT = VkDebugUtilsMessengerCreateInfoEXT
  { -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "flags"
  vkFlags :: VkDebugUtilsMessengerCreateFlagsEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "messageSeverity"
  vkMessageSeverity :: VkDebugUtilsMessageSeverityFlagsEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "messageType"
  vkMessageType :: VkDebugUtilsMessageTypeFlagsEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "pfnUserCallback"
  vkPfnUserCallback :: PFN_vkDebugUtilsMessengerCallbackEXT
  , -- No documentation found for Nested "VkDebugUtilsMessengerCreateInfoEXT" "pUserData"
  vkPUserData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsMessengerCreateInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkDebugUtilsMessengerCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                <*> peek (ptr `plusPtr` 8)
                                                <*> peek (ptr `plusPtr` 16)
                                                <*> peek (ptr `plusPtr` 20)
                                                <*> peek (ptr `plusPtr` 24)
                                                <*> peek (ptr `plusPtr` 32)
                                                <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkMessageSeverity (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkMessageType (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPfnUserCallback (poked :: VkDebugUtilsMessengerCreateInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkPUserData (poked :: VkDebugUtilsMessengerCreateInfoEXT))
-- | Dummy data to tag the 'Ptr' with
data VkDebugUtilsMessengerEXT_T
-- No documentation found for TopLevel "VkDebugUtilsMessengerEXT"
type VkDebugUtilsMessengerEXT = Ptr VkDebugUtilsMessengerEXT_T
-- No documentation found for TopLevel "VkDebugUtilsObjectNameInfoEXT"
data VkDebugUtilsObjectNameInfoEXT = VkDebugUtilsObjectNameInfoEXT
  { -- No documentation found for Nested "VkDebugUtilsObjectNameInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugUtilsObjectNameInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugUtilsObjectNameInfoEXT" "objectType"
  vkObjectType :: VkObjectType
  , -- No documentation found for Nested "VkDebugUtilsObjectNameInfoEXT" "objectHandle"
  vkObjectHandle :: Word64
  , -- No documentation found for Nested "VkDebugUtilsObjectNameInfoEXT" "pObjectName"
  vkPObjectName :: Ptr CChar
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsObjectNameInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDebugUtilsObjectNameInfoEXT <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
                                           <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkObjectType (poked :: VkDebugUtilsObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkObjectHandle (poked :: VkDebugUtilsObjectNameInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPObjectName (poked :: VkDebugUtilsObjectNameInfoEXT))
-- No documentation found for TopLevel "VkDebugUtilsObjectTagInfoEXT"
data VkDebugUtilsObjectTagInfoEXT = VkDebugUtilsObjectTagInfoEXT
  { -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "objectType"
  vkObjectType :: VkObjectType
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "objectHandle"
  vkObjectHandle :: Word64
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "tagName"
  vkTagName :: Word64
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "tagSize"
  vkTagSize :: CSize
  , -- No documentation found for Nested "VkDebugUtilsObjectTagInfoEXT" "pTag"
  vkPTag :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkDebugUtilsObjectTagInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDebugUtilsObjectTagInfoEXT <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 16)
                                          <*> peek (ptr `plusPtr` 24)
                                          <*> peek (ptr `plusPtr` 32)
                                          <*> peek (ptr `plusPtr` 40)
                                          <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkObjectType (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkObjectHandle (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkTagName (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 40) (vkTagSize (poked :: VkDebugUtilsObjectTagInfoEXT))
                *> poke (ptr `plusPtr` 48) (vkPTag (poked :: VkDebugUtilsObjectTagInfoEXT))
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdBeginDebugUtilsLabelEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginDebugUtilsLabelEXT" vkCmdBeginDebugUtilsLabelEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()

#endif
type FN_vkCmdBeginDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkCmdBeginDebugUtilsLabelEXT = FunPtr FN_vkCmdBeginDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdEndDebugUtilsLabelEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndDebugUtilsLabelEXT" vkCmdEndDebugUtilsLabelEXT :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()

#endif
type FN_vkCmdEndDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> IO ()
type PFN_vkCmdEndDebugUtilsLabelEXT = FunPtr FN_vkCmdEndDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCmdInsertDebugUtilsLabelEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdInsertDebugUtilsLabelEXT" vkCmdInsertDebugUtilsLabelEXT :: ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()

#endif
type FN_vkCmdInsertDebugUtilsLabelEXT = ("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkCmdInsertDebugUtilsLabelEXT = FunPtr FN_vkCmdInsertDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkCreateDebugUtilsMessengerEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDebugUtilsMessengerEXT" vkCreateDebugUtilsMessengerEXT :: ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult

#endif
type FN_vkCreateDebugUtilsMessengerEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult
type PFN_vkCreateDebugUtilsMessengerEXT = FunPtr FN_vkCreateDebugUtilsMessengerEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkDestroyDebugUtilsMessengerEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDebugUtilsMessengerEXT" vkDestroyDebugUtilsMessengerEXT :: ("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyDebugUtilsMessengerEXT = ("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDebugUtilsMessengerEXT = FunPtr FN_vkDestroyDebugUtilsMessengerEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkQueueBeginDebugUtilsLabelEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueBeginDebugUtilsLabelEXT" vkQueueBeginDebugUtilsLabelEXT :: ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()

#endif
type FN_vkQueueBeginDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkQueueBeginDebugUtilsLabelEXT = FunPtr FN_vkQueueBeginDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkQueueEndDebugUtilsLabelEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueEndDebugUtilsLabelEXT" vkQueueEndDebugUtilsLabelEXT :: ("queue" ::: VkQueue) -> IO ()

#endif
type FN_vkQueueEndDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> IO ()
type PFN_vkQueueEndDebugUtilsLabelEXT = FunPtr FN_vkQueueEndDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkQueueInsertDebugUtilsLabelEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkQueueInsertDebugUtilsLabelEXT" vkQueueInsertDebugUtilsLabelEXT :: ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()

#endif
type FN_vkQueueInsertDebugUtilsLabelEXT = ("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()
type PFN_vkQueueInsertDebugUtilsLabelEXT = FunPtr FN_vkQueueInsertDebugUtilsLabelEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkSetDebugUtilsObjectNameEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetDebugUtilsObjectNameEXT" vkSetDebugUtilsObjectNameEXT :: ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult

#endif
type FN_vkSetDebugUtilsObjectNameEXT = ("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult
type PFN_vkSetDebugUtilsObjectNameEXT = FunPtr FN_vkSetDebugUtilsObjectNameEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkSetDebugUtilsObjectTagEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetDebugUtilsObjectTagEXT" vkSetDebugUtilsObjectTagEXT :: ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult

#endif
type FN_vkSetDebugUtilsObjectTagEXT = ("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult
type PFN_vkSetDebugUtilsObjectTagEXT = FunPtr FN_vkSetDebugUtilsObjectTagEXT
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkSubmitDebugUtilsMessageEXT"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSubmitDebugUtilsMessageEXT" vkSubmitDebugUtilsMessageEXT :: ("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ()

#endif
type FN_vkSubmitDebugUtilsMessageEXT = ("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ()
type PFN_vkSubmitDebugUtilsMessageEXT = FunPtr FN_vkSubmitDebugUtilsMessageEXT
-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"
-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_SPEC_VERSION"
pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEBUG_UTILS_SPEC_VERSION = 1
-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT"
pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT :: VkObjectType
pattern VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT = VkObjectType 1000128000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT = VkStructureType 1000128002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT = VkStructureType 1000128003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT = VkStructureType 1000128004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT = VkStructureType 1000128000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT = VkStructureType 1000128001
