{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( VkDeviceQueueGlobalPriorityCreateInfoEXT(..)
  , VkQueueGlobalPriorityEXT(..)
  , pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT
  , pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT
  , pattern VK_ERROR_NOT_PERMITTED_EXT
  , pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME
  , pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  ) where

import Data.Int
  ( Int32
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
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- No documentation found for TopLevel "VkDeviceQueueGlobalPriorityCreateInfoEXT"
data VkDeviceQueueGlobalPriorityCreateInfoEXT = VkDeviceQueueGlobalPriorityCreateInfoEXT
  { -- No documentation found for Nested "VkDeviceQueueGlobalPriorityCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceQueueGlobalPriorityCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceQueueGlobalPriorityCreateInfoEXT" "globalPriority"
  vkGlobalPriority :: VkQueueGlobalPriorityEXT
  }
  deriving (Eq, Show)

instance Storable VkDeviceQueueGlobalPriorityCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceQueueGlobalPriorityCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                      <*> peek (ptr `plusPtr` 8)
                                                      <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceQueueGlobalPriorityCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceQueueGlobalPriorityCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkGlobalPriority (poked :: VkDeviceQueueGlobalPriorityCreateInfoEXT))

instance Zero VkDeviceQueueGlobalPriorityCreateInfoEXT where
  zero = VkDeviceQueueGlobalPriorityCreateInfoEXT zero
                                                  zero
                                                  zero
-- ** VkQueueGlobalPriorityEXT

-- No documentation found for TopLevel "VkQueueGlobalPriorityEXT"
newtype VkQueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkQueueGlobalPriorityEXT where
  showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT = showString "VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT"
  showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT = showString "VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT"
  showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT = showString "VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT"
  showsPrec _ VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = showString "VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT"
  showsPrec p (VkQueueGlobalPriorityEXT x) = showParen (p >= 11) (showString "VkQueueGlobalPriorityEXT " . showsPrec 11 x)

instance Read VkQueueGlobalPriorityEXT where
  readPrec = parens ( choose [ ("VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT",      pure VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT)
                             , ("VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT",   pure VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT)
                             , ("VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT",     pure VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT)
                             , ("VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT", pure VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueueGlobalPriorityEXT")
                        v <- step readPrec
                        pure (VkQueueGlobalPriorityEXT v)
                        )
                    )

-- No documentation found for Nested "VkQueueGlobalPriorityEXT" "VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT"
pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT :: VkQueueGlobalPriorityEXT
pattern VK_QUEUE_GLOBAL_PRIORITY_LOW_EXT = VkQueueGlobalPriorityEXT 128

-- No documentation found for Nested "VkQueueGlobalPriorityEXT" "VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT"
pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT :: VkQueueGlobalPriorityEXT
pattern VK_QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT = VkQueueGlobalPriorityEXT 256

-- No documentation found for Nested "VkQueueGlobalPriorityEXT" "VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT"
pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT :: VkQueueGlobalPriorityEXT
pattern VK_QUEUE_GLOBAL_PRIORITY_HIGH_EXT = VkQueueGlobalPriorityEXT 512

-- No documentation found for Nested "VkQueueGlobalPriorityEXT" "VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT"
pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT :: VkQueueGlobalPriorityEXT
pattern VK_QUEUE_GLOBAL_PRIORITY_REALTIME_EXT = VkQueueGlobalPriorityEXT 1024
-- No documentation found for Nested "VkResult" "VK_ERROR_NOT_PERMITTED_EXT"
pattern VK_ERROR_NOT_PERMITTED_EXT :: VkResult
pattern VK_ERROR_NOT_PERMITTED_EXT = VkResult (-1000174001)
-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME"
pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"
-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION"
pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT = VkStructureType 1000174000
