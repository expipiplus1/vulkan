{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior
  ( VkDeviceMemoryOverallocationCreateInfoAMD(..)
  , VkMemoryOverallocationBehaviorAMD(..)
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD
  , pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD
  , pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME
  , pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
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
  ( VkStructureType(..)
  , Zero(..)
  )


-- No documentation found for TopLevel "VkDeviceMemoryOverallocationCreateInfoAMD"
data VkDeviceMemoryOverallocationCreateInfoAMD = VkDeviceMemoryOverallocationCreateInfoAMD
  { -- No documentation found for Nested "VkDeviceMemoryOverallocationCreateInfoAMD" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceMemoryOverallocationCreateInfoAMD" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceMemoryOverallocationCreateInfoAMD" "overallocationBehavior"
  vkOverallocationBehavior :: VkMemoryOverallocationBehaviorAMD
  }
  deriving (Eq, Show)

instance Storable VkDeviceMemoryOverallocationCreateInfoAMD where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceMemoryOverallocationCreateInfoAMD <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceMemoryOverallocationCreateInfoAMD))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceMemoryOverallocationCreateInfoAMD))
                *> poke (ptr `plusPtr` 16) (vkOverallocationBehavior (poked :: VkDeviceMemoryOverallocationCreateInfoAMD))

instance Zero VkDeviceMemoryOverallocationCreateInfoAMD where
  zero = VkDeviceMemoryOverallocationCreateInfoAMD VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
                                                   zero
                                                   zero

-- ** VkMemoryOverallocationBehaviorAMD

-- No documentation found for TopLevel "VkMemoryOverallocationBehaviorAMD"
newtype VkMemoryOverallocationBehaviorAMD = VkMemoryOverallocationBehaviorAMD Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkMemoryOverallocationBehaviorAMD where
  showsPrec _ VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD = showString "VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD"
  showsPrec _ VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD = showString "VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD"
  showsPrec _ VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD = showString "VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD"
  showsPrec p (VkMemoryOverallocationBehaviorAMD x) = showParen (p >= 11) (showString "VkMemoryOverallocationBehaviorAMD " . showsPrec 11 x)

instance Read VkMemoryOverallocationBehaviorAMD where
  readPrec = parens ( choose [ ("VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD",    pure VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD)
                             , ("VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD",    pure VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD)
                             , ("VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD", pure VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryOverallocationBehaviorAMD")
                        v <- step readPrec
                        pure (VkMemoryOverallocationBehaviorAMD v)
                        )
                    )

-- No documentation found for Nested "VkMemoryOverallocationBehaviorAMD" "VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD"
pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD :: VkMemoryOverallocationBehaviorAMD
pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DEFAULT_AMD = VkMemoryOverallocationBehaviorAMD 0

-- No documentation found for Nested "VkMemoryOverallocationBehaviorAMD" "VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD"
pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD :: VkMemoryOverallocationBehaviorAMD
pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_ALLOWED_AMD = VkMemoryOverallocationBehaviorAMD 1

-- No documentation found for Nested "VkMemoryOverallocationBehaviorAMD" "VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD"
pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD :: VkMemoryOverallocationBehaviorAMD
pattern VK_MEMORY_OVERALLOCATION_BEHAVIOR_DISALLOWED_AMD = VkMemoryOverallocationBehaviorAMD 2

-- No documentation found for TopLevel "VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME"
pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_EXTENSION_NAME = "VK_AMD_memory_overallocation_behavior"

-- No documentation found for TopLevel "VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION"
pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION :: Integral a => a
pattern VK_AMD_MEMORY_OVERALLOCATION_BEHAVIOR_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD"
pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD = VkStructureType 1000189000
