{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackCreateInfoEXT(..)
  , VkPipelineCreationFeedbackEXT(..)
  , VkPipelineCreationFeedbackFlagBitsEXT(..)
  , pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT
  , pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT
  , pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT
  , VkPipelineCreationFeedbackFlagsEXT
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME
  , pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  , Word64
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
  , VkFlags
  )


-- No documentation found for TopLevel "VkPipelineCreationFeedbackCreateInfoEXT"
data VkPipelineCreationFeedbackCreateInfoEXT = VkPipelineCreationFeedbackCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineCreationFeedbackCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineCreationFeedbackCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineCreationFeedbackCreateInfoEXT" "pPipelineCreationFeedback"
  vkPPipelineCreationFeedback :: Ptr VkPipelineCreationFeedbackEXT
  , -- No documentation found for Nested "VkPipelineCreationFeedbackCreateInfoEXT" "pipelineStageCreationFeedbackCount"
  vkPipelineStageCreationFeedbackCount :: Word32
  , -- No documentation found for Nested "VkPipelineCreationFeedbackCreateInfoEXT" "pPipelineStageCreationFeedbacks"
  vkPPipelineStageCreationFeedbacks :: Ptr VkPipelineCreationFeedbackEXT
  }
  deriving (Eq, Show)

instance Storable VkPipelineCreationFeedbackCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPipelineCreationFeedbackCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
                                                     <*> peek (ptr `plusPtr` 24)
                                                     <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineCreationFeedbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineCreationFeedbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkPPipelineCreationFeedback (poked :: VkPipelineCreationFeedbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPipelineStageCreationFeedbackCount (poked :: VkPipelineCreationFeedbackCreateInfoEXT))
                *> poke (ptr `plusPtr` 32) (vkPPipelineStageCreationFeedbacks (poked :: VkPipelineCreationFeedbackCreateInfoEXT))

instance Zero VkPipelineCreationFeedbackCreateInfoEXT where
  zero = VkPipelineCreationFeedbackCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
                                                 zero
                                                 zero
                                                 zero
                                                 zero

-- No documentation found for TopLevel "VkPipelineCreationFeedbackEXT"
data VkPipelineCreationFeedbackEXT = VkPipelineCreationFeedbackEXT
  { -- No documentation found for Nested "VkPipelineCreationFeedbackEXT" "flags"
  vkFlags :: VkPipelineCreationFeedbackFlagsEXT
  , -- No documentation found for Nested "VkPipelineCreationFeedbackEXT" "duration"
  vkDuration :: Word64
  }
  deriving (Eq, Show)

instance Storable VkPipelineCreationFeedbackEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkPipelineCreationFeedbackEXT <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkFlags (poked :: VkPipelineCreationFeedbackEXT))
                *> poke (ptr `plusPtr` 8) (vkDuration (poked :: VkPipelineCreationFeedbackEXT))

instance Zero VkPipelineCreationFeedbackEXT where
  zero = VkPipelineCreationFeedbackEXT zero
                                       zero

-- ** VkPipelineCreationFeedbackFlagBitsEXT

-- No documentation found for TopLevel "VkPipelineCreationFeedbackFlagBitsEXT"
newtype VkPipelineCreationFeedbackFlagBitsEXT = VkPipelineCreationFeedbackFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkPipelineCreationFeedbackFlagBitsEXT where
  showsPrec _ VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = showString "VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT"
  showsPrec _ VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = showString "VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT"
  showsPrec _ VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = showString "VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT"
  showsPrec p (VkPipelineCreationFeedbackFlagBitsEXT x) = showParen (p >= 11) (showString "VkPipelineCreationFeedbackFlagBitsEXT " . showsPrec 11 x)

instance Read VkPipelineCreationFeedbackFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT",                          pure VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT)
                             , ("VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT", pure VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT)
                             , ("VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT",     pure VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineCreationFeedbackFlagBitsEXT")
                        v <- step readPrec
                        pure (VkPipelineCreationFeedbackFlagBitsEXT v)
                        )
                    )

-- No documentation found for Nested "VkPipelineCreationFeedbackFlagBitsEXT" "VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT"
pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT :: VkPipelineCreationFeedbackFlagBitsEXT
pattern VK_PIPELINE_CREATION_FEEDBACK_VALID_BIT_EXT = VkPipelineCreationFeedbackFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkPipelineCreationFeedbackFlagBitsEXT" "VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT"
pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT :: VkPipelineCreationFeedbackFlagBitsEXT
pattern VK_PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT_EXT = VkPipelineCreationFeedbackFlagBitsEXT 0x00000002

-- No documentation found for Nested "VkPipelineCreationFeedbackFlagBitsEXT" "VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT"
pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT :: VkPipelineCreationFeedbackFlagBitsEXT
pattern VK_PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT_EXT = VkPipelineCreationFeedbackFlagBitsEXT 0x00000004

-- No documentation found for TopLevel "VkPipelineCreationFeedbackFlagsEXT"
type VkPipelineCreationFeedbackFlagsEXT = VkPipelineCreationFeedbackFlagBitsEXT

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME"
pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_EXTENSION_NAME = "VK_EXT_pipeline_creation_feedback"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION"
pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION :: Integral a => a
pattern VK_EXT_PIPELINE_CREATION_FEEDBACK_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT = VkStructureType 1000192000
