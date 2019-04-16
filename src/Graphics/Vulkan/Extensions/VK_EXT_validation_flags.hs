{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_flags
  ( ValidationCheckEXT
  , withCStructValidationFlagsEXT
  , fromCStructValidationFlagsEXT
  , ValidationFlagsEXT(..)
  , pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION
  , pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags
  ( VkValidationCheckEXT(..)
  , VkValidationFlagsEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags
  ( pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION
  )


-- No documentation found for TopLevel "ValidationCheckEXT"
type ValidationCheckEXT = VkValidationCheckEXT
-- No documentation found for TopLevel "ValidationFlagsEXT"
data ValidationFlagsEXT = ValidationFlagsEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "ValidationFlagsEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ValidationFlagsEXT" "pDisabledValidationChecks"
  vkPDisabledValidationChecks :: Vector ValidationCheckEXT
  }
  deriving (Show, Eq)
withCStructValidationFlagsEXT :: ValidationFlagsEXT -> (VkValidationFlagsEXT -> IO a) -> IO a
withCStructValidationFlagsEXT from cont = withVec (&) (vkPDisabledValidationChecks (from :: ValidationFlagsEXT)) (\pDisabledValidationChecks -> maybeWith withSomeVkStruct (vkPNext (from :: ValidationFlagsEXT)) (\pPNext -> cont (VkValidationFlagsEXT VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT pPNext (fromIntegral (Data.Vector.length (vkPDisabledValidationChecks (from :: ValidationFlagsEXT)))) pDisabledValidationChecks)))
fromCStructValidationFlagsEXT :: VkValidationFlagsEXT -> IO ValidationFlagsEXT
fromCStructValidationFlagsEXT c = ValidationFlagsEXT <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkValidationFlagsEXT)))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkDisabledValidationCheckCount (c :: VkValidationFlagsEXT))) (peekElemOff (vkPDisabledValidationChecks (c :: VkValidationFlagsEXT))))
