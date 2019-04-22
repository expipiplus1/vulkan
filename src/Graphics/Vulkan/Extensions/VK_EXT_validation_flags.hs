{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_flags
  ( ValidationCheckEXT
  , pattern VALIDATION_CHECK_ALL_EXT
  , pattern VALIDATION_CHECK_SHADERS_EXT
  , withCStructValidationFlagsEXT
  , fromCStructValidationFlagsEXT
  , ValidationFlagsEXT(..)
  , pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME
  , pattern EXT_VALIDATION_FLAGS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  ) where

import Data.Function
  ( (&)
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags
  ( VkValidationCheckEXT(..)
  , VkValidationFlagsEXT(..)
  , pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  , pattern VK_VALIDATION_CHECK_ALL_EXT
  , pattern VK_VALIDATION_CHECK_SHADERS_EXT
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  )


-- | VkValidationCheckEXT - Specify validation checks to disable
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags.VkValidationFlagsEXT'
type ValidationCheckEXT = VkValidationCheckEXT


{-# complete VALIDATION_CHECK_ALL_EXT, VALIDATION_CHECK_SHADERS_EXT :: ValidationCheckEXT #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags.VK_VALIDATION_CHECK_ALL_EXT'
-- specifies that all validation checks are disabled.
pattern VALIDATION_CHECK_ALL_EXT :: (a ~ ValidationCheckEXT) => a
pattern VALIDATION_CHECK_ALL_EXT = VK_VALIDATION_CHECK_ALL_EXT


-- | 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags.VK_VALIDATION_CHECK_SHADERS_EXT'
-- specifies that shader validation is disabled.
pattern VALIDATION_CHECK_SHADERS_EXT :: (a ~ ValidationCheckEXT) => a
pattern VALIDATION_CHECK_SHADERS_EXT = VK_VALIDATION_CHECK_SHADERS_EXT


-- | VkValidationFlagsEXT - Specify validation checks to disable for a Vulkan
-- instance
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags.VkValidationCheckEXT'
data ValidationFlagsEXT = ValidationFlagsEXT
  { -- Univalued member elided
  -- No documentation found for Nested "ValidationFlagsEXT" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "ValidationFlagsEXT" "pDisabledValidationChecks"
  disabledValidationChecks :: Vector ValidationCheckEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkValidationFlagsEXT' and
-- marshal a 'ValidationFlagsEXT' into it. The 'VkValidationFlagsEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructValidationFlagsEXT :: ValidationFlagsEXT -> (VkValidationFlagsEXT -> IO a) -> IO a
withCStructValidationFlagsEXT marshalled cont = withVec (&) (disabledValidationChecks (marshalled :: ValidationFlagsEXT)) (\pPDisabledValidationChecks -> maybeWith withSomeVkStruct (next (marshalled :: ValidationFlagsEXT)) (\pPNext -> cont (VkValidationFlagsEXT VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT pPNext (fromIntegral (Data.Vector.length (disabledValidationChecks (marshalled :: ValidationFlagsEXT)))) pPDisabledValidationChecks)))

-- | A function to read a 'VkValidationFlagsEXT' and all additional
-- structures in the pointer chain into a 'ValidationFlagsEXT'.
fromCStructValidationFlagsEXT :: VkValidationFlagsEXT -> IO ValidationFlagsEXT
fromCStructValidationFlagsEXT c = ValidationFlagsEXT <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkValidationFlagsEXT)))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkDisabledValidationCheckCount (c :: VkValidationFlagsEXT))) (peekElemOff (vkPDisabledValidationChecks (c :: VkValidationFlagsEXT))))

instance Zero ValidationFlagsEXT where
  zero = ValidationFlagsEXT Nothing
                            Data.Vector.empty


-- No documentation found for TopLevel "VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME"
pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME = VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FLAGS_SPEC_VERSION"
pattern EXT_VALIDATION_FLAGS_SPEC_VERSION :: Integral a => a
pattern EXT_VALIDATION_FLAGS_SPEC_VERSION = VK_EXT_VALIDATION_FLAGS_SPEC_VERSION
