{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_validation_flags
  ( ValidationCheckEXT
  , pattern VALIDATION_CHECK_ALL_EXT
  , pattern VALIDATION_CHECK_SHADERS_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , ValidationFlagsEXT(..)
#endif
  , pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME
  , pattern EXT_VALIDATION_FLAGS_SPEC_VERSION
  , pattern STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags
  ( VkValidationCheckEXT(..)
  , pattern VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_FLAGS_SPEC_VERSION
  , pattern VK_VALIDATION_CHECK_ALL_EXT
  , pattern VK_VALIDATION_CHECK_SHADERS_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  )


-- No documentation found for TopLevel "ValidationCheckEXT"
type ValidationCheckEXT = VkValidationCheckEXT


{-# complete VALIDATION_CHECK_ALL_EXT, VALIDATION_CHECK_SHADERS_EXT :: ValidationCheckEXT #-}


-- No documentation found for Nested "ValidationCheckEXT" "VALIDATION_CHECK_ALL_EXT"
pattern VALIDATION_CHECK_ALL_EXT :: (a ~ ValidationCheckEXT) => a
pattern VALIDATION_CHECK_ALL_EXT = VK_VALIDATION_CHECK_ALL_EXT


-- No documentation found for Nested "ValidationCheckEXT" "VALIDATION_CHECK_SHADERS_EXT"
pattern VALIDATION_CHECK_SHADERS_EXT :: (a ~ ValidationCheckEXT) => a
pattern VALIDATION_CHECK_SHADERS_EXT = VK_VALIDATION_CHECK_SHADERS_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkValidationFlagsEXT"
data ValidationFlagsEXT = ValidationFlagsEXT
  { -- No documentation found for Nested "ValidationFlagsEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ValidationFlagsEXT" "pDisabledValidationChecks"
  disabledValidationChecks :: Vector ValidationCheckEXT
  }
  deriving (Show, Eq)

instance Zero ValidationFlagsEXT where
  zero = ValidationFlagsEXT Nothing
                            mempty

#endif

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME"
pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_VALIDATION_FLAGS_EXTENSION_NAME = VK_EXT_VALIDATION_FLAGS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FLAGS_SPEC_VERSION"
pattern EXT_VALIDATION_FLAGS_SPEC_VERSION :: Integral a => a
pattern EXT_VALIDATION_FLAGS_SPEC_VERSION = VK_EXT_VALIDATION_FLAGS_SPEC_VERSION
