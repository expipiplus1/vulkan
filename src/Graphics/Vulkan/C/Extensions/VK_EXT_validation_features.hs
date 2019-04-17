{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( VkValidationFeatureDisableEXT(..)
  , pattern VK_VALIDATION_FEATURE_DISABLE_ALL_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT
  , pattern VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT
  , VkValidationFeatureEnableEXT(..)
  , pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT
  , pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
  , VkValidationFeaturesEXT(..)
  , pattern VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME
  , pattern VK_EXT_VALIDATION_FEATURES_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
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


-- ** VkValidationFeatureDisableEXT

-- | VkValidationFeatureDisableEXT - Specify validation features to disable
--
-- = See Also
--
-- No cross-references are available
newtype VkValidationFeatureDisableEXT = VkValidationFeatureDisableEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkValidationFeatureDisableEXT where
  showsPrec _ VK_VALIDATION_FEATURE_DISABLE_ALL_EXT = showString "VK_VALIDATION_FEATURE_DISABLE_ALL_EXT"
  showsPrec _ VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT = showString "VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT"
  showsPrec _ VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT = showString "VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT"
  showsPrec _ VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT = showString "VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT"
  showsPrec _ VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT = showString "VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT"
  showsPrec _ VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT = showString "VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT"
  showsPrec _ VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT = showString "VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT"
  showsPrec p (VkValidationFeatureDisableEXT x) = showParen (p >= 11) (showString "VkValidationFeatureDisableEXT " . showsPrec 11 x)

instance Read VkValidationFeatureDisableEXT where
  readPrec = parens ( choose [ ("VK_VALIDATION_FEATURE_DISABLE_ALL_EXT",              pure VK_VALIDATION_FEATURE_DISABLE_ALL_EXT)
                             , ("VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT",          pure VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT)
                             , ("VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT",    pure VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT)
                             , ("VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT",   pure VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT)
                             , ("VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT", pure VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT)
                             , ("VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT",      pure VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT)
                             , ("VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT",   pure VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkValidationFeatureDisableEXT")
                        v <- step readPrec
                        pure (VkValidationFeatureDisableEXT v)
                        )
                    )

-- | @VK_VALIDATION_FEATURE_DISABLE_ALL_EXT@ specifies that all validation
-- checks are disabled.
pattern VK_VALIDATION_FEATURE_DISABLE_ALL_EXT :: VkValidationFeatureDisableEXT
pattern VK_VALIDATION_FEATURE_DISABLE_ALL_EXT = VkValidationFeatureDisableEXT 0

-- | @VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT@ specifies that shader
-- validation is disabled. This feature is enabled by default.
pattern VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT :: VkValidationFeatureDisableEXT
pattern VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT = VkValidationFeatureDisableEXT 1

-- | @VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT@ specifies that thread
-- safety validation is disabled. This feature is enabled by default.
pattern VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT :: VkValidationFeatureDisableEXT
pattern VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT = VkValidationFeatureDisableEXT 2

-- | @VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT@ specifies that
-- stateless parameter validation is disabled. This feature is enabled by
-- default.
pattern VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT :: VkValidationFeatureDisableEXT
pattern VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT = VkValidationFeatureDisableEXT 3

-- | @VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT@ specifies that
-- object lifetime validation is disabled. This feature is enabled by
-- default.
pattern VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT :: VkValidationFeatureDisableEXT
pattern VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT = VkValidationFeatureDisableEXT 4

-- | @VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT@ specifies that core
-- validation checks are disabled. This feature is enabled by default. If
-- this feature is disabled, the shader validation and GPU-assisted
-- validation features are also disabled.
pattern VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT :: VkValidationFeatureDisableEXT
pattern VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT = VkValidationFeatureDisableEXT 5

-- | @VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT@ specifies that
-- protection against duplicate non-dispatchable object handles is
-- disabled. This feature is enabled by default.
pattern VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT :: VkValidationFeatureDisableEXT
pattern VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT = VkValidationFeatureDisableEXT 6
-- ** VkValidationFeatureEnableEXT

-- | VkValidationFeatureEnableEXT - Specify validation features to enable
--
-- = See Also
--
-- No cross-references are available
newtype VkValidationFeatureEnableEXT = VkValidationFeatureEnableEXT Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkValidationFeatureEnableEXT where
  showsPrec _ VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT = showString "VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT"
  showsPrec _ VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT = showString "VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT"
  showsPrec p (VkValidationFeatureEnableEXT x) = showParen (p >= 11) (showString "VkValidationFeatureEnableEXT " . showsPrec 11 x)

instance Read VkValidationFeatureEnableEXT where
  readPrec = parens ( choose [ ("VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT",                      pure VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT)
                             , ("VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT", pure VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkValidationFeatureEnableEXT")
                        v <- step readPrec
                        pure (VkValidationFeatureEnableEXT v)
                        )
                    )

-- | @VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT@ specifies that
-- GPU-assisted validation is enabled. Activating this feature instruments
-- shader programs to generate additional diagnostic data. This feature is
-- disabled by default.
pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT :: VkValidationFeatureEnableEXT
pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT = VkValidationFeatureEnableEXT 0

-- | @VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT@
-- specifies that the validation layers reserve a descriptor set binding
-- slot for their own use. The layer reports a value for
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxBoundDescriptorSets@
-- that is one less than the value reported by the device. If the device
-- supports the binding of only one descriptor set, the validation layer
-- does not perform GPU-assisted validation. This feature is disabled by
-- default. The GPU-assisted validation feature must be enabled in order to
-- use this feature.
pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT :: VkValidationFeatureEnableEXT
pattern VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT = VkValidationFeatureEnableEXT 1
-- | VkValidationFeaturesEXT - Specify validation features to enable or
-- disable for a Vulkan instance
--
-- = Description
--
-- Unresolved directive in VkValidationFeaturesEXT.txt -
-- include::..\/validity\/structs\/VkValidationFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkValidationFeaturesEXT = VkValidationFeaturesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @enabledValidationFeatureCount@ is the number of features to enable.
  vkEnabledValidationFeatureCount :: Word32
  , -- | @pEnabledValidationFeatures@ is a pointer to an array of
  -- 'VkValidationFeatureEnableEXT' values specifying the validation features
  -- to be enabled.
  vkPEnabledValidationFeatures :: Ptr VkValidationFeatureEnableEXT
  , -- | @disabledValidationFeatureCount@ is the number of features to disable.
  vkDisabledValidationFeatureCount :: Word32
  , -- | @pDisabledValidationFeatures@ is a pointer to an array of
  -- 'VkValidationFeatureDisableEXT' values specifying the validation
  -- features to be disabled.
  vkPDisabledValidationFeatures :: Ptr VkValidationFeatureDisableEXT
  }
  deriving (Eq, Show)

instance Storable VkValidationFeaturesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkValidationFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkValidationFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkValidationFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkEnabledValidationFeatureCount (poked :: VkValidationFeaturesEXT))
                *> poke (ptr `plusPtr` 24) (vkPEnabledValidationFeatures (poked :: VkValidationFeaturesEXT))
                *> poke (ptr `plusPtr` 32) (vkDisabledValidationFeatureCount (poked :: VkValidationFeaturesEXT))
                *> poke (ptr `plusPtr` 40) (vkPDisabledValidationFeatures (poked :: VkValidationFeaturesEXT))

instance Zero VkValidationFeaturesEXT where
  zero = VkValidationFeaturesEXT zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
-- No documentation found for TopLevel "VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME"
pattern VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME = "VK_EXT_validation_features"
-- No documentation found for TopLevel "VK_EXT_VALIDATION_FEATURES_SPEC_VERSION"
pattern VK_EXT_VALIDATION_FEATURES_SPEC_VERSION :: Integral a => a
pattern VK_EXT_VALIDATION_FEATURES_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT = VkStructureType 1000247000
