{-# language CPP #-}
-- | = Name
--
-- VK_EXT_validation_features - instance extension
--
-- == VK_EXT_validation_features
--
-- [__Name String__]
--     @VK_EXT_validation_features@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     248
--
-- [__Revision__]
--     4
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Debugging tools>
--
-- [__Contact__]
--
--     -   Karl Schultz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_validation_features:%20&body=@karl-lunarg%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-11-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Karl Schultz, LunarG
--
--     -   Dave Houlton, LunarG
--
--     -   Mark Lobodzinski, LunarG
--
--     -   Camden Stocker, LunarG
--
--     -   Tony Barbour, LunarG
--
--     -   John Zulauf, LunarG
--
-- == Description
--
-- This extension provides the 'ValidationFeaturesEXT' struct that can be
-- included in the @pNext@ chain of the
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure passed
-- as the @pCreateInfo@ parameter of
-- 'Vulkan.Core10.DeviceInitialization.createInstance'. The structure
-- contains an array of 'ValidationFeatureEnableEXT' enum values that
-- enable specific validation features that are disabled by default. The
-- structure also contains an array of 'ValidationFeatureDisableEXT' enum
-- values that disable specific validation layer features that are enabled
-- by default.
--
-- Note
--
-- The @VK_EXT_validation_features@ extension subsumes all the
-- functionality provided in the @VK_EXT_validation_flags@ extension.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'ValidationFeaturesEXT'
--
-- == New Enums
--
-- -   'ValidationFeatureDisableEXT'
--
-- -   'ValidationFeatureEnableEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VALIDATION_FEATURES_EXTENSION_NAME'
--
-- -   'EXT_VALIDATION_FEATURES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VALIDATION_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-11-14 (Karl Schultz)
--
--     -   Initial revision
--
-- -   Revision 2, 2019-08-06 (Mark Lobodzinski)
--
--     -   Add Best Practices enable
--
-- -   Revision 3, 2020-03-04 (Tony Barbour)
--
--     -   Add Debug Printf enable
--
-- -   Revision 4, 2020-07-29 (John Zulauf)
--
--     -   Add Synchronization Validation enable
--
-- = See Also
--
-- 'ValidationFeatureDisableEXT', 'ValidationFeatureEnableEXT',
-- 'ValidationFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_validation_features Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_validation_features  ( ValidationFeaturesEXT(..)
                                                     , ValidationFeatureEnableEXT( VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT
                                                                                 , VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
                                                                                 , VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT
                                                                                 , VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT
                                                                                 , VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT
                                                                                 , ..
                                                                                 )
                                                     , ValidationFeatureDisableEXT( VALIDATION_FEATURE_DISABLE_ALL_EXT
                                                                                  , VALIDATION_FEATURE_DISABLE_SHADERS_EXT
                                                                                  , VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT
                                                                                  , VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT
                                                                                  , VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT
                                                                                  , VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT
                                                                                  , VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT
                                                                                  , ..
                                                                                  )
                                                     , EXT_VALIDATION_FEATURES_SPEC_VERSION
                                                     , pattern EXT_VALIDATION_FEATURES_SPEC_VERSION
                                                     , EXT_VALIDATION_FEATURES_EXTENSION_NAME
                                                     , pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME
                                                     ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VALIDATION_FEATURES_EXT))
-- | VkValidationFeaturesEXT - Specify validation features to enable or
-- disable for a Vulkan instance
--
-- == Valid Usage
--
-- -   #VUID-VkValidationFeaturesEXT-pEnabledValidationFeatures-02967# If
--     the @pEnabledValidationFeatures@ array contains
--     'VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT',
--     then it /must/ also contain
--     'VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT'
--
-- -   #VUID-VkValidationFeaturesEXT-pEnabledValidationFeatures-02968# If
--     the @pEnabledValidationFeatures@ array contains
--     'VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT', then it /must/ not
--     contain 'VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkValidationFeaturesEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VALIDATION_FEATURES_EXT'
--
-- -   #VUID-VkValidationFeaturesEXT-pEnabledValidationFeatures-parameter#
--     If @enabledValidationFeatureCount@ is not @0@,
--     @pEnabledValidationFeatures@ /must/ be a valid pointer to an array
--     of @enabledValidationFeatureCount@ valid
--     'ValidationFeatureEnableEXT' values
--
-- -   #VUID-VkValidationFeaturesEXT-pDisabledValidationFeatures-parameter#
--     If @disabledValidationFeatureCount@ is not @0@,
--     @pDisabledValidationFeatures@ /must/ be a valid pointer to an array
--     of @disabledValidationFeatureCount@ valid
--     'ValidationFeatureDisableEXT' values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'ValidationFeatureDisableEXT', 'ValidationFeatureEnableEXT'
data ValidationFeaturesEXT = ValidationFeaturesEXT
  { -- | @pEnabledValidationFeatures@ is a pointer to an array of
    -- 'ValidationFeatureEnableEXT' values specifying the validation features
    -- to be enabled.
    enabledValidationFeatures :: Vector ValidationFeatureEnableEXT
  , -- | @pDisabledValidationFeatures@ is a pointer to an array of
    -- 'ValidationFeatureDisableEXT' values specifying the validation features
    -- to be disabled.
    disabledValidationFeatures :: Vector ValidationFeatureDisableEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ValidationFeaturesEXT)
#endif
deriving instance Show ValidationFeaturesEXT

instance ToCStruct ValidationFeaturesEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ValidationFeaturesEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_FEATURES_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledValidationFeatures)) :: Word32))
    pPEnabledValidationFeatures' <- ContT $ allocaBytesAligned @ValidationFeatureEnableEXT ((Data.Vector.length (enabledValidationFeatures)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPEnabledValidationFeatures' `plusPtr` (4 * (i)) :: Ptr ValidationFeatureEnableEXT) (e)) (enabledValidationFeatures)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ValidationFeatureEnableEXT))) (pPEnabledValidationFeatures')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (disabledValidationFeatures)) :: Word32))
    pPDisabledValidationFeatures' <- ContT $ allocaBytesAligned @ValidationFeatureDisableEXT ((Data.Vector.length (disabledValidationFeatures)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDisabledValidationFeatures' `plusPtr` (4 * (i)) :: Ptr ValidationFeatureDisableEXT) (e)) (disabledValidationFeatures)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ValidationFeatureDisableEXT))) (pPDisabledValidationFeatures')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ValidationFeaturesEXT where
  peekCStruct p = do
    enabledValidationFeatureCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pEnabledValidationFeatures <- peek @(Ptr ValidationFeatureEnableEXT) ((p `plusPtr` 24 :: Ptr (Ptr ValidationFeatureEnableEXT)))
    pEnabledValidationFeatures' <- generateM (fromIntegral enabledValidationFeatureCount) (\i -> peek @ValidationFeatureEnableEXT ((pEnabledValidationFeatures `advancePtrBytes` (4 * (i)) :: Ptr ValidationFeatureEnableEXT)))
    disabledValidationFeatureCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pDisabledValidationFeatures <- peek @(Ptr ValidationFeatureDisableEXT) ((p `plusPtr` 40 :: Ptr (Ptr ValidationFeatureDisableEXT)))
    pDisabledValidationFeatures' <- generateM (fromIntegral disabledValidationFeatureCount) (\i -> peek @ValidationFeatureDisableEXT ((pDisabledValidationFeatures `advancePtrBytes` (4 * (i)) :: Ptr ValidationFeatureDisableEXT)))
    pure $ ValidationFeaturesEXT
             pEnabledValidationFeatures' pDisabledValidationFeatures'

instance Zero ValidationFeaturesEXT where
  zero = ValidationFeaturesEXT
           mempty
           mempty


-- | VkValidationFeatureEnableEXT - Specify validation features to enable
--
-- = See Also
--
-- 'ValidationFeaturesEXT'
newtype ValidationFeatureEnableEXT = ValidationFeatureEnableEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT' specifies that GPU-assisted
-- validation is enabled. Activating this feature instruments shader
-- programs to generate additional diagnostic data. This feature is
-- disabled by default.
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT                      = ValidationFeatureEnableEXT 0
-- | 'VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT'
-- specifies that the validation layers reserve a descriptor set binding
-- slot for their own use. The layer reports a value for
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxBoundDescriptorSets@
-- that is one less than the value reported by the device. If the device
-- supports the binding of only one descriptor set, the validation layer
-- does not perform GPU-assisted validation. This feature is disabled by
-- default.
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT = ValidationFeatureEnableEXT 1
-- | 'VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT' specifies that Vulkan
-- best-practices validation is enabled. Activating this feature enables
-- the output of warnings related to common misuse of the API, but which
-- are not explicitly prohibited by the specification. This feature is
-- disabled by default.
pattern VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT                    = ValidationFeatureEnableEXT 2
-- | 'VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT' specifies that the layers
-- will process @debugPrintfEXT@ operations in shaders and send the
-- resulting output to the debug callback. This feature is disabled by
-- default.
pattern VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT                      = ValidationFeatureEnableEXT 3
-- | 'VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT' specifies
-- that Vulkan synchronization validation is enabled. This feature reports
-- resource access conflicts due to missing or incorrect synchronization
-- operations between actions (Draw, Copy, Dispatch, Blit) reading or
-- writing the same regions of memory. This feature is disabled by default.
pattern VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT        = ValidationFeatureEnableEXT 4
{-# complete VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT,
             VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT,
             VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT,
             VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT,
             VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT :: ValidationFeatureEnableEXT #-}

conNameValidationFeatureEnableEXT :: String
conNameValidationFeatureEnableEXT = "ValidationFeatureEnableEXT"

enumPrefixValidationFeatureEnableEXT :: String
enumPrefixValidationFeatureEnableEXT = "VALIDATION_FEATURE_ENABLE_"

showTableValidationFeatureEnableEXT :: [(ValidationFeatureEnableEXT, String)]
showTableValidationFeatureEnableEXT =
  [ (VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT                     , "GPU_ASSISTED_EXT")
  , (VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT, "GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT")
  , (VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT                   , "BEST_PRACTICES_EXT")
  , (VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT                     , "DEBUG_PRINTF_EXT")
  , (VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT       , "SYNCHRONIZATION_VALIDATION_EXT")
  ]

instance Show ValidationFeatureEnableEXT where
  showsPrec = enumShowsPrec enumPrefixValidationFeatureEnableEXT
                            showTableValidationFeatureEnableEXT
                            conNameValidationFeatureEnableEXT
                            (\(ValidationFeatureEnableEXT x) -> x)
                            (showsPrec 11)

instance Read ValidationFeatureEnableEXT where
  readPrec = enumReadPrec enumPrefixValidationFeatureEnableEXT
                          showTableValidationFeatureEnableEXT
                          conNameValidationFeatureEnableEXT
                          ValidationFeatureEnableEXT


-- | VkValidationFeatureDisableEXT - Specify validation features to disable
--
-- = See Also
--
-- 'ValidationFeaturesEXT'
newtype ValidationFeatureDisableEXT = ValidationFeatureDisableEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'VALIDATION_FEATURE_DISABLE_ALL_EXT' specifies that all validation
-- checks are disabled.
pattern VALIDATION_FEATURE_DISABLE_ALL_EXT              = ValidationFeatureDisableEXT 0
-- | 'VALIDATION_FEATURE_DISABLE_SHADERS_EXT' specifies that shader
-- validation is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_SHADERS_EXT          = ValidationFeatureDisableEXT 1
-- | 'VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT' specifies that thread
-- safety validation is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT    = ValidationFeatureDisableEXT 2
-- | 'VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT' specifies that stateless
-- parameter validation is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT   = ValidationFeatureDisableEXT 3
-- | 'VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT' specifies that object
-- lifetime validation is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT = ValidationFeatureDisableEXT 4
-- | 'VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT' specifies that core
-- validation checks are disabled. This feature is enabled by default. If
-- this feature is disabled, the shader validation and GPU-assisted
-- validation features are also disabled.
pattern VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT      = ValidationFeatureDisableEXT 5
-- | 'VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT' specifies that
-- protection against duplicate non-dispatchable object handles is
-- disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT   = ValidationFeatureDisableEXT 6
{-# complete VALIDATION_FEATURE_DISABLE_ALL_EXT,
             VALIDATION_FEATURE_DISABLE_SHADERS_EXT,
             VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT,
             VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT,
             VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT,
             VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT,
             VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT :: ValidationFeatureDisableEXT #-}

conNameValidationFeatureDisableEXT :: String
conNameValidationFeatureDisableEXT = "ValidationFeatureDisableEXT"

enumPrefixValidationFeatureDisableEXT :: String
enumPrefixValidationFeatureDisableEXT = "VALIDATION_FEATURE_DISABLE_"

showTableValidationFeatureDisableEXT :: [(ValidationFeatureDisableEXT, String)]
showTableValidationFeatureDisableEXT =
  [ (VALIDATION_FEATURE_DISABLE_ALL_EXT             , "ALL_EXT")
  , (VALIDATION_FEATURE_DISABLE_SHADERS_EXT         , "SHADERS_EXT")
  , (VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT   , "THREAD_SAFETY_EXT")
  , (VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT  , "API_PARAMETERS_EXT")
  , (VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT, "OBJECT_LIFETIMES_EXT")
  , (VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT     , "CORE_CHECKS_EXT")
  , (VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT  , "UNIQUE_HANDLES_EXT")
  ]

instance Show ValidationFeatureDisableEXT where
  showsPrec = enumShowsPrec enumPrefixValidationFeatureDisableEXT
                            showTableValidationFeatureDisableEXT
                            conNameValidationFeatureDisableEXT
                            (\(ValidationFeatureDisableEXT x) -> x)
                            (showsPrec 11)

instance Read ValidationFeatureDisableEXT where
  readPrec = enumReadPrec enumPrefixValidationFeatureDisableEXT
                          showTableValidationFeatureDisableEXT
                          conNameValidationFeatureDisableEXT
                          ValidationFeatureDisableEXT


type EXT_VALIDATION_FEATURES_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FEATURES_SPEC_VERSION"
pattern EXT_VALIDATION_FEATURES_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VALIDATION_FEATURES_SPEC_VERSION = 4


type EXT_VALIDATION_FEATURES_EXTENSION_NAME = "VK_EXT_validation_features"

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME"
pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME = "VK_EXT_validation_features"

