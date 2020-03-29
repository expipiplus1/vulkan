{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_validation_features  ( ValidationFeaturesEXT(..)
                                                              , ValidationFeatureEnableEXT( VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT
                                                                                          , VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT
                                                                                          , VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT
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

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VALIDATION_FEATURES_EXT))
-- | VkValidationFeaturesEXT - Specify validation features to enable or
-- disable for a Vulkan instance
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VALIDATION_FEATURES_EXT'
--
-- -   If @enabledValidationFeatureCount@ is not @0@,
--     @pEnabledValidationFeatures@ /must/ be a valid pointer to an array
--     of @enabledValidationFeatureCount@ valid
--     'ValidationFeatureEnableEXT' values
--
-- -   If @disabledValidationFeatureCount@ is not @0@,
--     @pDisabledValidationFeatures@ /must/ be a valid pointer to an array
--     of @disabledValidationFeatureCount@ valid
--     'ValidationFeatureDisableEXT' values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
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
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_VALIDATION_FEATURES_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPEnabledValidationFeatures' <- ContT $ allocaBytesAligned @ValidationFeatureEnableEXT ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPEnabledValidationFeatures' `plusPtr` (4 * (i)) :: Ptr ValidationFeatureEnableEXT) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ValidationFeatureEnableEXT))) (pPEnabledValidationFeatures')
    pPDisabledValidationFeatures' <- ContT $ allocaBytesAligned @ValidationFeatureDisableEXT ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDisabledValidationFeatures' `plusPtr` (4 * (i)) :: Ptr ValidationFeatureDisableEXT) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr ValidationFeatureDisableEXT))) (pPDisabledValidationFeatures')
    lift $ f

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
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT = ValidationFeatureEnableEXT 0
-- | 'VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT'
-- specifies that the validation layers reserve a descriptor set binding
-- slot for their own use. The layer reports a value for
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxBoundDescriptorSets@
-- that is one less than the value reported by the device. If the device
-- supports the binding of only one descriptor set, the validation layer
-- does not perform GPU-assisted validation. This feature is disabled by
-- default. The GPU-assisted validation feature must be enabled in order to
-- use this feature.
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT = ValidationFeatureEnableEXT 1
-- | 'VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT' specifies that Vulkan
-- best-practices validation is enabled. Activating this feature enables
-- the output of warnings related to common misuse of the API, but which
-- are not explicitly prohibited by the specification. This feature is
-- disabled by default.
pattern VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT = ValidationFeatureEnableEXT 2
{-# complete VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT,
             VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT,
             VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT :: ValidationFeatureEnableEXT #-}

instance Show ValidationFeatureEnableEXT where
  showsPrec p = \case
    VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT -> showString "VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT"
    VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT -> showString "VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT"
    VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT -> showString "VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT"
    ValidationFeatureEnableEXT x -> showParen (p >= 11) (showString "ValidationFeatureEnableEXT " . showsPrec 11 x)

instance Read ValidationFeatureEnableEXT where
  readPrec = parens (choose [("VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT", pure VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT)
                            , ("VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT", pure VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT)
                            , ("VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT", pure VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ValidationFeatureEnableEXT")
                       v <- step readPrec
                       pure (ValidationFeatureEnableEXT v)))


-- | VkValidationFeatureDisableEXT - Specify validation features to disable
--
-- = See Also
--
-- 'ValidationFeaturesEXT'
newtype ValidationFeatureDisableEXT = ValidationFeatureDisableEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'VALIDATION_FEATURE_DISABLE_ALL_EXT' specifies that all validation
-- checks are disabled.
pattern VALIDATION_FEATURE_DISABLE_ALL_EXT = ValidationFeatureDisableEXT 0
-- | 'VALIDATION_FEATURE_DISABLE_SHADERS_EXT' specifies that shader
-- validation is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_SHADERS_EXT = ValidationFeatureDisableEXT 1
-- | 'VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT' specifies that thread
-- safety validation is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT = ValidationFeatureDisableEXT 2
-- | 'VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT' specifies that stateless
-- parameter validation is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT = ValidationFeatureDisableEXT 3
-- | 'VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT' specifies that object
-- lifetime validation is disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT = ValidationFeatureDisableEXT 4
-- | 'VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT' specifies that core
-- validation checks are disabled. This feature is enabled by default. If
-- this feature is disabled, the shader validation and GPU-assisted
-- validation features are also disabled.
pattern VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT = ValidationFeatureDisableEXT 5
-- | 'VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT' specifies that
-- protection against duplicate non-dispatchable object handles is
-- disabled. This feature is enabled by default.
pattern VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT = ValidationFeatureDisableEXT 6
{-# complete VALIDATION_FEATURE_DISABLE_ALL_EXT,
             VALIDATION_FEATURE_DISABLE_SHADERS_EXT,
             VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT,
             VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT,
             VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT,
             VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT,
             VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT :: ValidationFeatureDisableEXT #-}

instance Show ValidationFeatureDisableEXT where
  showsPrec p = \case
    VALIDATION_FEATURE_DISABLE_ALL_EXT -> showString "VALIDATION_FEATURE_DISABLE_ALL_EXT"
    VALIDATION_FEATURE_DISABLE_SHADERS_EXT -> showString "VALIDATION_FEATURE_DISABLE_SHADERS_EXT"
    VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT -> showString "VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT"
    VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT -> showString "VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT"
    VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT -> showString "VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT"
    VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT -> showString "VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT"
    VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT -> showString "VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT"
    ValidationFeatureDisableEXT x -> showParen (p >= 11) (showString "ValidationFeatureDisableEXT " . showsPrec 11 x)

instance Read ValidationFeatureDisableEXT where
  readPrec = parens (choose [("VALIDATION_FEATURE_DISABLE_ALL_EXT", pure VALIDATION_FEATURE_DISABLE_ALL_EXT)
                            , ("VALIDATION_FEATURE_DISABLE_SHADERS_EXT", pure VALIDATION_FEATURE_DISABLE_SHADERS_EXT)
                            , ("VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT", pure VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT)
                            , ("VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT", pure VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT)
                            , ("VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT", pure VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT)
                            , ("VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT", pure VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT)
                            , ("VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT", pure VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ValidationFeatureDisableEXT")
                       v <- step readPrec
                       pure (ValidationFeatureDisableEXT v)))


type EXT_VALIDATION_FEATURES_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FEATURES_SPEC_VERSION"
pattern EXT_VALIDATION_FEATURES_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_VALIDATION_FEATURES_SPEC_VERSION = 2


type EXT_VALIDATION_FEATURES_EXTENSION_NAME = "VK_EXT_validation_features"

-- No documentation found for TopLevel "VK_EXT_VALIDATION_FEATURES_EXTENSION_NAME"
pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_VALIDATION_FEATURES_EXTENSION_NAME = "VK_EXT_validation_features"

