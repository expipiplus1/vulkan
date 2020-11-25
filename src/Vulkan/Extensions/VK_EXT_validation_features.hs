{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_validation_features"
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_VALIDATION_FEATURES_EXT))

-- No documentation found for TopLevel "VkValidationFeaturesEXT"
data ValidationFeaturesEXT = ValidationFeaturesEXT
  { -- No documentation found for Nested "VkValidationFeaturesEXT" "pEnabledValidationFeatures"
    enabledValidationFeatures :: Vector ValidationFeatureEnableEXT
  , -- No documentation found for Nested "VkValidationFeaturesEXT" "pDisabledValidationFeatures"
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


-- No documentation found for TopLevel "VkValidationFeatureEnableEXT"
newtype ValidationFeatureEnableEXT = ValidationFeatureEnableEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkValidationFeatureEnableEXT" "VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT"
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_EXT                      = ValidationFeatureEnableEXT 0
-- No documentation found for Nested "VkValidationFeatureEnableEXT" "VK_VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT"
pattern VALIDATION_FEATURE_ENABLE_GPU_ASSISTED_RESERVE_BINDING_SLOT_EXT = ValidationFeatureEnableEXT 1
-- No documentation found for Nested "VkValidationFeatureEnableEXT" "VK_VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT"
pattern VALIDATION_FEATURE_ENABLE_BEST_PRACTICES_EXT                    = ValidationFeatureEnableEXT 2
-- No documentation found for Nested "VkValidationFeatureEnableEXT" "VK_VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT"
pattern VALIDATION_FEATURE_ENABLE_DEBUG_PRINTF_EXT                      = ValidationFeatureEnableEXT 3
-- No documentation found for Nested "VkValidationFeatureEnableEXT" "VK_VALIDATION_FEATURE_ENABLE_SYNCHRONIZATION_VALIDATION_EXT"
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


-- No documentation found for TopLevel "VkValidationFeatureDisableEXT"
newtype ValidationFeatureDisableEXT = ValidationFeatureDisableEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkValidationFeatureDisableEXT" "VK_VALIDATION_FEATURE_DISABLE_ALL_EXT"
pattern VALIDATION_FEATURE_DISABLE_ALL_EXT              = ValidationFeatureDisableEXT 0
-- No documentation found for Nested "VkValidationFeatureDisableEXT" "VK_VALIDATION_FEATURE_DISABLE_SHADERS_EXT"
pattern VALIDATION_FEATURE_DISABLE_SHADERS_EXT          = ValidationFeatureDisableEXT 1
-- No documentation found for Nested "VkValidationFeatureDisableEXT" "VK_VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT"
pattern VALIDATION_FEATURE_DISABLE_THREAD_SAFETY_EXT    = ValidationFeatureDisableEXT 2
-- No documentation found for Nested "VkValidationFeatureDisableEXT" "VK_VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT"
pattern VALIDATION_FEATURE_DISABLE_API_PARAMETERS_EXT   = ValidationFeatureDisableEXT 3
-- No documentation found for Nested "VkValidationFeatureDisableEXT" "VK_VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT"
pattern VALIDATION_FEATURE_DISABLE_OBJECT_LIFETIMES_EXT = ValidationFeatureDisableEXT 4
-- No documentation found for Nested "VkValidationFeatureDisableEXT" "VK_VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT"
pattern VALIDATION_FEATURE_DISABLE_CORE_CHECKS_EXT      = ValidationFeatureDisableEXT 5
-- No documentation found for Nested "VkValidationFeatureDisableEXT" "VK_VALIDATION_FEATURE_DISABLE_UNIQUE_HANDLES_EXT"
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

