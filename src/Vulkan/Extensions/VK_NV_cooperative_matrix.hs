{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_cooperative_matrix"
module Vulkan.Extensions.VK_NV_cooperative_matrix  ( getPhysicalDeviceCooperativeMatrixPropertiesNV
                                                   , PhysicalDeviceCooperativeMatrixFeaturesNV(..)
                                                   , PhysicalDeviceCooperativeMatrixPropertiesNV(..)
                                                   , CooperativeMatrixPropertiesNV(..)
                                                   , ScopeNV( SCOPE_DEVICE_NV
                                                            , SCOPE_WORKGROUP_NV
                                                            , SCOPE_SUBGROUP_NV
                                                            , SCOPE_QUEUE_FAMILY_NV
                                                            , ..
                                                            )
                                                   , ComponentTypeNV( COMPONENT_TYPE_FLOAT16_NV
                                                                    , COMPONENT_TYPE_FLOAT32_NV
                                                                    , COMPONENT_TYPE_FLOAT64_NV
                                                                    , COMPONENT_TYPE_SINT8_NV
                                                                    , COMPONENT_TYPE_SINT16_NV
                                                                    , COMPONENT_TYPE_SINT32_NV
                                                                    , COMPONENT_TYPE_SINT64_NV
                                                                    , COMPONENT_TYPE_UINT8_NV
                                                                    , COMPONENT_TYPE_UINT16_NV
                                                                    , COMPONENT_TYPE_UINT32_NV
                                                                    , COMPONENT_TYPE_UINT64_NV
                                                                    , ..
                                                                    )
                                                   , NV_COOPERATIVE_MATRIX_SPEC_VERSION
                                                   , pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION
                                                   , NV_COOPERATIVE_MATRIX_EXTENSION_NAME
                                                   , pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME
                                                   ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCooperativeMatrixPropertiesNV))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeMatrixPropertiesNV -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr CooperativeMatrixPropertiesNV -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV"
getPhysicalDeviceCooperativeMatrixPropertiesNV :: forall io
                                                . (MonadIO io)
                                               => -- No documentation found for Nested "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV" "physicalDevice"
                                                  PhysicalDevice
                                               -> io (Result, ("properties" ::: Vector CooperativeMatrixPropertiesNV))
getPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCooperativeMatrixPropertiesNVPtr = pVkGetPhysicalDeviceCooperativeMatrixPropertiesNV (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceCooperativeMatrixPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCooperativeMatrixPropertiesNV is null" Nothing Nothing
  let vkGetPhysicalDeviceCooperativeMatrixPropertiesNV' = mkVkGetPhysicalDeviceCooperativeMatrixPropertiesNV vkGetPhysicalDeviceCooperativeMatrixPropertiesNVPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPPropertyCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceCooperativeMatrixPropertiesNV' physicalDevice' (pPPropertyCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPropertyCount <- lift $ peek @Word32 pPPropertyCount
  pPProperties <- ContT $ bracket (callocBytes @CooperativeMatrixPropertiesNV ((fromIntegral (pPropertyCount)) * 48)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 48) :: Ptr CooperativeMatrixPropertiesNV) . ($ ())) [0..(fromIntegral (pPropertyCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceCooperativeMatrixPropertiesNV' physicalDevice' (pPPropertyCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPropertyCount' <- lift $ peek @Word32 pPPropertyCount
  pProperties' <- lift $ generateM (fromIntegral (pPropertyCount')) (\i -> peekCStruct @CooperativeMatrixPropertiesNV (((pPProperties) `advancePtrBytes` (48 * (i)) :: Ptr CooperativeMatrixPropertiesNV)))
  pure $ ((r'), pProperties')



-- No documentation found for TopLevel "VkPhysicalDeviceCooperativeMatrixFeaturesNV"
data PhysicalDeviceCooperativeMatrixFeaturesNV = PhysicalDeviceCooperativeMatrixFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrix"
    cooperativeMatrix :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrixRobustBufferAccess"
    cooperativeMatrixRobustBufferAccess :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeMatrixFeaturesNV)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrixFeaturesNV

instance ToCStruct PhysicalDeviceCooperativeMatrixFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrixFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrix))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (cooperativeMatrixRobustBufferAccess))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCooperativeMatrixFeaturesNV where
  peekCStruct p = do
    cooperativeMatrix <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    cooperativeMatrixRobustBufferAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceCooperativeMatrixFeaturesNV
             (bool32ToBool cooperativeMatrix) (bool32ToBool cooperativeMatrixRobustBufferAccess)


instance Storable PhysicalDeviceCooperativeMatrixFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrixFeaturesNV where
  zero = PhysicalDeviceCooperativeMatrixFeaturesNV
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceCooperativeMatrixPropertiesNV"
data PhysicalDeviceCooperativeMatrixPropertiesNV = PhysicalDeviceCooperativeMatrixPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixPropertiesNV" "cooperativeMatrixSupportedStages"
    cooperativeMatrixSupportedStages :: ShaderStageFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCooperativeMatrixPropertiesNV)
#endif
deriving instance Show PhysicalDeviceCooperativeMatrixPropertiesNV

instance ToCStruct PhysicalDeviceCooperativeMatrixPropertiesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCooperativeMatrixPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (cooperativeMatrixSupportedStages)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceCooperativeMatrixPropertiesNV where
  peekCStruct p = do
    cooperativeMatrixSupportedStages <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    pure $ PhysicalDeviceCooperativeMatrixPropertiesNV
             cooperativeMatrixSupportedStages


instance Storable PhysicalDeviceCooperativeMatrixPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCooperativeMatrixPropertiesNV where
  zero = PhysicalDeviceCooperativeMatrixPropertiesNV
           zero



-- No documentation found for TopLevel "VkCooperativeMatrixPropertiesNV"
data CooperativeMatrixPropertiesNV = CooperativeMatrixPropertiesNV
  { -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "MSize"
    mSize :: Word32
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "NSize"
    nSize :: Word32
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "KSize"
    kSize :: Word32
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "AType"
    aType :: ComponentTypeNV
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "BType"
    bType :: ComponentTypeNV
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "CType"
    cType :: ComponentTypeNV
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "DType"
    dType :: ComponentTypeNV
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "scope"
    scope :: ScopeNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CooperativeMatrixPropertiesNV)
#endif
deriving instance Show CooperativeMatrixPropertiesNV

instance ToCStruct CooperativeMatrixPropertiesNV where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CooperativeMatrixPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (mSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (nSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (kSize)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeNV)) (aType)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeNV)) (bType)
    poke ((p `plusPtr` 36 :: Ptr ComponentTypeNV)) (cType)
    poke ((p `plusPtr` 40 :: Ptr ComponentTypeNV)) (dType)
    poke ((p `plusPtr` 44 :: Ptr ScopeNV)) (scope)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ComponentTypeNV)) (zero)
    poke ((p `plusPtr` 32 :: Ptr ComponentTypeNV)) (zero)
    poke ((p `plusPtr` 36 :: Ptr ComponentTypeNV)) (zero)
    poke ((p `plusPtr` 40 :: Ptr ComponentTypeNV)) (zero)
    poke ((p `plusPtr` 44 :: Ptr ScopeNV)) (zero)
    f

instance FromCStruct CooperativeMatrixPropertiesNV where
  peekCStruct p = do
    mSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    nSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    kSize <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    aType <- peek @ComponentTypeNV ((p `plusPtr` 28 :: Ptr ComponentTypeNV))
    bType <- peek @ComponentTypeNV ((p `plusPtr` 32 :: Ptr ComponentTypeNV))
    cType <- peek @ComponentTypeNV ((p `plusPtr` 36 :: Ptr ComponentTypeNV))
    dType <- peek @ComponentTypeNV ((p `plusPtr` 40 :: Ptr ComponentTypeNV))
    scope <- peek @ScopeNV ((p `plusPtr` 44 :: Ptr ScopeNV))
    pure $ CooperativeMatrixPropertiesNV
             mSize nSize kSize aType bType cType dType scope


instance Storable CooperativeMatrixPropertiesNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CooperativeMatrixPropertiesNV where
  zero = CooperativeMatrixPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- No documentation found for TopLevel "VkScopeNV"
newtype ScopeNV = ScopeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkScopeNV" "VK_SCOPE_DEVICE_NV"
pattern SCOPE_DEVICE_NV       = ScopeNV 1
-- No documentation found for Nested "VkScopeNV" "VK_SCOPE_WORKGROUP_NV"
pattern SCOPE_WORKGROUP_NV    = ScopeNV 2
-- No documentation found for Nested "VkScopeNV" "VK_SCOPE_SUBGROUP_NV"
pattern SCOPE_SUBGROUP_NV     = ScopeNV 3
-- No documentation found for Nested "VkScopeNV" "VK_SCOPE_QUEUE_FAMILY_NV"
pattern SCOPE_QUEUE_FAMILY_NV = ScopeNV 5
{-# complete SCOPE_DEVICE_NV,
             SCOPE_WORKGROUP_NV,
             SCOPE_SUBGROUP_NV,
             SCOPE_QUEUE_FAMILY_NV :: ScopeNV #-}

conNameScopeNV :: String
conNameScopeNV = "ScopeNV"

enumPrefixScopeNV :: String
enumPrefixScopeNV = "SCOPE_"

showTableScopeNV :: [(ScopeNV, String)]
showTableScopeNV =
  [ (SCOPE_DEVICE_NV      , "DEVICE_NV")
  , (SCOPE_WORKGROUP_NV   , "WORKGROUP_NV")
  , (SCOPE_SUBGROUP_NV    , "SUBGROUP_NV")
  , (SCOPE_QUEUE_FAMILY_NV, "QUEUE_FAMILY_NV")
  ]


instance Show ScopeNV where
showsPrec = enumShowsPrec enumPrefixScopeNV showTableScopeNV conNameScopeNV (\(ScopeNV x) -> x) (showsPrec 11)


instance Read ScopeNV where
  readPrec = enumReadPrec enumPrefixScopeNV showTableScopeNV conNameScopeNV ScopeNV


-- No documentation found for TopLevel "VkComponentTypeNV"
newtype ComponentTypeNV = ComponentTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_FLOAT16_NV"
pattern COMPONENT_TYPE_FLOAT16_NV = ComponentTypeNV 0
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_FLOAT32_NV"
pattern COMPONENT_TYPE_FLOAT32_NV = ComponentTypeNV 1
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_FLOAT64_NV"
pattern COMPONENT_TYPE_FLOAT64_NV = ComponentTypeNV 2
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_SINT8_NV"
pattern COMPONENT_TYPE_SINT8_NV   = ComponentTypeNV 3
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_SINT16_NV"
pattern COMPONENT_TYPE_SINT16_NV  = ComponentTypeNV 4
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_SINT32_NV"
pattern COMPONENT_TYPE_SINT32_NV  = ComponentTypeNV 5
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_SINT64_NV"
pattern COMPONENT_TYPE_SINT64_NV  = ComponentTypeNV 6
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_UINT8_NV"
pattern COMPONENT_TYPE_UINT8_NV   = ComponentTypeNV 7
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_UINT16_NV"
pattern COMPONENT_TYPE_UINT16_NV  = ComponentTypeNV 8
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_UINT32_NV"
pattern COMPONENT_TYPE_UINT32_NV  = ComponentTypeNV 9
-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_UINT64_NV"
pattern COMPONENT_TYPE_UINT64_NV  = ComponentTypeNV 10
{-# complete COMPONENT_TYPE_FLOAT16_NV,
             COMPONENT_TYPE_FLOAT32_NV,
             COMPONENT_TYPE_FLOAT64_NV,
             COMPONENT_TYPE_SINT8_NV,
             COMPONENT_TYPE_SINT16_NV,
             COMPONENT_TYPE_SINT32_NV,
             COMPONENT_TYPE_SINT64_NV,
             COMPONENT_TYPE_UINT8_NV,
             COMPONENT_TYPE_UINT16_NV,
             COMPONENT_TYPE_UINT32_NV,
             COMPONENT_TYPE_UINT64_NV :: ComponentTypeNV #-}

conNameComponentTypeNV :: String
conNameComponentTypeNV = "ComponentTypeNV"

enumPrefixComponentTypeNV :: String
enumPrefixComponentTypeNV = "COMPONENT_TYPE_"

showTableComponentTypeNV :: [(ComponentTypeNV, String)]
showTableComponentTypeNV =
  [ (COMPONENT_TYPE_FLOAT16_NV, "FLOAT16_NV")
  , (COMPONENT_TYPE_FLOAT32_NV, "FLOAT32_NV")
  , (COMPONENT_TYPE_FLOAT64_NV, "FLOAT64_NV")
  , (COMPONENT_TYPE_SINT8_NV  , "SINT8_NV")
  , (COMPONENT_TYPE_SINT16_NV , "SINT16_NV")
  , (COMPONENT_TYPE_SINT32_NV , "SINT32_NV")
  , (COMPONENT_TYPE_SINT64_NV , "SINT64_NV")
  , (COMPONENT_TYPE_UINT8_NV  , "UINT8_NV")
  , (COMPONENT_TYPE_UINT16_NV , "UINT16_NV")
  , (COMPONENT_TYPE_UINT32_NV , "UINT32_NV")
  , (COMPONENT_TYPE_UINT64_NV , "UINT64_NV")
  ]


instance Show ComponentTypeNV where
showsPrec = enumShowsPrec enumPrefixComponentTypeNV
                          showTableComponentTypeNV
                          conNameComponentTypeNV
                          (\(ComponentTypeNV x) -> x)
                          (showsPrec 11)


instance Read ComponentTypeNV where
  readPrec = enumReadPrec enumPrefixComponentTypeNV showTableComponentTypeNV conNameComponentTypeNV ComponentTypeNV


type NV_COOPERATIVE_MATRIX_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION"
pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION :: forall a . Integral a => a
pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION = 1


type NV_COOPERATIVE_MATRIX_EXTENSION_NAME = "VK_NV_cooperative_matrix"

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME"
pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME = "VK_NV_cooperative_matrix"

