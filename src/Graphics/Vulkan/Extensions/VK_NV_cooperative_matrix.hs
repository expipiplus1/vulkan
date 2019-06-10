{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix
  ( ComponentTypeNV
  , pattern COMPONENT_TYPE_FLOAT16_NV
  , pattern COMPONENT_TYPE_FLOAT32_NV
  , pattern COMPONENT_TYPE_FLOAT64_NV
  , pattern COMPONENT_TYPE_SINT8_NV
  , pattern COMPONENT_TYPE_SINT16_NV
  , pattern COMPONENT_TYPE_SINT32_NV
  , pattern COMPONENT_TYPE_SINT64_NV
  , pattern COMPONENT_TYPE_UINT8_NV
  , pattern COMPONENT_TYPE_UINT16_NV
  , pattern COMPONENT_TYPE_UINT32_NV
  , pattern COMPONENT_TYPE_UINT64_NV
#if defined(VK_USE_PLATFORM_GGP)
  , CooperativeMatrixPropertiesNV(..)
  , PhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , PhysicalDeviceCooperativeMatrixPropertiesNV(..)
#endif
  , ScopeNV
  , pattern SCOPE_DEVICE_NV
  , pattern SCOPE_WORKGROUP_NV
  , pattern SCOPE_SUBGROUP_NV
  , pattern SCOPE_QUEUE_FAMILY_NV
#if defined(VK_USE_PLATFORM_GGP)
  , getNumPhysicalDeviceCooperativeMatrixPropertiesNV
  , getPhysicalDeviceCooperativeMatrixPropertiesNV
  , getAllPhysicalDeviceCooperativeMatrixPropertiesNV
#endif
  , pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME
  , pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkComponentTypeNV(..)
  , VkScopeNV(..)
  , pattern VK_COMPONENT_TYPE_FLOAT16_NV
  , pattern VK_COMPONENT_TYPE_FLOAT32_NV
  , pattern VK_COMPONENT_TYPE_FLOAT64_NV
  , pattern VK_COMPONENT_TYPE_SINT16_NV
  , pattern VK_COMPONENT_TYPE_SINT32_NV
  , pattern VK_COMPONENT_TYPE_SINT64_NV
  , pattern VK_COMPONENT_TYPE_SINT8_NV
  , pattern VK_COMPONENT_TYPE_UINT16_NV
  , pattern VK_COMPONENT_TYPE_UINT32_NV
  , pattern VK_COMPONENT_TYPE_UINT64_NV
  , pattern VK_COMPONENT_TYPE_UINT8_NV
  , pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME
  , pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION
  , pattern VK_SCOPE_DEVICE_NV
  , pattern VK_SCOPE_QUEUE_FAMILY_NV
  , pattern VK_SCOPE_SUBGROUP_NV
  , pattern VK_SCOPE_WORKGROUP_NV
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  )


-- No documentation found for TopLevel "ComponentTypeNV"
type ComponentTypeNV = VkComponentTypeNV


{-# complete COMPONENT_TYPE_FLOAT16_NV, COMPONENT_TYPE_FLOAT32_NV, COMPONENT_TYPE_FLOAT64_NV, COMPONENT_TYPE_SINT8_NV, COMPONENT_TYPE_SINT16_NV, COMPONENT_TYPE_SINT32_NV, COMPONENT_TYPE_SINT64_NV, COMPONENT_TYPE_UINT8_NV, COMPONENT_TYPE_UINT16_NV, COMPONENT_TYPE_UINT32_NV, COMPONENT_TYPE_UINT64_NV :: ComponentTypeNV #-}


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_FLOAT16_NV"
pattern COMPONENT_TYPE_FLOAT16_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_FLOAT16_NV = VK_COMPONENT_TYPE_FLOAT16_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_FLOAT32_NV"
pattern COMPONENT_TYPE_FLOAT32_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_FLOAT32_NV = VK_COMPONENT_TYPE_FLOAT32_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_FLOAT64_NV"
pattern COMPONENT_TYPE_FLOAT64_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_FLOAT64_NV = VK_COMPONENT_TYPE_FLOAT64_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_SINT8_NV"
pattern COMPONENT_TYPE_SINT8_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_SINT8_NV = VK_COMPONENT_TYPE_SINT8_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_SINT16_NV"
pattern COMPONENT_TYPE_SINT16_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_SINT16_NV = VK_COMPONENT_TYPE_SINT16_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_SINT32_NV"
pattern COMPONENT_TYPE_SINT32_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_SINT32_NV = VK_COMPONENT_TYPE_SINT32_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_SINT64_NV"
pattern COMPONENT_TYPE_SINT64_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_SINT64_NV = VK_COMPONENT_TYPE_SINT64_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_UINT8_NV"
pattern COMPONENT_TYPE_UINT8_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_UINT8_NV = VK_COMPONENT_TYPE_UINT8_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_UINT16_NV"
pattern COMPONENT_TYPE_UINT16_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_UINT16_NV = VK_COMPONENT_TYPE_UINT16_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_UINT32_NV"
pattern COMPONENT_TYPE_UINT32_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_UINT32_NV = VK_COMPONENT_TYPE_UINT32_NV


-- No documentation found for Nested "ComponentTypeNV" "COMPONENT_TYPE_UINT64_NV"
pattern COMPONENT_TYPE_UINT64_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_UINT64_NV = VK_COMPONENT_TYPE_UINT64_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCooperativeMatrixPropertiesNV"
data CooperativeMatrixPropertiesNV = CooperativeMatrixPropertiesNV
  { -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "MSize"
  mSize :: Word32
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "NSize"
  nSize :: Word32
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "KSize"
  kSize :: Word32
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "AType"
  aType :: ComponentTypeNV
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "BType"
  bType :: ComponentTypeNV
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "CType"
  cType :: ComponentTypeNV
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "DType"
  dType :: ComponentTypeNV
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "scope"
  scope :: ScopeNV
  }
  deriving (Show, Eq)

instance Zero CooperativeMatrixPropertiesNV where
  zero = CooperativeMatrixPropertiesNV Nothing
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero
                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceCooperativeMatrixFeaturesNV"
data PhysicalDeviceCooperativeMatrixFeaturesNV = PhysicalDeviceCooperativeMatrixFeaturesNV
  { -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrix"
  cooperativeMatrix :: Bool
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrixRobustBufferAccess"
  cooperativeMatrixRobustBufferAccess :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceCooperativeMatrixFeaturesNV where
  zero = PhysicalDeviceCooperativeMatrixFeaturesNV Nothing
                                                   False
                                                   False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceCooperativeMatrixPropertiesNV"
data PhysicalDeviceCooperativeMatrixPropertiesNV = PhysicalDeviceCooperativeMatrixPropertiesNV
  { -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixPropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixPropertiesNV" "cooperativeMatrixSupportedStages"
  cooperativeMatrixSupportedStages :: ShaderStageFlags
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceCooperativeMatrixPropertiesNV where
  zero = PhysicalDeviceCooperativeMatrixPropertiesNV Nothing
                                                     zero

#endif

-- No documentation found for TopLevel "ScopeNV"
type ScopeNV = VkScopeNV


{-# complete SCOPE_DEVICE_NV, SCOPE_WORKGROUP_NV, SCOPE_SUBGROUP_NV, SCOPE_QUEUE_FAMILY_NV :: ScopeNV #-}


-- No documentation found for Nested "ScopeNV" "SCOPE_DEVICE_NV"
pattern SCOPE_DEVICE_NV :: (a ~ ScopeNV) => a
pattern SCOPE_DEVICE_NV = VK_SCOPE_DEVICE_NV


-- No documentation found for Nested "ScopeNV" "SCOPE_WORKGROUP_NV"
pattern SCOPE_WORKGROUP_NV :: (a ~ ScopeNV) => a
pattern SCOPE_WORKGROUP_NV = VK_SCOPE_WORKGROUP_NV


-- No documentation found for Nested "ScopeNV" "SCOPE_SUBGROUP_NV"
pattern SCOPE_SUBGROUP_NV :: (a ~ ScopeNV) => a
pattern SCOPE_SUBGROUP_NV = VK_SCOPE_SUBGROUP_NV


-- No documentation found for Nested "ScopeNV" "SCOPE_QUEUE_FAMILY_NV"
pattern SCOPE_QUEUE_FAMILY_NV :: (a ~ ScopeNV) => a
pattern SCOPE_QUEUE_FAMILY_NV = VK_SCOPE_QUEUE_FAMILY_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV"
getNumPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceCooperativeMatrixPropertiesNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV"
getPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector CooperativeMatrixPropertiesNV)
getPhysicalDeviceCooperativeMatrixPropertiesNV = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceCooperativeMatrixPropertiesNV'.
getAllPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  IO (Vector CooperativeMatrixPropertiesNV)
getAllPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice' =
  snd <$> getNumPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice' num

#endif

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME"
pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME = VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION"
pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION :: Integral a => a
pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION = VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION
