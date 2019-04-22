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
  , withCStructCooperativeMatrixPropertiesNV
  , fromCStructCooperativeMatrixPropertiesNV
  , CooperativeMatrixPropertiesNV(..)
  , withCStructPhysicalDeviceCooperativeMatrixFeaturesNV
  , fromCStructPhysicalDeviceCooperativeMatrixFeaturesNV
  , PhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , withCStructPhysicalDeviceCooperativeMatrixPropertiesNV
  , fromCStructPhysicalDeviceCooperativeMatrixPropertiesNV
  , PhysicalDeviceCooperativeMatrixPropertiesNV(..)
  , ScopeNV
  , pattern SCOPE_DEVICE_NV
  , pattern SCOPE_WORKGROUP_NV
  , pattern SCOPE_SUBGROUP_NV
  , pattern SCOPE_QUEUE_FAMILY_NV
  , getNumPhysicalDeviceCooperativeMatrixPropertiesNV
  , getPhysicalDeviceCooperativeMatrixPropertiesNV
  , getAllPhysicalDeviceCooperativeMatrixPropertiesNV
  , pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME
  , pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkComponentTypeNV(..)
  , VkCooperativeMatrixPropertiesNV(..)
  , VkPhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , VkPhysicalDeviceCooperativeMatrixPropertiesNV(..)
  , VkScopeNV(..)
  , vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
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
  , pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  )


-- | VkComponentTypeNV - Specify SPIR-V cooperative matrix component type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkCooperativeMatrixPropertiesNV'
type ComponentTypeNV = VkComponentTypeNV


{-# complete COMPONENT_TYPE_FLOAT16_NV, COMPONENT_TYPE_FLOAT32_NV, COMPONENT_TYPE_FLOAT64_NV, COMPONENT_TYPE_SINT8_NV, COMPONENT_TYPE_SINT16_NV, COMPONENT_TYPE_SINT32_NV, COMPONENT_TYPE_SINT64_NV, COMPONENT_TYPE_UINT8_NV, COMPONENT_TYPE_UINT16_NV, COMPONENT_TYPE_UINT32_NV, COMPONENT_TYPE_UINT64_NV :: ComponentTypeNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_FLOAT16_NV'
-- corresponds to SPIR-V @OpTypeFloat@ 16.
pattern COMPONENT_TYPE_FLOAT16_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_FLOAT16_NV = VK_COMPONENT_TYPE_FLOAT16_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_FLOAT32_NV'
-- corresponds to SPIR-V @OpTypeFloat@ 32.
pattern COMPONENT_TYPE_FLOAT32_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_FLOAT32_NV = VK_COMPONENT_TYPE_FLOAT32_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_FLOAT64_NV'
-- corresponds to SPIR-V @OpTypeFloat@ 64.
pattern COMPONENT_TYPE_FLOAT64_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_FLOAT64_NV = VK_COMPONENT_TYPE_FLOAT64_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_SINT8_NV'
-- corresponds to SPIR-V @OpTypeInt@ 8 1.
pattern COMPONENT_TYPE_SINT8_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_SINT8_NV = VK_COMPONENT_TYPE_SINT8_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_SINT16_NV'
-- corresponds to SPIR-V @OpTypeInt@ 16 1.
pattern COMPONENT_TYPE_SINT16_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_SINT16_NV = VK_COMPONENT_TYPE_SINT16_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_SINT32_NV'
-- corresponds to SPIR-V @OpTypeInt@ 32 1.
pattern COMPONENT_TYPE_SINT32_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_SINT32_NV = VK_COMPONENT_TYPE_SINT32_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_SINT64_NV'
-- corresponds to SPIR-V @OpTypeInt@ 64 1.
pattern COMPONENT_TYPE_SINT64_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_SINT64_NV = VK_COMPONENT_TYPE_SINT64_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_UINT8_NV'
-- corresponds to SPIR-V @OpTypeInt@ 8 0.
pattern COMPONENT_TYPE_UINT8_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_UINT8_NV = VK_COMPONENT_TYPE_UINT8_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_UINT16_NV'
-- corresponds to SPIR-V @OpTypeInt@ 16 0.
pattern COMPONENT_TYPE_UINT16_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_UINT16_NV = VK_COMPONENT_TYPE_UINT16_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_UINT32_NV'
-- corresponds to SPIR-V @OpTypeInt@ 32 0.
pattern COMPONENT_TYPE_UINT32_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_UINT32_NV = VK_COMPONENT_TYPE_UINT32_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_COMPONENT_TYPE_UINT64_NV'
-- corresponds to SPIR-V @OpTypeInt@ 64 0.
pattern COMPONENT_TYPE_UINT64_NV :: (a ~ ComponentTypeNV) => a
pattern COMPONENT_TYPE_UINT64_NV = VK_COMPONENT_TYPE_UINT64_NV


-- | VkCooperativeMatrixPropertiesNV - Structure specifying cooperative
-- matrix properties
--
-- = Description
--
-- If some types are preferred over other types (e.g. for performance),
-- they /should/ appear earlier in the list enumerated by
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.vkGetPhysicalDeviceCooperativeMatrixPropertiesNV'.
--
-- At least one entry in the list /must/ have power of two values for all
-- of @MSize@, @KSize@, and @NSize@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkComponentTypeNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkScopeNV',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.vkGetPhysicalDeviceCooperativeMatrixPropertiesNV'
data CooperativeMatrixPropertiesNV = CooperativeMatrixPropertiesNV
  { -- Univalued member elided
  -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkCooperativeMatrixPropertiesNV' and
-- marshal a 'CooperativeMatrixPropertiesNV' into it. The 'VkCooperativeMatrixPropertiesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCooperativeMatrixPropertiesNV :: CooperativeMatrixPropertiesNV -> (VkCooperativeMatrixPropertiesNV -> IO a) -> IO a
withCStructCooperativeMatrixPropertiesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CooperativeMatrixPropertiesNV)) (\pPNext -> cont (VkCooperativeMatrixPropertiesNV VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV pPNext (mSize (marshalled :: CooperativeMatrixPropertiesNV)) (nSize (marshalled :: CooperativeMatrixPropertiesNV)) (kSize (marshalled :: CooperativeMatrixPropertiesNV)) (aType (marshalled :: CooperativeMatrixPropertiesNV)) (bType (marshalled :: CooperativeMatrixPropertiesNV)) (cType (marshalled :: CooperativeMatrixPropertiesNV)) (dType (marshalled :: CooperativeMatrixPropertiesNV)) (scope (marshalled :: CooperativeMatrixPropertiesNV))))

-- | A function to read a 'VkCooperativeMatrixPropertiesNV' and all additional
-- structures in the pointer chain into a 'CooperativeMatrixPropertiesNV'.
fromCStructCooperativeMatrixPropertiesNV :: VkCooperativeMatrixPropertiesNV -> IO CooperativeMatrixPropertiesNV
fromCStructCooperativeMatrixPropertiesNV c = CooperativeMatrixPropertiesNV <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCooperativeMatrixPropertiesNV)))
                                                                           <*> pure (vkMSize (c :: VkCooperativeMatrixPropertiesNV))
                                                                           <*> pure (vkNSize (c :: VkCooperativeMatrixPropertiesNV))
                                                                           <*> pure (vkKSize (c :: VkCooperativeMatrixPropertiesNV))
                                                                           <*> pure (vkAType (c :: VkCooperativeMatrixPropertiesNV))
                                                                           <*> pure (vkBType (c :: VkCooperativeMatrixPropertiesNV))
                                                                           <*> pure (vkCType (c :: VkCooperativeMatrixPropertiesNV))
                                                                           <*> pure (vkDType (c :: VkCooperativeMatrixPropertiesNV))
                                                                           <*> pure (vkScope (c :: VkCooperativeMatrixPropertiesNV))

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



-- | VkPhysicalDeviceCooperativeMatrixFeaturesNV - Structure describing
-- cooperative matrix features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkPhysicalDeviceCooperativeMatrixFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkPhysicalDeviceCooperativeMatrixFeaturesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkPhysicalDeviceCooperativeMatrixFeaturesNV'
-- /can/ also be used in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceCooperativeMatrixFeaturesNV = PhysicalDeviceCooperativeMatrixFeaturesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrix"
  cooperativeMatrix :: Bool
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrixRobustBufferAccess"
  cooperativeMatrixRobustBufferAccess :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceCooperativeMatrixFeaturesNV' and
-- marshal a 'PhysicalDeviceCooperativeMatrixFeaturesNV' into it. The 'VkPhysicalDeviceCooperativeMatrixFeaturesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceCooperativeMatrixFeaturesNV :: PhysicalDeviceCooperativeMatrixFeaturesNV -> (VkPhysicalDeviceCooperativeMatrixFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceCooperativeMatrixFeaturesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceCooperativeMatrixFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceCooperativeMatrixFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV pPNext (boolToBool32 (cooperativeMatrix (marshalled :: PhysicalDeviceCooperativeMatrixFeaturesNV))) (boolToBool32 (cooperativeMatrixRobustBufferAccess (marshalled :: PhysicalDeviceCooperativeMatrixFeaturesNV)))))

-- | A function to read a 'VkPhysicalDeviceCooperativeMatrixFeaturesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceCooperativeMatrixFeaturesNV'.
fromCStructPhysicalDeviceCooperativeMatrixFeaturesNV :: VkPhysicalDeviceCooperativeMatrixFeaturesNV -> IO PhysicalDeviceCooperativeMatrixFeaturesNV
fromCStructPhysicalDeviceCooperativeMatrixFeaturesNV c = PhysicalDeviceCooperativeMatrixFeaturesNV <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceCooperativeMatrixFeaturesNV)))
                                                                                                   <*> pure (bool32ToBool (vkCooperativeMatrix (c :: VkPhysicalDeviceCooperativeMatrixFeaturesNV)))
                                                                                                   <*> pure (bool32ToBool (vkCooperativeMatrixRobustBufferAccess (c :: VkPhysicalDeviceCooperativeMatrixFeaturesNV)))

instance Zero PhysicalDeviceCooperativeMatrixFeaturesNV where
  zero = PhysicalDeviceCooperativeMatrixFeaturesNV Nothing
                                                   False
                                                   False



-- | VkPhysicalDeviceCooperativeMatrixPropertiesNV - Structure describing
-- cooperative matrix properties supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkPhysicalDeviceCooperativeMatrixPropertiesNV'
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkPhysicalDeviceCooperativeMatrixPropertiesNV'
-- structure is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceCooperativeMatrixPropertiesNV = PhysicalDeviceCooperativeMatrixPropertiesNV
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixPropertiesNV" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixPropertiesNV" "cooperativeMatrixSupportedStages"
  cooperativeMatrixSupportedStages :: ShaderStageFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceCooperativeMatrixPropertiesNV' and
-- marshal a 'PhysicalDeviceCooperativeMatrixPropertiesNV' into it. The 'VkPhysicalDeviceCooperativeMatrixPropertiesNV' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDeviceCooperativeMatrixPropertiesNV -> (VkPhysicalDeviceCooperativeMatrixPropertiesNV -> IO a) -> IO a
withCStructPhysicalDeviceCooperativeMatrixPropertiesNV marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceCooperativeMatrixPropertiesNV)) (\pPNext -> cont (VkPhysicalDeviceCooperativeMatrixPropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV pPNext (cooperativeMatrixSupportedStages (marshalled :: PhysicalDeviceCooperativeMatrixPropertiesNV))))

-- | A function to read a 'VkPhysicalDeviceCooperativeMatrixPropertiesNV' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceCooperativeMatrixPropertiesNV'.
fromCStructPhysicalDeviceCooperativeMatrixPropertiesNV :: VkPhysicalDeviceCooperativeMatrixPropertiesNV -> IO PhysicalDeviceCooperativeMatrixPropertiesNV
fromCStructPhysicalDeviceCooperativeMatrixPropertiesNV c = PhysicalDeviceCooperativeMatrixPropertiesNV <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceCooperativeMatrixPropertiesNV)))
                                                                                                       <*> pure (vkCooperativeMatrixSupportedStages (c :: VkPhysicalDeviceCooperativeMatrixPropertiesNV))

instance Zero PhysicalDeviceCooperativeMatrixPropertiesNV where
  zero = PhysicalDeviceCooperativeMatrixPropertiesNV Nothing
                                                     zero


-- | VkScopeNV - Specify SPIR-V scope
--
-- = Description
--
-- All enum values match the corresponding SPIR-V value.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkCooperativeMatrixPropertiesNV'
type ScopeNV = VkScopeNV


{-# complete SCOPE_DEVICE_NV, SCOPE_WORKGROUP_NV, SCOPE_SUBGROUP_NV, SCOPE_QUEUE_FAMILY_NV :: ScopeNV #-}


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_SCOPE_DEVICE_NV'
-- corresponds to SPIR-V
-- 'Graphics.Vulkan.Core10.DeviceInitialization.Device' scope.
pattern SCOPE_DEVICE_NV :: (a ~ ScopeNV) => a
pattern SCOPE_DEVICE_NV = VK_SCOPE_DEVICE_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_SCOPE_WORKGROUP_NV'
-- corresponds to SPIR-V @Workgroup@ scope.
pattern SCOPE_WORKGROUP_NV :: (a ~ ScopeNV) => a
pattern SCOPE_WORKGROUP_NV = VK_SCOPE_WORKGROUP_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_SCOPE_SUBGROUP_NV'
-- corresponds to SPIR-V @Subgroup@ scope.
pattern SCOPE_SUBGROUP_NV :: (a ~ ScopeNV) => a
pattern SCOPE_SUBGROUP_NV = VK_SCOPE_SUBGROUP_NV


-- | 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VK_SCOPE_QUEUE_FAMILY_NV'
-- corresponds to SPIR-V @QueueFamilyKHR@ scope.
pattern SCOPE_QUEUE_FAMILY_NV :: (a ~ ScopeNV) => a
pattern SCOPE_QUEUE_FAMILY_NV = VK_SCOPE_QUEUE_FAMILY_NV


-- | vkGetPhysicalDeviceCooperativeMatrixPropertiesNV - Returns properties
-- describing what cooperative matrix types are supported
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     cooperative matrix properties available or queried.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkCooperativeMatrixPropertiesNV'
--     structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of cooperative matrix
-- properties available is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If @pPropertyCount@ is less than the number of
-- cooperative matrix properties available, at most @pPropertyCount@
-- structures will be written. If @pPropertyCount@ is smaller than the
-- number of cooperative matrix properties available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available cooperative matrix properties were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkCooperativeMatrixPropertiesNV'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkCooperativeMatrixPropertiesNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getNumPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceCooperativeMatrixPropertiesNV = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pPropertyCount' -> vkGetPhysicalDeviceCooperativeMatrixPropertiesNV commandTable physicalDevice' pPropertyCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPropertyCount')))

-- | vkGetPhysicalDeviceCooperativeMatrixPropertiesNV - Returns properties
-- describing what cooperative matrix types are supported
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @pPropertyCount@ is a pointer to an integer related to the number of
--     cooperative matrix properties available or queried.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkCooperativeMatrixPropertiesNV'
--     structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of cooperative matrix
-- properties available is returned in @pPropertyCount@. Otherwise,
-- @pPropertyCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If @pPropertyCount@ is less than the number of
-- cooperative matrix properties available, at most @pPropertyCount@
-- structures will be written. If @pPropertyCount@ is smaller than the
-- number of cooperative matrix properties available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available cooperative matrix properties were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pPropertyCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pPropertyCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pPropertyCount@
--     'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkCooperativeMatrixPropertiesNV'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkCooperativeMatrixPropertiesNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector CooperativeMatrixPropertiesNV)
getPhysicalDeviceCooperativeMatrixPropertiesNV = \(PhysicalDevice physicalDevice' commandTable) -> \propertyCount' -> allocaArray (fromIntegral propertyCount') (\pProperties' -> with propertyCount' (\pPropertyCount' -> vkGetPhysicalDeviceCooperativeMatrixPropertiesNV commandTable physicalDevice' pPropertyCount' pProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructCooperativeMatrixPropertiesNV <=< peekElemOff p) pProperties') =<< (fromIntegral <$> (peek pPropertyCount')))))))
-- | Returns all the values available from 'getPhysicalDeviceCooperativeMatrixPropertiesNV'.
getAllPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  IO (Vector CooperativeMatrixPropertiesNV)
getAllPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice' =
  snd <$> getNumPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice'
    >>= \num -> snd <$> getPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice' num


-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME"
pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_COOPERATIVE_MATRIX_EXTENSION_NAME = VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION"
pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION :: Integral a => a
pattern NV_COOPERATIVE_MATRIX_SPEC_VERSION = VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION
