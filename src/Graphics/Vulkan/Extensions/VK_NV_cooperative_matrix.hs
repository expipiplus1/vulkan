{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix
  ( ComponentTypeNV
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
  , getNumPhysicalDeviceCooperativeMatrixPropertiesNV
  , getPhysicalDeviceCooperativeMatrixPropertiesNV
  , getAllPhysicalDeviceCooperativeMatrixPropertiesNV
  , pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION
  , pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
import qualified Graphics.Vulkan.C.Dynamic
  ( getPhysicalDeviceCooperativeMatrixPropertiesNV
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkComponentTypeNV(..)
  , VkCooperativeMatrixPropertiesNV(..)
  , VkPhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , VkPhysicalDeviceCooperativeMatrixPropertiesNV(..)
  , VkScopeNV(..)
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
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME
  , pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION
  )


-- No documentation found for TopLevel "ComponentTypeNV"
type ComponentTypeNV = VkComponentTypeNV
-- No documentation found for TopLevel "CooperativeMatrixPropertiesNV"
data CooperativeMatrixPropertiesNV = CooperativeMatrixPropertiesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "MSize"
  vkMSize :: Word32
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "NSize"
  vkNSize :: Word32
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "KSize"
  vkKSize :: Word32
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "AType"
  vkAType :: ComponentTypeNV
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "BType"
  vkBType :: ComponentTypeNV
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "CType"
  vkCType :: ComponentTypeNV
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "DType"
  vkDType :: ComponentTypeNV
  , -- No documentation found for Nested "CooperativeMatrixPropertiesNV" "scope"
  vkScope :: ScopeNV
  }
  deriving (Show, Eq)
withCStructCooperativeMatrixPropertiesNV :: CooperativeMatrixPropertiesNV -> (VkCooperativeMatrixPropertiesNV -> IO a) -> IO a
withCStructCooperativeMatrixPropertiesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: CooperativeMatrixPropertiesNV)) (\pPNext -> cont (VkCooperativeMatrixPropertiesNV VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV pPNext (vkMSize (from :: CooperativeMatrixPropertiesNV)) (vkNSize (from :: CooperativeMatrixPropertiesNV)) (vkKSize (from :: CooperativeMatrixPropertiesNV)) (vkAType (from :: CooperativeMatrixPropertiesNV)) (vkBType (from :: CooperativeMatrixPropertiesNV)) (vkCType (from :: CooperativeMatrixPropertiesNV)) (vkDType (from :: CooperativeMatrixPropertiesNV)) (vkScope (from :: CooperativeMatrixPropertiesNV))))
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
-- No documentation found for TopLevel "PhysicalDeviceCooperativeMatrixFeaturesNV"
data PhysicalDeviceCooperativeMatrixFeaturesNV = PhysicalDeviceCooperativeMatrixFeaturesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrix"
  vkCooperativeMatrix :: Bool
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrixRobustBufferAccess"
  vkCooperativeMatrixRobustBufferAccess :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceCooperativeMatrixFeaturesNV :: PhysicalDeviceCooperativeMatrixFeaturesNV -> (VkPhysicalDeviceCooperativeMatrixFeaturesNV -> IO a) -> IO a
withCStructPhysicalDeviceCooperativeMatrixFeaturesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceCooperativeMatrixFeaturesNV)) (\pPNext -> cont (VkPhysicalDeviceCooperativeMatrixFeaturesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV pPNext (boolToBool32 (vkCooperativeMatrix (from :: PhysicalDeviceCooperativeMatrixFeaturesNV))) (boolToBool32 (vkCooperativeMatrixRobustBufferAccess (from :: PhysicalDeviceCooperativeMatrixFeaturesNV)))))
fromCStructPhysicalDeviceCooperativeMatrixFeaturesNV :: VkPhysicalDeviceCooperativeMatrixFeaturesNV -> IO PhysicalDeviceCooperativeMatrixFeaturesNV
fromCStructPhysicalDeviceCooperativeMatrixFeaturesNV c = PhysicalDeviceCooperativeMatrixFeaturesNV <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceCooperativeMatrixFeaturesNV)))
                                                                                                   <*> pure (bool32ToBool (vkCooperativeMatrix (c :: VkPhysicalDeviceCooperativeMatrixFeaturesNV)))
                                                                                                   <*> pure (bool32ToBool (vkCooperativeMatrixRobustBufferAccess (c :: VkPhysicalDeviceCooperativeMatrixFeaturesNV)))
-- No documentation found for TopLevel "PhysicalDeviceCooperativeMatrixPropertiesNV"
data PhysicalDeviceCooperativeMatrixPropertiesNV = PhysicalDeviceCooperativeMatrixPropertiesNV
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixPropertiesNV" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceCooperativeMatrixPropertiesNV" "cooperativeMatrixSupportedStages"
  vkCooperativeMatrixSupportedStages :: ShaderStageFlags
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDeviceCooperativeMatrixPropertiesNV -> (VkPhysicalDeviceCooperativeMatrixPropertiesNV -> IO a) -> IO a
withCStructPhysicalDeviceCooperativeMatrixPropertiesNV from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceCooperativeMatrixPropertiesNV)) (\pPNext -> cont (VkPhysicalDeviceCooperativeMatrixPropertiesNV VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV pPNext (vkCooperativeMatrixSupportedStages (from :: PhysicalDeviceCooperativeMatrixPropertiesNV))))
fromCStructPhysicalDeviceCooperativeMatrixPropertiesNV :: VkPhysicalDeviceCooperativeMatrixPropertiesNV -> IO PhysicalDeviceCooperativeMatrixPropertiesNV
fromCStructPhysicalDeviceCooperativeMatrixPropertiesNV c = PhysicalDeviceCooperativeMatrixPropertiesNV <$> -- Univalued Member elided
                                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceCooperativeMatrixPropertiesNV)))
                                                                                                       <*> pure (vkCooperativeMatrixSupportedStages (c :: VkPhysicalDeviceCooperativeMatrixPropertiesNV))
-- No documentation found for TopLevel "ScopeNV"
type ScopeNV = VkScopeNV

-- | Wrapper for 'vkGetPhysicalDeviceCooperativeMatrixPropertiesNV'
getNumPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  IO (VkResult, Word32)
getNumPhysicalDeviceCooperativeMatrixPropertiesNV = \(PhysicalDevice physicalDevice commandTable) -> alloca (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceCooperativeMatrixPropertiesNV commandTable physicalDevice pPropertyCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for 'vkGetPhysicalDeviceCooperativeMatrixPropertiesNV'
getPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  Word32 ->  IO ( VkResult
, Vector CooperativeMatrixPropertiesNV )
getPhysicalDeviceCooperativeMatrixPropertiesNV = \(PhysicalDevice physicalDevice commandTable) -> \propertyCount -> allocaArray (fromIntegral propertyCount) (\pProperties -> with propertyCount (\pPropertyCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceCooperativeMatrixPropertiesNV commandTable physicalDevice pPropertyCount pProperties >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructCooperativeMatrixPropertiesNV <=< peekElemOff p) pProperties) =<< (fromIntegral <$> (peek pPropertyCount)))))))
-- | Call 'getNumPhysicalDeviceCooperativeMatrixPropertiesNV' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceCooperativeMatrixPropertiesNV' to get all the values.
getAllPhysicalDeviceCooperativeMatrixPropertiesNV :: PhysicalDevice ->  IO (Vector CooperativeMatrixPropertiesNV)
getAllPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice =
  snd <$> getNumPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice
    >>= \num -> snd <$> getPhysicalDeviceCooperativeMatrixPropertiesNV physicalDevice num

