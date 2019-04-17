{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkComponentTypeNV(..)
  , pattern VK_COMPONENT_TYPE_FLOAT16_NV
  , pattern VK_COMPONENT_TYPE_FLOAT32_NV
  , pattern VK_COMPONENT_TYPE_FLOAT64_NV
  , pattern VK_COMPONENT_TYPE_SINT8_NV
  , pattern VK_COMPONENT_TYPE_SINT16_NV
  , pattern VK_COMPONENT_TYPE_SINT32_NV
  , pattern VK_COMPONENT_TYPE_SINT64_NV
  , pattern VK_COMPONENT_TYPE_UINT8_NV
  , pattern VK_COMPONENT_TYPE_UINT16_NV
  , pattern VK_COMPONENT_TYPE_UINT32_NV
  , pattern VK_COMPONENT_TYPE_UINT64_NV
  , VkCooperativeMatrixPropertiesNV(..)
  , VkPhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , VkPhysicalDeviceCooperativeMatrixPropertiesNV(..)
  , VkScopeNV(..)
  , pattern VK_SCOPE_DEVICE_NV
  , pattern VK_SCOPE_WORKGROUP_NV
  , pattern VK_SCOPE_SUBGROUP_NV
  , pattern VK_SCOPE_QUEUE_FAMILY_NV
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
#endif
  , FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  , PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  , pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME
  , pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
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
  ( FunPtr
  , Ptr
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkComponentTypeNV

-- | VkComponentTypeNV - Specify SPIR-V cooperative matrix component type
--
-- = See Also
--
-- No cross-references are available
newtype VkComponentTypeNV = VkComponentTypeNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkComponentTypeNV where
  showsPrec _ VK_COMPONENT_TYPE_FLOAT16_NV = showString "VK_COMPONENT_TYPE_FLOAT16_NV"
  showsPrec _ VK_COMPONENT_TYPE_FLOAT32_NV = showString "VK_COMPONENT_TYPE_FLOAT32_NV"
  showsPrec _ VK_COMPONENT_TYPE_FLOAT64_NV = showString "VK_COMPONENT_TYPE_FLOAT64_NV"
  showsPrec _ VK_COMPONENT_TYPE_SINT8_NV = showString "VK_COMPONENT_TYPE_SINT8_NV"
  showsPrec _ VK_COMPONENT_TYPE_SINT16_NV = showString "VK_COMPONENT_TYPE_SINT16_NV"
  showsPrec _ VK_COMPONENT_TYPE_SINT32_NV = showString "VK_COMPONENT_TYPE_SINT32_NV"
  showsPrec _ VK_COMPONENT_TYPE_SINT64_NV = showString "VK_COMPONENT_TYPE_SINT64_NV"
  showsPrec _ VK_COMPONENT_TYPE_UINT8_NV = showString "VK_COMPONENT_TYPE_UINT8_NV"
  showsPrec _ VK_COMPONENT_TYPE_UINT16_NV = showString "VK_COMPONENT_TYPE_UINT16_NV"
  showsPrec _ VK_COMPONENT_TYPE_UINT32_NV = showString "VK_COMPONENT_TYPE_UINT32_NV"
  showsPrec _ VK_COMPONENT_TYPE_UINT64_NV = showString "VK_COMPONENT_TYPE_UINT64_NV"
  showsPrec p (VkComponentTypeNV x) = showParen (p >= 11) (showString "VkComponentTypeNV " . showsPrec 11 x)

instance Read VkComponentTypeNV where
  readPrec = parens ( choose [ ("VK_COMPONENT_TYPE_FLOAT16_NV", pure VK_COMPONENT_TYPE_FLOAT16_NV)
                             , ("VK_COMPONENT_TYPE_FLOAT32_NV", pure VK_COMPONENT_TYPE_FLOAT32_NV)
                             , ("VK_COMPONENT_TYPE_FLOAT64_NV", pure VK_COMPONENT_TYPE_FLOAT64_NV)
                             , ("VK_COMPONENT_TYPE_SINT8_NV",   pure VK_COMPONENT_TYPE_SINT8_NV)
                             , ("VK_COMPONENT_TYPE_SINT16_NV",  pure VK_COMPONENT_TYPE_SINT16_NV)
                             , ("VK_COMPONENT_TYPE_SINT32_NV",  pure VK_COMPONENT_TYPE_SINT32_NV)
                             , ("VK_COMPONENT_TYPE_SINT64_NV",  pure VK_COMPONENT_TYPE_SINT64_NV)
                             , ("VK_COMPONENT_TYPE_UINT8_NV",   pure VK_COMPONENT_TYPE_UINT8_NV)
                             , ("VK_COMPONENT_TYPE_UINT16_NV",  pure VK_COMPONENT_TYPE_UINT16_NV)
                             , ("VK_COMPONENT_TYPE_UINT32_NV",  pure VK_COMPONENT_TYPE_UINT32_NV)
                             , ("VK_COMPONENT_TYPE_UINT64_NV",  pure VK_COMPONENT_TYPE_UINT64_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkComponentTypeNV")
                        v <- step readPrec
                        pure (VkComponentTypeNV v)
                        )
                    )

-- | @VK_COMPONENT_TYPE_FLOAT16_NV@ corresponds to SPIR-V @OpTypeFloat@ 16.
pattern VK_COMPONENT_TYPE_FLOAT16_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_FLOAT16_NV = VkComponentTypeNV 0

-- | @VK_COMPONENT_TYPE_FLOAT32_NV@ corresponds to SPIR-V @OpTypeFloat@ 32.
pattern VK_COMPONENT_TYPE_FLOAT32_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_FLOAT32_NV = VkComponentTypeNV 1

-- | @VK_COMPONENT_TYPE_FLOAT64_NV@ corresponds to SPIR-V @OpTypeFloat@ 64.
pattern VK_COMPONENT_TYPE_FLOAT64_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_FLOAT64_NV = VkComponentTypeNV 2

-- | @VK_COMPONENT_TYPE_SINT8_NV@ corresponds to SPIR-V @OpTypeInt@ 8 1.
pattern VK_COMPONENT_TYPE_SINT8_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_SINT8_NV = VkComponentTypeNV 3

-- | @VK_COMPONENT_TYPE_SINT16_NV@ corresponds to SPIR-V @OpTypeInt@ 16 1.
pattern VK_COMPONENT_TYPE_SINT16_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_SINT16_NV = VkComponentTypeNV 4

-- | @VK_COMPONENT_TYPE_SINT32_NV@ corresponds to SPIR-V @OpTypeInt@ 32 1.
pattern VK_COMPONENT_TYPE_SINT32_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_SINT32_NV = VkComponentTypeNV 5

-- | @VK_COMPONENT_TYPE_SINT64_NV@ corresponds to SPIR-V @OpTypeInt@ 64 1.
pattern VK_COMPONENT_TYPE_SINT64_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_SINT64_NV = VkComponentTypeNV 6

-- | @VK_COMPONENT_TYPE_UINT8_NV@ corresponds to SPIR-V @OpTypeInt@ 8 0.
pattern VK_COMPONENT_TYPE_UINT8_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_UINT8_NV = VkComponentTypeNV 7

-- | @VK_COMPONENT_TYPE_UINT16_NV@ corresponds to SPIR-V @OpTypeInt@ 16 0.
pattern VK_COMPONENT_TYPE_UINT16_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_UINT16_NV = VkComponentTypeNV 8

-- | @VK_COMPONENT_TYPE_UINT32_NV@ corresponds to SPIR-V @OpTypeInt@ 32 0.
pattern VK_COMPONENT_TYPE_UINT32_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_UINT32_NV = VkComponentTypeNV 9

-- | @VK_COMPONENT_TYPE_UINT64_NV@ corresponds to SPIR-V @OpTypeInt@ 64 0.
pattern VK_COMPONENT_TYPE_UINT64_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_UINT64_NV = VkComponentTypeNV 10
-- | VkCooperativeMatrixPropertiesNV - Structure specifying cooperative
-- matrix properties
--
-- = Description
--
-- If some types are preferred over other types (e.g. for performance),
-- they /should/ appear earlier in the list enumerated by
-- 'vkGetPhysicalDeviceCooperativeMatrixPropertiesNV'.
--
-- At least one entry in the list /must/ have power of two values for all
-- of @MSize@, @KSize@, and @NSize@.
--
-- Unresolved directive in VkCooperativeMatrixPropertiesNV.txt -
-- include::..\/validity\/structs\/VkCooperativeMatrixPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkCooperativeMatrixPropertiesNV = VkCooperativeMatrixPropertiesNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @MSize@ is the number of rows in matrices A, C, and D.
  vkMSize :: Word32
  , -- | @NSize@ is the number of columns in matrices B, C, D.
  vkNSize :: Word32
  , -- | @KSize@ is the number of columns in matrix A and rows in matrix B.
  vkKSize :: Word32
  , -- | @AType@ is the component type of matrix A, of type 'VkComponentTypeNV'.
  vkAType :: VkComponentTypeNV
  , -- | @BType@ is the component type of matrix B, of type 'VkComponentTypeNV'.
  vkBType :: VkComponentTypeNV
  , -- | @CType@ is the component type of matrix C, of type 'VkComponentTypeNV'.
  vkCType :: VkComponentTypeNV
  , -- | @DType@ is the component type of matrix D, of type 'VkComponentTypeNV'.
  vkDType :: VkComponentTypeNV
  , -- | @scope@ is the scope of all the matrix types, of type 'VkScopeNV'.
  vkScope :: VkScopeNV
  }
  deriving (Eq, Show)

instance Storable VkCooperativeMatrixPropertiesNV where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkCooperativeMatrixPropertiesNV <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 28)
                                             <*> peek (ptr `plusPtr` 32)
                                             <*> peek (ptr `plusPtr` 36)
                                             <*> peek (ptr `plusPtr` 40)
                                             <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 16) (vkMSize (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 20) (vkNSize (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 24) (vkKSize (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 28) (vkAType (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 32) (vkBType (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 36) (vkCType (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 40) (vkDType (poked :: VkCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 44) (vkScope (poked :: VkCooperativeMatrixPropertiesNV))

instance Zero VkCooperativeMatrixPropertiesNV where
  zero = VkCooperativeMatrixPropertiesNV zero
                                         zero
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
-- The members of the @VkPhysicalDeviceCooperativeMatrixFeaturesNV@
-- structure describe the following features:
--
-- = Description
--
-- If the @VkPhysicalDeviceCooperativeMatrixFeaturesNV@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether the feature is supported.
-- @VkPhysicalDeviceCooperativeMatrixFeaturesNV@ /can/ also be used in the
-- @pNext@ chain of 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to
-- enable features.
--
-- Unresolved directive in VkPhysicalDeviceCooperativeMatrixFeaturesNV.txt
-- -
-- include::..\/validity\/structs\/VkPhysicalDeviceCooperativeMatrixFeaturesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceCooperativeMatrixFeaturesNV = VkPhysicalDeviceCooperativeMatrixFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- | @cooperativeMatrix@ indicates that the implementation supports the
  -- @CooperativeMatrixNV@ SPIR-V capability.
  vkCooperativeMatrix :: VkBool32
  , -- | @cooperativeMatrixRobustBufferAccess@ indicates that the implementation
  -- supports robust buffer access for SPIR-V @OpCooperativeMatrixLoadNV@ and
  -- @OpCooperativeMatrixStoreNV@ instructions.
  vkCooperativeMatrixRobustBufferAccess :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceCooperativeMatrixFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceCooperativeMatrixFeaturesNV <$> peek (ptr `plusPtr` 0)
                                                         <*> peek (ptr `plusPtr` 8)
                                                         <*> peek (ptr `plusPtr` 16)
                                                         <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceCooperativeMatrixFeaturesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceCooperativeMatrixFeaturesNV))
                *> poke (ptr `plusPtr` 16) (vkCooperativeMatrix (poked :: VkPhysicalDeviceCooperativeMatrixFeaturesNV))
                *> poke (ptr `plusPtr` 20) (vkCooperativeMatrixRobustBufferAccess (poked :: VkPhysicalDeviceCooperativeMatrixFeaturesNV))

instance Zero VkPhysicalDeviceCooperativeMatrixFeaturesNV where
  zero = VkPhysicalDeviceCooperativeMatrixFeaturesNV zero
                                                     zero
                                                     zero
                                                     zero
-- | VkPhysicalDeviceCooperativeMatrixPropertiesNV - Structure describing
-- cooperative matrix properties supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceCooperativeMatrixPropertiesNV@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the @VkPhysicalDeviceCooperativeMatrixPropertiesNV@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in
-- VkPhysicalDeviceCooperativeMatrixPropertiesNV.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceCooperativeMatrixPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceCooperativeMatrixPropertiesNV = VkPhysicalDeviceCooperativeMatrixPropertiesNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @cooperativeMatrixSupportedStages@ is a bitfield of
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits' describing the
  -- shader stages that cooperative matrix instructions are supported in.
  -- @cooperativeMatrixSupportedStages@ will have the
  -- @VK_SHADER_STAGE_COMPUTE_BIT@ bit set if any of the physical device’s
  -- queues support @VK_QUEUE_COMPUTE_BIT@.
  vkCooperativeMatrixSupportedStages :: VkShaderStageFlags
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceCooperativeMatrixPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceCooperativeMatrixPropertiesNV <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceCooperativeMatrixPropertiesNV))
                *> poke (ptr `plusPtr` 16) (vkCooperativeMatrixSupportedStages (poked :: VkPhysicalDeviceCooperativeMatrixPropertiesNV))

instance Zero VkPhysicalDeviceCooperativeMatrixPropertiesNV where
  zero = VkPhysicalDeviceCooperativeMatrixPropertiesNV zero
                                                       zero
                                                       zero
-- ** VkScopeNV

-- | VkScopeNV - Specify SPIR-V scope
--
-- = Description
--
-- All enum values match the corresponding SPIR-V value.
--
-- = See Also
--
-- No cross-references are available
newtype VkScopeNV = VkScopeNV Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkScopeNV where
  showsPrec _ VK_SCOPE_DEVICE_NV = showString "VK_SCOPE_DEVICE_NV"
  showsPrec _ VK_SCOPE_WORKGROUP_NV = showString "VK_SCOPE_WORKGROUP_NV"
  showsPrec _ VK_SCOPE_SUBGROUP_NV = showString "VK_SCOPE_SUBGROUP_NV"
  showsPrec _ VK_SCOPE_QUEUE_FAMILY_NV = showString "VK_SCOPE_QUEUE_FAMILY_NV"
  showsPrec p (VkScopeNV x) = showParen (p >= 11) (showString "VkScopeNV " . showsPrec 11 x)

instance Read VkScopeNV where
  readPrec = parens ( choose [ ("VK_SCOPE_DEVICE_NV",       pure VK_SCOPE_DEVICE_NV)
                             , ("VK_SCOPE_WORKGROUP_NV",    pure VK_SCOPE_WORKGROUP_NV)
                             , ("VK_SCOPE_SUBGROUP_NV",     pure VK_SCOPE_SUBGROUP_NV)
                             , ("VK_SCOPE_QUEUE_FAMILY_NV", pure VK_SCOPE_QUEUE_FAMILY_NV)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkScopeNV")
                        v <- step readPrec
                        pure (VkScopeNV v)
                        )
                    )

-- | @VK_SCOPE_DEVICE_NV@ corresponds to SPIR-V @Device@ scope.
pattern VK_SCOPE_DEVICE_NV :: VkScopeNV
pattern VK_SCOPE_DEVICE_NV = VkScopeNV 1

-- | @VK_SCOPE_WORKGROUP_NV@ corresponds to SPIR-V @Workgroup@ scope.
pattern VK_SCOPE_WORKGROUP_NV :: VkScopeNV
pattern VK_SCOPE_WORKGROUP_NV = VkScopeNV 2

-- | @VK_SCOPE_SUBGROUP_NV@ corresponds to SPIR-V @Subgroup@ scope.
pattern VK_SCOPE_SUBGROUP_NV :: VkScopeNV
pattern VK_SCOPE_SUBGROUP_NV = VkScopeNV 3

-- | @VK_SCOPE_QUEUE_FAMILY_NV@ corresponds to SPIR-V @QueueFamilyKHR@ scope.
pattern VK_SCOPE_QUEUE_FAMILY_NV :: VkScopeNV
pattern VK_SCOPE_QUEUE_FAMILY_NV = VkScopeNV 5
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
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
--     'VkCooperativeMatrixPropertiesNV' structures.
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
-- number of cooperative matrix properties available, @VK_INCOMPLETE@ will
-- be returned instead of @VK_SUCCESS@, to indicate that not all the
-- available cooperative matrix properties were returned.
--
-- Unresolved directive in
-- vkGetPhysicalDeviceCooperativeMatrixPropertiesNV.txt -
-- include::..\/validity\/protos\/vkGetPhysicalDeviceCooperativeMatrixPropertiesNV.txt[]
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV" vkGetPhysicalDeviceCooperativeMatrixPropertiesNV :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkCooperativeMatrixPropertiesNV) -> IO VkResult

#endif
type FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkCooperativeMatrixPropertiesNV) -> IO VkResult
type PFN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV = FunPtr FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME"
pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_COOPERATIVE_MATRIX_EXTENSION_NAME = "VK_NV_cooperative_matrix"
-- No documentation found for TopLevel "VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION"
pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION :: Integral a => a
pattern VK_NV_COOPERATIVE_MATRIX_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV"
pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV = VkStructureType 1000249001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV = VkStructureType 1000249000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV = VkStructureType 1000249002
