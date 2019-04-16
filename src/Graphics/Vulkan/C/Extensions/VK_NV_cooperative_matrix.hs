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

-- No documentation found for TopLevel "VkComponentTypeNV"
newtype VkComponentTypeNV = VkComponentTypeNV Int32
  deriving (Eq, Ord, Storable)

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

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_FLOAT16_NV"
pattern VK_COMPONENT_TYPE_FLOAT16_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_FLOAT16_NV = VkComponentTypeNV 0

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_FLOAT32_NV"
pattern VK_COMPONENT_TYPE_FLOAT32_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_FLOAT32_NV = VkComponentTypeNV 1

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_FLOAT64_NV"
pattern VK_COMPONENT_TYPE_FLOAT64_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_FLOAT64_NV = VkComponentTypeNV 2

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_SINT8_NV"
pattern VK_COMPONENT_TYPE_SINT8_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_SINT8_NV = VkComponentTypeNV 3

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_SINT16_NV"
pattern VK_COMPONENT_TYPE_SINT16_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_SINT16_NV = VkComponentTypeNV 4

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_SINT32_NV"
pattern VK_COMPONENT_TYPE_SINT32_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_SINT32_NV = VkComponentTypeNV 5

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_SINT64_NV"
pattern VK_COMPONENT_TYPE_SINT64_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_SINT64_NV = VkComponentTypeNV 6

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_UINT8_NV"
pattern VK_COMPONENT_TYPE_UINT8_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_UINT8_NV = VkComponentTypeNV 7

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_UINT16_NV"
pattern VK_COMPONENT_TYPE_UINT16_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_UINT16_NV = VkComponentTypeNV 8

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_UINT32_NV"
pattern VK_COMPONENT_TYPE_UINT32_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_UINT32_NV = VkComponentTypeNV 9

-- No documentation found for Nested "VkComponentTypeNV" "VK_COMPONENT_TYPE_UINT64_NV"
pattern VK_COMPONENT_TYPE_UINT64_NV :: VkComponentTypeNV
pattern VK_COMPONENT_TYPE_UINT64_NV = VkComponentTypeNV 10
-- No documentation found for TopLevel "VkCooperativeMatrixPropertiesNV"
data VkCooperativeMatrixPropertiesNV = VkCooperativeMatrixPropertiesNV
  { -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "MSize"
  vkMSize :: Word32
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "NSize"
  vkNSize :: Word32
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "KSize"
  vkKSize :: Word32
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "AType"
  vkAType :: VkComponentTypeNV
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "BType"
  vkBType :: VkComponentTypeNV
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "CType"
  vkCType :: VkComponentTypeNV
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "DType"
  vkDType :: VkComponentTypeNV
  , -- No documentation found for Nested "VkCooperativeMatrixPropertiesNV" "scope"
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
-- No documentation found for TopLevel "VkPhysicalDeviceCooperativeMatrixFeaturesNV"
data VkPhysicalDeviceCooperativeMatrixFeaturesNV = VkPhysicalDeviceCooperativeMatrixFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrix"
  vkCooperativeMatrix :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixFeaturesNV" "cooperativeMatrixRobustBufferAccess"
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
-- No documentation found for TopLevel "VkPhysicalDeviceCooperativeMatrixPropertiesNV"
data VkPhysicalDeviceCooperativeMatrixPropertiesNV = VkPhysicalDeviceCooperativeMatrixPropertiesNV
  { -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixPropertiesNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixPropertiesNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceCooperativeMatrixPropertiesNV" "cooperativeMatrixSupportedStages"
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
-- ** VkScopeNV

-- No documentation found for TopLevel "VkScopeNV"
newtype VkScopeNV = VkScopeNV Int32
  deriving (Eq, Ord, Storable)

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

-- No documentation found for Nested "VkScopeNV" "VK_SCOPE_DEVICE_NV"
pattern VK_SCOPE_DEVICE_NV :: VkScopeNV
pattern VK_SCOPE_DEVICE_NV = VkScopeNV 1

-- No documentation found for Nested "VkScopeNV" "VK_SCOPE_WORKGROUP_NV"
pattern VK_SCOPE_WORKGROUP_NV :: VkScopeNV
pattern VK_SCOPE_WORKGROUP_NV = VkScopeNV 2

-- No documentation found for Nested "VkScopeNV" "VK_SCOPE_SUBGROUP_NV"
pattern VK_SCOPE_SUBGROUP_NV :: VkScopeNV
pattern VK_SCOPE_SUBGROUP_NV = VkScopeNV 3

-- No documentation found for Nested "VkScopeNV" "VK_SCOPE_QUEUE_FAMILY_NV"
pattern VK_SCOPE_QUEUE_FAMILY_NV :: VkScopeNV
pattern VK_SCOPE_QUEUE_FAMILY_NV = VkScopeNV 5
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- No documentation found for TopLevel "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV"
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
