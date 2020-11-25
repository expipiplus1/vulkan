{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_multiview"
module Vulkan.Core11.Promoted_From_VK_KHR_multiview  ( PhysicalDeviceMultiviewFeatures(..)
                                                     , PhysicalDeviceMultiviewProperties(..)
                                                     , RenderPassMultiviewCreateInfo(..)
                                                     , StructureType(..)
                                                     , DependencyFlagBits(..)
                                                     , DependencyFlags
                                                     ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Data.Int (Int32)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlagBits(..))
import Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewFeatures"
data PhysicalDeviceMultiviewFeatures = PhysicalDeviceMultiviewFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiview"
    multiview :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiviewGeometryShader"
    multiviewGeometryShader :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewFeatures" "multiviewTessellationShader"
    multiviewTessellationShader :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultiviewFeatures)
#endif
deriving instance Show PhysicalDeviceMultiviewFeatures

instance ToCStruct PhysicalDeviceMultiviewFeatures where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (multiview))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (multiviewGeometryShader))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (multiviewTessellationShader))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMultiviewFeatures where
  peekCStruct p = do
    multiview <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    multiviewGeometryShader <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    multiviewTessellationShader <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceMultiviewFeatures
             (bool32ToBool multiview) (bool32ToBool multiviewGeometryShader) (bool32ToBool multiviewTessellationShader)


instance Storable PhysicalDeviceMultiviewFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewFeatures where
  zero = PhysicalDeviceMultiviewFeatures
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceMultiviewProperties"
data PhysicalDeviceMultiviewProperties = PhysicalDeviceMultiviewProperties
  { -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "maxMultiviewViewCount"
    maxMultiviewViewCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceMultiviewProperties" "maxMultiviewInstanceIndex"
    maxMultiviewInstanceIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMultiviewProperties)
#endif
deriving instance Show PhysicalDeviceMultiviewProperties

instance ToCStruct PhysicalDeviceMultiviewProperties where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMultiviewProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxMultiviewViewCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxMultiviewInstanceIndex)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceMultiviewProperties where
  peekCStruct p = do
    maxMultiviewViewCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxMultiviewInstanceIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PhysicalDeviceMultiviewProperties
             maxMultiviewViewCount maxMultiviewInstanceIndex


instance Storable PhysicalDeviceMultiviewProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMultiviewProperties where
  zero = PhysicalDeviceMultiviewProperties
           zero
           zero



-- No documentation found for TopLevel "VkRenderPassMultiviewCreateInfo"
data RenderPassMultiviewCreateInfo = RenderPassMultiviewCreateInfo
  { -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "pViewMasks"
    viewMasks :: Vector Word32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "pViewOffsets"
    viewOffsets :: Vector Int32
  , -- No documentation found for Nested "VkRenderPassMultiviewCreateInfo" "pCorrelationMasks"
    correlationMasks :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassMultiviewCreateInfo)
#endif
deriving instance Show RenderPassMultiviewCreateInfo

instance ToCStruct RenderPassMultiviewCreateInfo where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassMultiviewCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewMasks)) :: Word32))
    pPViewMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (viewMasks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (viewMasks)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPViewMasks')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (viewOffsets)) :: Word32))
    pPViewOffsets' <- ContT $ allocaBytesAligned @Int32 ((Data.Vector.length (viewOffsets)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewOffsets' `plusPtr` (4 * (i)) :: Ptr Int32) (e)) (viewOffsets)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Int32))) (pPViewOffsets')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (correlationMasks)) :: Word32))
    pPCorrelationMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (correlationMasks)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCorrelationMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (correlationMasks)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Word32))) (pPCorrelationMasks')
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPViewMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPViewMasks')
    pPViewOffsets' <- ContT $ allocaBytesAligned @Int32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPViewOffsets' `plusPtr` (4 * (i)) :: Ptr Int32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Int32))) (pPViewOffsets')
    pPCorrelationMasks' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPCorrelationMasks' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr Word32))) (pPCorrelationMasks')
    lift $ f

instance FromCStruct RenderPassMultiviewCreateInfo where
  peekCStruct p = do
    subpassCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pViewMasks <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pViewMasks' <- generateM (fromIntegral subpassCount) (\i -> peek @Word32 ((pViewMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    dependencyCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pViewOffsets <- peek @(Ptr Int32) ((p `plusPtr` 40 :: Ptr (Ptr Int32)))
    pViewOffsets' <- generateM (fromIntegral dependencyCount) (\i -> peek @Int32 ((pViewOffsets `advancePtrBytes` (4 * (i)) :: Ptr Int32)))
    correlationMaskCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pCorrelationMasks <- peek @(Ptr Word32) ((p `plusPtr` 56 :: Ptr (Ptr Word32)))
    pCorrelationMasks' <- generateM (fromIntegral correlationMaskCount) (\i -> peek @Word32 ((pCorrelationMasks `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ RenderPassMultiviewCreateInfo
             pViewMasks' pViewOffsets' pCorrelationMasks'

instance Zero RenderPassMultiviewCreateInfo where
  zero = RenderPassMultiviewCreateInfo
           mempty
           mempty
           mempty

