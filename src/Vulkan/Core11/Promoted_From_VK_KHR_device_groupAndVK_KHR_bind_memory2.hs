{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2"
module Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2  ( BindBufferMemoryDeviceGroupInfo(..)
                                                                              , BindImageMemoryDeviceGroupInfo(..)
                                                                              , StructureType(..)
                                                                              , ImageCreateFlagBits(..)
                                                                              , ImageCreateFlags
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
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))

-- No documentation found for TopLevel "VkBindBufferMemoryDeviceGroupInfo"
data BindBufferMemoryDeviceGroupInfo = BindBufferMemoryDeviceGroupInfo
  { -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "pDeviceIndices"
    deviceIndices :: Vector Word32 }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindBufferMemoryDeviceGroupInfo)
#endif
deriving instance Show BindBufferMemoryDeviceGroupInfo

instance ToCStruct BindBufferMemoryDeviceGroupInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindBufferMemoryDeviceGroupInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceIndices)) :: Word32))
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (deviceIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f

instance FromCStruct BindBufferMemoryDeviceGroupInfo where
  peekCStruct p = do
    deviceIndexCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pDeviceIndices' <- generateM (fromIntegral deviceIndexCount) (\i -> peek @Word32 ((pDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BindBufferMemoryDeviceGroupInfo
             pDeviceIndices'

instance Zero BindBufferMemoryDeviceGroupInfo where
  zero = BindBufferMemoryDeviceGroupInfo
           mempty



-- No documentation found for TopLevel "VkBindImageMemoryDeviceGroupInfo"
data BindImageMemoryDeviceGroupInfo = BindImageMemoryDeviceGroupInfo
  { -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "pDeviceIndices"
    deviceIndices :: Vector Word32
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "pSplitInstanceBindRegions"
    splitInstanceBindRegions :: Vector Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindImageMemoryDeviceGroupInfo)
#endif
deriving instance Show BindImageMemoryDeviceGroupInfo

instance ToCStruct BindImageMemoryDeviceGroupInfo where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindImageMemoryDeviceGroupInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceIndices)) :: Word32))
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (deviceIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceIndices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (splitInstanceBindRegions)) :: Word32))
    pPSplitInstanceBindRegions' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (splitInstanceBindRegions)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSplitInstanceBindRegions' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (splitInstanceBindRegions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Rect2D))) (pPSplitInstanceBindRegions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    pPSplitInstanceBindRegions' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (mempty)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSplitInstanceBindRegions' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Rect2D))) (pPSplitInstanceBindRegions')
    lift $ f

instance FromCStruct BindImageMemoryDeviceGroupInfo where
  peekCStruct p = do
    deviceIndexCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 24 :: Ptr (Ptr Word32)))
    pDeviceIndices' <- generateM (fromIntegral deviceIndexCount) (\i -> peek @Word32 ((pDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    splitInstanceBindRegionCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pSplitInstanceBindRegions <- peek @(Ptr Rect2D) ((p `plusPtr` 40 :: Ptr (Ptr Rect2D)))
    pSplitInstanceBindRegions' <- generateM (fromIntegral splitInstanceBindRegionCount) (\i -> peekCStruct @Rect2D ((pSplitInstanceBindRegions `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ BindImageMemoryDeviceGroupInfo
             pDeviceIndices' pSplitInstanceBindRegions'

instance Zero BindImageMemoryDeviceGroupInfo where
  zero = BindImageMemoryDeviceGroupInfo
           mempty
           mempty

