{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_device_group_creation"
module Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation  ( enumeratePhysicalDeviceGroups
                                                                 , PhysicalDeviceGroupProperties(..)
                                                                 , DeviceGroupDeviceCreateInfo(..)
                                                                 , StructureType(..)
                                                                 , MemoryHeapFlagBits(..)
                                                                 , MemoryHeapFlags
                                                                 , MAX_DEVICE_GROUP_SIZE
                                                                 , pattern MAX_DEVICE_GROUP_SIZE
                                                                 ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkEnumeratePhysicalDeviceGroups))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.APIConstants (MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.APIConstants (MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlagBits(..))
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceGroups
  :: FunPtr (Ptr Instance_T -> Ptr Word32 -> Ptr PhysicalDeviceGroupProperties -> IO Result) -> Ptr Instance_T -> Ptr Word32 -> Ptr PhysicalDeviceGroupProperties -> IO Result

-- No documentation found for TopLevel "vkEnumeratePhysicalDeviceGroups"
enumeratePhysicalDeviceGroups :: forall io
                               . (MonadIO io)
                              => -- No documentation found for Nested "vkEnumeratePhysicalDeviceGroups" "instance"
                                 Instance
                              -> io (Result, ("physicalDeviceGroupProperties" ::: Vector PhysicalDeviceGroupProperties))
enumeratePhysicalDeviceGroups instance' = liftIO . evalContT $ do
  let vkEnumeratePhysicalDeviceGroupsPtr = pVkEnumeratePhysicalDeviceGroups (instanceCmds (instance' :: Instance))
  lift $ unless (vkEnumeratePhysicalDeviceGroupsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkEnumeratePhysicalDeviceGroups is null" Nothing Nothing
  let vkEnumeratePhysicalDeviceGroups' = mkVkEnumeratePhysicalDeviceGroups vkEnumeratePhysicalDeviceGroupsPtr
  let instance'' = instanceHandle (instance')
  pPPhysicalDeviceGroupCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkEnumeratePhysicalDeviceGroups' instance'' (pPPhysicalDeviceGroupCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPhysicalDeviceGroupCount <- lift $ peek @Word32 pPPhysicalDeviceGroupCount
  pPPhysicalDeviceGroupProperties <- ContT $ bracket (callocBytes @PhysicalDeviceGroupProperties ((fromIntegral (pPhysicalDeviceGroupCount)) * 288)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPPhysicalDeviceGroupProperties `advancePtrBytes` (i * 288) :: Ptr PhysicalDeviceGroupProperties) . ($ ())) [0..(fromIntegral (pPhysicalDeviceGroupCount)) - 1]
  r' <- lift $ vkEnumeratePhysicalDeviceGroups' instance'' (pPPhysicalDeviceGroupCount) ((pPPhysicalDeviceGroupProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pPhysicalDeviceGroupCount' <- lift $ peek @Word32 pPPhysicalDeviceGroupCount
  pPhysicalDeviceGroupProperties' <- lift $ generateM (fromIntegral (pPhysicalDeviceGroupCount')) (\i -> peekCStruct @PhysicalDeviceGroupProperties (((pPPhysicalDeviceGroupProperties) `advancePtrBytes` (288 * (i)) :: Ptr PhysicalDeviceGroupProperties)))
  pure $ ((r'), pPhysicalDeviceGroupProperties')



-- No documentation found for TopLevel "VkPhysicalDeviceGroupProperties"
data PhysicalDeviceGroupProperties = PhysicalDeviceGroupProperties
  { -- No documentation found for Nested "VkPhysicalDeviceGroupProperties" "physicalDeviceCount"
    physicalDeviceCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceGroupProperties" "physicalDevices"
    physicalDevices :: Vector (Ptr PhysicalDevice_T)
  , -- No documentation found for Nested "VkPhysicalDeviceGroupProperties" "subsetAllocation"
    subsetAllocation :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceGroupProperties)
#endif
deriving instance Show PhysicalDeviceGroupProperties

instance ToCStruct PhysicalDeviceGroupProperties where
  withCStruct x f = allocaBytesAligned 288 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceGroupProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (physicalDeviceCount)
    unless ((Data.Vector.length $ (physicalDevices)) <= MAX_DEVICE_GROUP_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "physicalDevices is too long, a maximum of MAX_DEVICE_GROUP_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE (Ptr PhysicalDevice_T))))) `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (physicalDevices)
    poke ((p `plusPtr` 280 :: Ptr Bool32)) (boolToBool32 (subsetAllocation))
    f
  cStructSize = 288
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    unless ((Data.Vector.length $ (mempty)) <= MAX_DEVICE_GROUP_SIZE) $
      throwIO $ IOError Nothing InvalidArgument "" "physicalDevices is too long, a maximum of MAX_DEVICE_GROUP_SIZE elements are allowed" Nothing Nothing
    Data.Vector.imapM_ (\i e -> poke ((lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE (Ptr PhysicalDevice_T))))) `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (mempty)
    poke ((p `plusPtr` 280 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceGroupProperties where
  peekCStruct p = do
    physicalDeviceCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    physicalDevices <- generateM (MAX_DEVICE_GROUP_SIZE) (\i -> peek @(Ptr PhysicalDevice_T) (((lowerArrayPtr @(Ptr PhysicalDevice_T) ((p `plusPtr` 24 :: Ptr (FixedArray MAX_DEVICE_GROUP_SIZE (Ptr PhysicalDevice_T))))) `advancePtrBytes` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T))))
    subsetAllocation <- peek @Bool32 ((p `plusPtr` 280 :: Ptr Bool32))
    pure $ PhysicalDeviceGroupProperties
             physicalDeviceCount physicalDevices (bool32ToBool subsetAllocation)


instance Storable PhysicalDeviceGroupProperties where
  sizeOf ~_ = 288
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceGroupProperties where
  zero = PhysicalDeviceGroupProperties
           zero
           mempty
           zero



-- No documentation found for TopLevel "VkDeviceGroupDeviceCreateInfo"
data DeviceGroupDeviceCreateInfo = DeviceGroupDeviceCreateInfo
  { -- No documentation found for Nested "VkDeviceGroupDeviceCreateInfo" "pPhysicalDevices"
    physicalDevices :: Vector (Ptr PhysicalDevice_T) }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceGroupDeviceCreateInfo)
#endif
deriving instance Show DeviceGroupDeviceCreateInfo

instance ToCStruct DeviceGroupDeviceCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceGroupDeviceCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (physicalDevices)) :: Word32))
    pPPhysicalDevices' <- ContT $ allocaBytesAligned @(Ptr PhysicalDevice_T) ((Data.Vector.length (physicalDevices)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPhysicalDevices' `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (physicalDevices)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (Ptr PhysicalDevice_T)))) (pPPhysicalDevices')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPPhysicalDevices' <- ContT $ allocaBytesAligned @(Ptr PhysicalDevice_T) ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPhysicalDevices' `plusPtr` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T)) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (Ptr PhysicalDevice_T)))) (pPPhysicalDevices')
    lift $ f

instance FromCStruct DeviceGroupDeviceCreateInfo where
  peekCStruct p = do
    physicalDeviceCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPhysicalDevices <- peek @(Ptr (Ptr PhysicalDevice_T)) ((p `plusPtr` 24 :: Ptr (Ptr (Ptr PhysicalDevice_T))))
    pPhysicalDevices' <- generateM (fromIntegral physicalDeviceCount) (\i -> peek @(Ptr PhysicalDevice_T) ((pPhysicalDevices `advancePtrBytes` (8 * (i)) :: Ptr (Ptr PhysicalDevice_T))))
    pure $ DeviceGroupDeviceCreateInfo
             pPhysicalDevices'

instance Zero DeviceGroupDeviceCreateInfo where
  zero = DeviceGroupDeviceCreateInfo
           mempty

