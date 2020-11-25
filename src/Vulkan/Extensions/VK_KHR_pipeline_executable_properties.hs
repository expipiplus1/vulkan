{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_pipeline_executable_properties"
module Vulkan.Extensions.VK_KHR_pipeline_executable_properties  ( getPipelineExecutablePropertiesKHR
                                                                , getPipelineExecutableStatisticsKHR
                                                                , getPipelineExecutableInternalRepresentationsKHR
                                                                , PhysicalDevicePipelineExecutablePropertiesFeaturesKHR(..)
                                                                , PipelineInfoKHR(..)
                                                                , PipelineExecutablePropertiesKHR(..)
                                                                , PipelineExecutableInfoKHR(..)
                                                                , PipelineExecutableStatisticKHR(..)
                                                                , PipelineExecutableInternalRepresentationKHR(..)
                                                                , PipelineExecutableStatisticValueKHR(..)
                                                                , peekPipelineExecutableStatisticValueKHR
                                                                , PipelineExecutableStatisticFormatKHR( PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR
                                                                                                      , PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR
                                                                                                      , PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR
                                                                                                      , PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR
                                                                                                      , ..
                                                                                                      )
                                                                , KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION
                                                                , pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION
                                                                , KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
                                                                , pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME
                                                                ) where

import Vulkan.CStruct.Utils (FixedArray)
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
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Cont (runContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CDouble)
import Foreign.C.Types (CDouble(CDouble))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutableInternalRepresentationsKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutablePropertiesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutableStatisticsKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineExecutablePropertiesKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineInfoKHR -> Ptr Word32 -> Ptr PipelineExecutablePropertiesKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineInfoKHR -> Ptr Word32 -> Ptr PipelineExecutablePropertiesKHR -> IO Result

-- No documentation found for TopLevel "vkGetPipelineExecutablePropertiesKHR"
getPipelineExecutablePropertiesKHR :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkGetPipelineExecutablePropertiesKHR" "device"
                                      Device
                                   -> -- No documentation found for Nested "vkGetPipelineExecutablePropertiesKHR" "pPipelineInfo"
                                      PipelineInfoKHR
                                   -> io (Result, ("properties" ::: Vector PipelineExecutablePropertiesKHR))
getPipelineExecutablePropertiesKHR device pipelineInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutablePropertiesKHRPtr = pVkGetPipelineExecutablePropertiesKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetPipelineExecutablePropertiesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineExecutablePropertiesKHR is null" Nothing Nothing
  let vkGetPipelineExecutablePropertiesKHR' = mkVkGetPipelineExecutablePropertiesKHR vkGetPipelineExecutablePropertiesKHRPtr
  let device' = deviceHandle (device)
  pPipelineInfo <- ContT $ withCStruct (pipelineInfo)
  pPExecutableCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPipelineExecutablePropertiesKHR' device' pPipelineInfo (pPExecutableCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pExecutableCount <- lift $ peek @Word32 pPExecutableCount
  pPProperties <- ContT $ bracket (callocBytes @PipelineExecutablePropertiesKHR ((fromIntegral (pExecutableCount)) * 536)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPProperties `advancePtrBytes` (i * 536) :: Ptr PipelineExecutablePropertiesKHR) . ($ ())) [0..(fromIntegral (pExecutableCount)) - 1]
  r' <- lift $ vkGetPipelineExecutablePropertiesKHR' device' pPipelineInfo (pPExecutableCount) ((pPProperties))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pExecutableCount' <- lift $ peek @Word32 pPExecutableCount
  pProperties' <- lift $ generateM (fromIntegral (pExecutableCount')) (\i -> peekCStruct @PipelineExecutablePropertiesKHR (((pPProperties) `advancePtrBytes` (536 * (i)) :: Ptr PipelineExecutablePropertiesKHR)))
  pure $ ((r'), pProperties')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineExecutableStatisticsKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineExecutableInfoKHR -> Ptr Word32 -> Ptr PipelineExecutableStatisticKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineExecutableInfoKHR -> Ptr Word32 -> Ptr PipelineExecutableStatisticKHR -> IO Result

-- No documentation found for TopLevel "vkGetPipelineExecutableStatisticsKHR"
getPipelineExecutableStatisticsKHR :: forall io
                                    . (MonadIO io)
                                   => -- No documentation found for Nested "vkGetPipelineExecutableStatisticsKHR" "device"
                                      Device
                                   -> -- No documentation found for Nested "vkGetPipelineExecutableStatisticsKHR" "pExecutableInfo"
                                      PipelineExecutableInfoKHR
                                   -> io (Result, ("statistics" ::: Vector PipelineExecutableStatisticKHR))
getPipelineExecutableStatisticsKHR device executableInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutableStatisticsKHRPtr = pVkGetPipelineExecutableStatisticsKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetPipelineExecutableStatisticsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineExecutableStatisticsKHR is null" Nothing Nothing
  let vkGetPipelineExecutableStatisticsKHR' = mkVkGetPipelineExecutableStatisticsKHR vkGetPipelineExecutableStatisticsKHRPtr
  let device' = deviceHandle (device)
  pExecutableInfo <- ContT $ withCStruct (executableInfo)
  pPStatisticCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPipelineExecutableStatisticsKHR' device' pExecutableInfo (pPStatisticCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pStatisticCount <- lift $ peek @Word32 pPStatisticCount
  pPStatistics <- ContT $ bracket (callocBytes @PipelineExecutableStatisticKHR ((fromIntegral (pStatisticCount)) * 544)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPStatistics `advancePtrBytes` (i * 544) :: Ptr PipelineExecutableStatisticKHR) . ($ ())) [0..(fromIntegral (pStatisticCount)) - 1]
  r' <- lift $ vkGetPipelineExecutableStatisticsKHR' device' pExecutableInfo (pPStatisticCount) ((pPStatistics))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pStatisticCount' <- lift $ peek @Word32 pPStatisticCount
  pStatistics' <- lift $ generateM (fromIntegral (pStatisticCount')) (\i -> peekCStruct @PipelineExecutableStatisticKHR (((pPStatistics) `advancePtrBytes` (544 * (i)) :: Ptr PipelineExecutableStatisticKHR)))
  pure $ ((r'), pStatistics')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineExecutableInternalRepresentationsKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineExecutableInfoKHR -> Ptr Word32 -> Ptr PipelineExecutableInternalRepresentationKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineExecutableInfoKHR -> Ptr Word32 -> Ptr PipelineExecutableInternalRepresentationKHR -> IO Result

-- No documentation found for TopLevel "vkGetPipelineExecutableInternalRepresentationsKHR"
getPipelineExecutableInternalRepresentationsKHR :: forall io
                                                 . (MonadIO io)
                                                => -- No documentation found for Nested "vkGetPipelineExecutableInternalRepresentationsKHR" "device"
                                                   Device
                                                -> -- No documentation found for Nested "vkGetPipelineExecutableInternalRepresentationsKHR" "pExecutableInfo"
                                                   PipelineExecutableInfoKHR
                                                -> io (Result, ("internalRepresentations" ::: Vector PipelineExecutableInternalRepresentationKHR))
getPipelineExecutableInternalRepresentationsKHR device executableInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutableInternalRepresentationsKHRPtr = pVkGetPipelineExecutableInternalRepresentationsKHR (deviceCmds (device :: Device))
  lift $ unless (vkGetPipelineExecutableInternalRepresentationsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPipelineExecutableInternalRepresentationsKHR is null" Nothing Nothing
  let vkGetPipelineExecutableInternalRepresentationsKHR' = mkVkGetPipelineExecutableInternalRepresentationsKHR vkGetPipelineExecutableInternalRepresentationsKHRPtr
  let device' = deviceHandle (device)
  pExecutableInfo <- ContT $ withCStruct (executableInfo)
  pPInternalRepresentationCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPipelineExecutableInternalRepresentationsKHR' device' pExecutableInfo (pPInternalRepresentationCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pInternalRepresentationCount <- lift $ peek @Word32 pPInternalRepresentationCount
  pPInternalRepresentations <- ContT $ bracket (callocBytes @PipelineExecutableInternalRepresentationKHR ((fromIntegral (pInternalRepresentationCount)) * 552)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPInternalRepresentations `advancePtrBytes` (i * 552) :: Ptr PipelineExecutableInternalRepresentationKHR) . ($ ())) [0..(fromIntegral (pInternalRepresentationCount)) - 1]
  r' <- lift $ vkGetPipelineExecutableInternalRepresentationsKHR' device' pExecutableInfo (pPInternalRepresentationCount) ((pPInternalRepresentations))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pInternalRepresentationCount' <- lift $ peek @Word32 pPInternalRepresentationCount
  pInternalRepresentations' <- lift $ generateM (fromIntegral (pInternalRepresentationCount')) (\i -> peekCStruct @PipelineExecutableInternalRepresentationKHR (((pPInternalRepresentations) `advancePtrBytes` (552 * (i)) :: Ptr PipelineExecutableInternalRepresentationKHR)))
  pure $ ((r'), pInternalRepresentations')



-- No documentation found for TopLevel "VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR"
data PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR" "pipelineExecutableInfo"
    pipelineExecutableInfo :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
#endif
deriving instance Show PhysicalDevicePipelineExecutablePropertiesFeaturesKHR

instance ToCStruct PhysicalDevicePipelineExecutablePropertiesFeaturesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePipelineExecutablePropertiesFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineExecutableInfo))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePipelineExecutablePropertiesFeaturesKHR where
  peekCStruct p = do
    pipelineExecutableInfo <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
             (bool32ToBool pipelineExecutableInfo)


instance Storable PhysicalDevicePipelineExecutablePropertiesFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePipelineExecutablePropertiesFeaturesKHR where
  zero = PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
           zero



-- No documentation found for TopLevel "VkPipelineInfoKHR"
data PipelineInfoKHR = PipelineInfoKHR
  { -- No documentation found for Nested "VkPipelineInfoKHR" "pipeline"
    pipeline :: Pipeline }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineInfoKHR)
#endif
deriving instance Show PipelineInfoKHR

instance ToCStruct PipelineInfoKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (pipeline)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (zero)
    f

instance FromCStruct PipelineInfoKHR where
  peekCStruct p = do
    pipeline <- peek @Pipeline ((p `plusPtr` 16 :: Ptr Pipeline))
    pure $ PipelineInfoKHR
             pipeline


instance Storable PipelineInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineInfoKHR where
  zero = PipelineInfoKHR
           zero



-- No documentation found for TopLevel "VkPipelineExecutablePropertiesKHR"
data PipelineExecutablePropertiesKHR = PipelineExecutablePropertiesKHR
  { -- No documentation found for Nested "VkPipelineExecutablePropertiesKHR" "stages"
    stages :: ShaderStageFlags
  , -- No documentation found for Nested "VkPipelineExecutablePropertiesKHR" "name"
    name :: ByteString
  , -- No documentation found for Nested "VkPipelineExecutablePropertiesKHR" "description"
    description :: ByteString
  , -- No documentation found for Nested "VkPipelineExecutablePropertiesKHR" "subgroupSize"
    subgroupSize :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineExecutablePropertiesKHR)
#endif
deriving instance Show PipelineExecutablePropertiesKHR

instance ToCStruct PipelineExecutablePropertiesKHR where
  withCStruct x f = allocaBytesAligned 536 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutablePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (stages)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 532 :: Ptr Word32)) (subgroupSize)
    f
  cStructSize = 536
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 532 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineExecutablePropertiesKHR where
  peekCStruct p = do
    stages <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    name <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 276 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    subgroupSize <- peek @Word32 ((p `plusPtr` 532 :: Ptr Word32))
    pure $ PipelineExecutablePropertiesKHR
             stages name description subgroupSize


instance Storable PipelineExecutablePropertiesKHR where
  sizeOf ~_ = 536
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineExecutablePropertiesKHR where
  zero = PipelineExecutablePropertiesKHR
           zero
           mempty
           mempty
           zero



-- No documentation found for TopLevel "VkPipelineExecutableInfoKHR"
data PipelineExecutableInfoKHR = PipelineExecutableInfoKHR
  { -- No documentation found for Nested "VkPipelineExecutableInfoKHR" "pipeline"
    pipeline :: Pipeline
  , -- No documentation found for Nested "VkPipelineExecutableInfoKHR" "executableIndex"
    executableIndex :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineExecutableInfoKHR)
#endif
deriving instance Show PipelineExecutableInfoKHR

instance ToCStruct PipelineExecutableInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutableInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (pipeline)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (executableIndex)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Pipeline)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineExecutableInfoKHR where
  peekCStruct p = do
    pipeline <- peek @Pipeline ((p `plusPtr` 16 :: Ptr Pipeline))
    executableIndex <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ PipelineExecutableInfoKHR
             pipeline executableIndex


instance Storable PipelineExecutableInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineExecutableInfoKHR where
  zero = PipelineExecutableInfoKHR
           zero
           zero



-- No documentation found for TopLevel "VkPipelineExecutableStatisticKHR"
data PipelineExecutableStatisticKHR = PipelineExecutableStatisticKHR
  { -- No documentation found for Nested "VkPipelineExecutableStatisticKHR" "name"
    name :: ByteString
  , -- No documentation found for Nested "VkPipelineExecutableStatisticKHR" "description"
    description :: ByteString
  , -- No documentation found for Nested "VkPipelineExecutableStatisticKHR" "format"
    format :: PipelineExecutableStatisticFormatKHR
  , -- No documentation found for Nested "VkPipelineExecutableStatisticKHR" "value"
    value :: PipelineExecutableStatisticValueKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineExecutableStatisticKHR)
#endif
deriving instance Show PipelineExecutableStatisticKHR

instance ToCStruct PipelineExecutableStatisticKHR where
  withCStruct x f = allocaBytesAligned 544 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutableStatisticKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    lift $ poke ((p `plusPtr` 528 :: Ptr PipelineExecutableStatisticFormatKHR)) (format)
    ContT $ pokeCStruct ((p `plusPtr` 536 :: Ptr PipelineExecutableStatisticValueKHR)) (value) . ($ ())
    lift $ f
  cStructSize = 544
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    lift $ poke ((p `plusPtr` 528 :: Ptr PipelineExecutableStatisticFormatKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 536 :: Ptr PipelineExecutableStatisticValueKHR)) (zero) . ($ ())
    lift $ f

instance FromCStruct PipelineExecutableStatisticKHR where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    format <- peek @PipelineExecutableStatisticFormatKHR ((p `plusPtr` 528 :: Ptr PipelineExecutableStatisticFormatKHR))
    value <- peekPipelineExecutableStatisticValueKHR format ((p `plusPtr` 536 :: Ptr PipelineExecutableStatisticValueKHR))
    pure $ PipelineExecutableStatisticKHR
             name description format value

instance Zero PipelineExecutableStatisticKHR where
  zero = PipelineExecutableStatisticKHR
           mempty
           mempty
           zero
           zero



-- No documentation found for TopLevel "VkPipelineExecutableInternalRepresentationKHR"
data PipelineExecutableInternalRepresentationKHR = PipelineExecutableInternalRepresentationKHR
  { -- No documentation found for Nested "VkPipelineExecutableInternalRepresentationKHR" "name"
    name :: ByteString
  , -- No documentation found for Nested "VkPipelineExecutableInternalRepresentationKHR" "description"
    description :: ByteString
  , -- No documentation found for Nested "VkPipelineExecutableInternalRepresentationKHR" "isText"
    isText :: Bool
  , -- No documentation found for Nested "VkPipelineExecutableInternalRepresentationKHR" "dataSize"
    dataSize :: Word64
  , -- No documentation found for Nested "VkPipelineExecutableInternalRepresentationKHR" "pData"
    data' :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineExecutableInternalRepresentationKHR)
#endif
deriving instance Show PipelineExecutableInternalRepresentationKHR

instance ToCStruct PipelineExecutableInternalRepresentationKHR where
  withCStruct x f = allocaBytesAligned 552 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutableInternalRepresentationKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 528 :: Ptr Bool32)) (boolToBool32 (isText))
    poke ((p `plusPtr` 536 :: Ptr CSize)) (CSize (dataSize))
    poke ((p `plusPtr` 544 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 552
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 528 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineExecutableInternalRepresentationKHR where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    isText <- peek @Bool32 ((p `plusPtr` 528 :: Ptr Bool32))
    dataSize <- peek @CSize ((p `plusPtr` 536 :: Ptr CSize))
    pData <- peek @(Ptr ()) ((p `plusPtr` 544 :: Ptr (Ptr ())))
    pure $ PipelineExecutableInternalRepresentationKHR
             name description (bool32ToBool isText) ((\(CSize a) -> a) dataSize) pData


instance Storable PipelineExecutableInternalRepresentationKHR where
  sizeOf ~_ = 552
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineExecutableInternalRepresentationKHR where
  zero = PipelineExecutableInternalRepresentationKHR
           mempty
           mempty
           zero
           zero
           zero


data PipelineExecutableStatisticValueKHR
  = B32 Bool
  | I64 Int64
  | U64 Word64
  | F64 Double
  deriving (Show)

instance ToCStruct PipelineExecutableStatisticValueKHR where
  withCStruct x f = allocaBytesAligned 8 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct :: Ptr PipelineExecutableStatisticValueKHR -> PipelineExecutableStatisticValueKHR -> IO a -> IO a
  pokeCStruct p = (. const) . runContT .  \case
    B32 v -> lift $ poke (castPtr @_ @Bool32 p) (boolToBool32 (v))
    I64 v -> lift $ poke (castPtr @_ @Int64 p) (v)
    U64 v -> lift $ poke (castPtr @_ @Word64 p) (v)
    F64 v -> lift $ poke (castPtr @_ @CDouble p) (CDouble (v))
  pokeZeroCStruct :: Ptr PipelineExecutableStatisticValueKHR -> IO b -> IO b
  pokeZeroCStruct _ f = f
  cStructSize = 8
  cStructAlignment = 8

instance Zero PipelineExecutableStatisticValueKHR where
  zero = I64 zero

peekPipelineExecutableStatisticValueKHR :: PipelineExecutableStatisticFormatKHR -> Ptr PipelineExecutableStatisticValueKHR -> IO PipelineExecutableStatisticValueKHR
peekPipelineExecutableStatisticValueKHR tag p = case tag of
  PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR -> B32 <$> (do
    b32 <- peek @Bool32 (castPtr @_ @Bool32 p)
    pure $ bool32ToBool b32)
  PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR -> I64 <$> (peek @Int64 (castPtr @_ @Int64 p))
  PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR -> U64 <$> (peek @Word64 (castPtr @_ @Word64 p))
  PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR -> F64 <$> (do
    f64 <- peek @CDouble (castPtr @_ @CDouble p)
    pure $ (\(CDouble a) -> a) f64)


-- No documentation found for TopLevel "VkPipelineExecutableStatisticFormatKHR"
newtype PipelineExecutableStatisticFormatKHR = PipelineExecutableStatisticFormatKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPipelineExecutableStatisticFormatKHR" "VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR"
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR  = PipelineExecutableStatisticFormatKHR 0
-- No documentation found for Nested "VkPipelineExecutableStatisticFormatKHR" "VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR"
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR   = PipelineExecutableStatisticFormatKHR 1
-- No documentation found for Nested "VkPipelineExecutableStatisticFormatKHR" "VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR"
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR  = PipelineExecutableStatisticFormatKHR 2
-- No documentation found for Nested "VkPipelineExecutableStatisticFormatKHR" "VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR"
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR = PipelineExecutableStatisticFormatKHR 3
{-# complete PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR :: PipelineExecutableStatisticFormatKHR #-}

conNamePipelineExecutableStatisticFormatKHR :: String
conNamePipelineExecutableStatisticFormatKHR = "PipelineExecutableStatisticFormatKHR"

enumPrefixPipelineExecutableStatisticFormatKHR :: String
enumPrefixPipelineExecutableStatisticFormatKHR = "PIPELINE_EXECUTABLE_STATISTIC_FORMAT_"

showTablePipelineExecutableStatisticFormatKHR :: [(PipelineExecutableStatisticFormatKHR, String)]
showTablePipelineExecutableStatisticFormatKHR =
  [ (PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR , "BOOL32_KHR")
  , (PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR  , "INT64_KHR")
  , (PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR , "UINT64_KHR")
  , (PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR, "FLOAT64_KHR")
  ]


instance Show PipelineExecutableStatisticFormatKHR where
showsPrec = enumShowsPrec enumPrefixPipelineExecutableStatisticFormatKHR
                          showTablePipelineExecutableStatisticFormatKHR
                          conNamePipelineExecutableStatisticFormatKHR
                          (\(PipelineExecutableStatisticFormatKHR x) -> x)
                          (showsPrec 11)


instance Read PipelineExecutableStatisticFormatKHR where
  readPrec = enumReadPrec enumPrefixPipelineExecutableStatisticFormatKHR
                          showTablePipelineExecutableStatisticFormatKHR
                          conNamePipelineExecutableStatisticFormatKHR
                          PipelineExecutableStatisticFormatKHR


type KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION"
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1


type KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME = "VK_KHR_pipeline_executable_properties"

-- No documentation found for TopLevel "VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME"
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME = "VK_KHR_pipeline_executable_properties"

