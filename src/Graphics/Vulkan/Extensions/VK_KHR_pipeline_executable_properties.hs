{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties  ( getPipelineExecutablePropertiesKHR
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

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
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
import Data.Int (Int32)
import Data.Int (Int64)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutableInternalRepresentationsKHR))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutablePropertiesKHR))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetPipelineExecutableStatisticsKHR))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Graphics.Vulkan.Core10.Handles (Pipeline)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_INFO_KHR))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineExecutablePropertiesKHR
  :: FunPtr (Ptr Device_T -> Ptr PipelineInfoKHR -> Ptr Word32 -> Ptr PipelineExecutablePropertiesKHR -> IO Result) -> Ptr Device_T -> Ptr PipelineInfoKHR -> Ptr Word32 -> Ptr PipelineExecutablePropertiesKHR -> IO Result

-- | vkGetPipelineExecutablePropertiesKHR - Get the executables associated
-- with a pipeline
--
-- = Parameters
--
-- -   @device@ is the device that created the pipeline.
--
-- -   @pPipelineInfo@ describes the pipeline being queried.
--
-- -   @pExecutableCount@ is a pointer to an integer related to the number
--     of pipeline executables available or queried, as described below.
--
-- -   @pProperties@ is either @NULL@ or a pointer to an array of
--     'PipelineExecutablePropertiesKHR' structures.
--
-- = Description
--
-- If @pProperties@ is @NULL@, then the number of executables associated
-- with the pipeline is returned in @pExecutableCount@. Otherwise,
-- @pExecutableCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pProperties@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pProperties@. If @pExecutableCount@ is less than the number of
-- executables associated with the pipeline, at most @pExecutableCount@
-- structures will be written and 'getPipelineExecutablePropertiesKHR' will
-- return 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'.
--
-- == Valid Usage
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineExecutableInfo pipelineExecutableInfo>
--     /must/ be enabled.
--
-- -   @pipeline@ member of @pPipelineInfo@ /must/ have been created with
--     @device@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pPipelineInfo@ /must/ be a valid pointer to a valid
--     'PipelineInfoKHR' structure
--
-- -   @pExecutableCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pExecutableCount@ is not @0@, and
--     @pProperties@ is not @NULL@, @pProperties@ /must/ be a valid pointer
--     to an array of @pExecutableCount@ 'PipelineExecutablePropertiesKHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'PipelineExecutablePropertiesKHR', 'PipelineInfoKHR'
getPipelineExecutablePropertiesKHR :: forall io . MonadIO io => Device -> PipelineInfoKHR -> io (Result, ("properties" ::: Vector PipelineExecutablePropertiesKHR))
getPipelineExecutablePropertiesKHR device pipelineInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutablePropertiesKHR' = mkVkGetPipelineExecutablePropertiesKHR (pVkGetPipelineExecutablePropertiesKHR (deviceCmds (device :: Device)))
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

-- | vkGetPipelineExecutableStatisticsKHR - Get compile time statistics
-- associated with a pipeline executable
--
-- = Parameters
--
-- -   @device@ is the device that created the pipeline.
--
-- -   @pExecutableInfo@ describes the pipeline executable being queried.
--
-- -   @pStatisticCount@ is a pointer to an integer related to the number
--     of statistics available or queried, as described below.
--
-- -   @pStatistics@ is either @NULL@ or a pointer to an array of
--     'PipelineExecutableStatisticKHR' structures.
--
-- = Description
--
-- If @pStatistics@ is @NULL@, then the number of statistics associated
-- with the pipeline executable is returned in @pStatisticCount@.
-- Otherwise, @pStatisticCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pStatistics@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pStatistics@. If @pStatisticCount@ is less than the number of
-- statistics associated with the pipeline executable, at most
-- @pStatisticCount@ structures will be written and
-- 'getPipelineExecutableStatisticsKHR' will return
-- 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'.
--
-- == Valid Usage
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineExecutableInfo pipelineExecutableInfo>
--     /must/ be enabled.
--
-- -   @pipeline@ member of @pExecutableInfo@ /must/ have been created with
--     @device@.
--
-- -   @pipeline@ member of @pExecutableInfo@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR'
--     set in the @flags@ field of
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' or
--     'Graphics.Vulkan.Core10.Pipeline.ComputePipelineCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pExecutableInfo@ /must/ be a valid pointer to a valid
--     'PipelineExecutableInfoKHR' structure
--
-- -   @pStatisticCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   If the value referenced by @pStatisticCount@ is not @0@, and
--     @pStatistics@ is not @NULL@, @pStatistics@ /must/ be a valid pointer
--     to an array of @pStatisticCount@ 'PipelineExecutableStatisticKHR'
--     structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'PipelineExecutableInfoKHR',
-- 'PipelineExecutableStatisticKHR'
getPipelineExecutableStatisticsKHR :: forall io . MonadIO io => Device -> PipelineExecutableInfoKHR -> io (Result, ("statistics" ::: Vector PipelineExecutableStatisticKHR))
getPipelineExecutableStatisticsKHR device executableInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutableStatisticsKHR' = mkVkGetPipelineExecutableStatisticsKHR (pVkGetPipelineExecutableStatisticsKHR (deviceCmds (device :: Device)))
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

-- | vkGetPipelineExecutableInternalRepresentationsKHR - Get internal
-- representations of the pipeline executable
--
-- = Parameters
--
-- -   @device@ is the device that created the pipeline.
--
-- -   @pExecutableInfo@ describes the pipeline executable being queried.
--
-- -   @pInternalRepresentationCount@ is a pointer to an integer related to
--     the number of internal representations available or queried, as
--     described below.
--
-- -   @pInternalRepresentations@ is either @NULL@ or a pointer to an array
--     of 'PipelineExecutableInternalRepresentationKHR' structures.
--
-- = Description
--
-- If @pInternalRepresentations@ is @NULL@, then the number of internal
-- representations associated with the pipeline executable is returned in
-- @pInternalRepresentationCount@. Otherwise,
-- @pInternalRepresentationCount@ /must/ point to a variable set by the
-- user to the number of elements in the @pInternalRepresentations@ array,
-- and on return the variable is overwritten with the number of structures
-- actually written to @pInternalRepresentations@. If
-- @pInternalRepresentationCount@ is less than the number of internal
-- representations associated with the pipeline executable, at most
-- @pInternalRepresentationCount@ structures will be written and
-- 'getPipelineExecutableInternalRepresentationsKHR' will return
-- 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'.
--
-- While the details of the internal representations remain implementation
-- dependent, the implementation /should/ order the internal
-- representations in the order in which they occur in the compile pipeline
-- with the final shader assembly (if any) last.
--
-- == Valid Usage
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineExecutableInfo pipelineExecutableInfo>
--     /must/ be enabled.
--
-- -   @pipeline@ member of @pExecutableInfo@ /must/ have been created with
--     @device@.
--
-- -   @pipeline@ member of @pExecutableInfo@ /must/ have been created with
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--     set in the @flags@ field of
--     'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' or
--     'Graphics.Vulkan.Core10.Pipeline.ComputePipelineCreateInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   @pExecutableInfo@ /must/ be a valid pointer to a valid
--     'PipelineExecutableInfoKHR' structure
--
-- -   @pInternalRepresentationCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pInternalRepresentationCount@ is not
--     @0@, and @pInternalRepresentations@ is not @NULL@,
--     @pInternalRepresentations@ /must/ be a valid pointer to an array of
--     @pInternalRepresentationCount@
--     'PipelineExecutableInternalRepresentationKHR' structures
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device', 'PipelineExecutableInfoKHR',
-- 'PipelineExecutableInternalRepresentationKHR'
getPipelineExecutableInternalRepresentationsKHR :: forall io . MonadIO io => Device -> PipelineExecutableInfoKHR -> io (Result, ("internalRepresentations" ::: Vector PipelineExecutableInternalRepresentationKHR))
getPipelineExecutableInternalRepresentationsKHR device executableInfo = liftIO . evalContT $ do
  let vkGetPipelineExecutableInternalRepresentationsKHR' = mkVkGetPipelineExecutableInternalRepresentationsKHR (pVkGetPipelineExecutableInternalRepresentationsKHR (deviceCmds (device :: Device)))
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


-- | VkPhysicalDevicePipelineExecutablePropertiesFeaturesKHR - Structure
-- describing whether pipeline executable properties are available
--
-- = Members
--
-- The members of the
-- 'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR' structure
-- is included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR' /can/ also be
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to enable features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
  { -- | @pipelineExecutableInfo@ indicates that the implementation supports
    -- reporting properties and statistics about the executables associated
    -- with a compiled pipeline.
    pipelineExecutableInfo :: Bool }
  deriving (Typeable)
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


-- | VkPipelineInfoKHR - Structure describing a pipeline
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutablePropertiesKHR'
data PipelineInfoKHR = PipelineInfoKHR
  { -- | @pipeline@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Pipeline'
    -- handle
    pipeline :: Pipeline }
  deriving (Typeable)
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


-- | VkPipelineExecutablePropertiesKHR - Structure describing a pipeline
-- executable
--
-- = Description
--
-- The @stages@ field /may/ be zero or it /may/ contain one or more bits
-- describing the stages principally used to compile this pipeline. Not all
-- implementations have a 1:1 mapping between shader stages and pipeline
-- executables and some implementations /may/ reduce a given shader stage
-- to fixed function hardware programming such that no executable is
-- available. No guarantees are provided about the mapping between shader
-- stages and pipeline executables and @stages@ /should/ be considered a
-- best effort hint. Because the application /cannot/ rely on the @stages@
-- field to provide an exact description, @name@ and @description@ provide
-- a human readable name and description which more accurately describes
-- the given pipeline executable.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutablePropertiesKHR'
data PipelineExecutablePropertiesKHR = PipelineExecutablePropertiesKHR
  { -- | @stages@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
    -- indicating which shader stages (if any) were principally used as inputs
    -- to compile this pipeline executable.
    stages :: ShaderStageFlags
  , -- | @name@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is a short human
    -- readable name for this executable.
    name :: ByteString
  , -- | @description@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is a human readable
    -- description for this executable.
    description :: ByteString
  , -- | @subgroupSize@ is the subgroup size with which this executable is
    -- dispatched.
    subgroupSize :: Word32
  }
  deriving (Typeable)
deriving instance Show PipelineExecutablePropertiesKHR

instance ToCStruct PipelineExecutablePropertiesKHR where
  withCStruct x f = allocaBytesAligned 536 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutablePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (stages)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 532 :: Ptr Word32)) (subgroupSize)
    f
  cStructSize = 536
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ShaderStageFlags)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 20 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 276 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 532 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineExecutablePropertiesKHR where
  peekCStruct p = do
    stages <- peek @ShaderStageFlags ((p `plusPtr` 16 :: Ptr ShaderStageFlags))
    name <- packCString (lowerArrayPtr ((p `plusPtr` 20 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 276 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))))
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


-- | VkPipelineExecutableInfoKHR - Structure describing a pipeline executable
-- to query for associated statistics or internal representations
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutableInternalRepresentationsKHR',
-- 'getPipelineExecutableStatisticsKHR'
data PipelineExecutableInfoKHR = PipelineExecutableInfoKHR
  { -- | @pipeline@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Pipeline'
    -- handle
    pipeline :: Pipeline
  , -- | @executableIndex@ /must/ be less than the number of executables
    -- associated with @pipeline@ as returned in the @pExecutableCount@
    -- parameter of 'getPipelineExecutablePropertiesKHR'.
    executableIndex :: Word32
  }
  deriving (Typeable)
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


-- | VkPipelineExecutableStatisticKHR - Structure describing a compile-time
-- pipeline executable statistic
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineExecutableStatisticFormatKHR',
-- 'PipelineExecutableStatisticValueKHR',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutableStatisticsKHR'
data PipelineExecutableStatisticKHR = PipelineExecutableStatisticKHR
  { -- | @name@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is a short human
    -- readable name for this statistic.
    name :: ByteString
  , -- | @description@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is a human readable
    -- description for this statistic.
    description :: ByteString
  , -- | @format@ is a 'PipelineExecutableStatisticFormatKHR' value specifying
    -- the format of the data found in @value@.
    format :: PipelineExecutableStatisticFormatKHR
  , -- | @value@ is the value of this statistic.
    value :: PipelineExecutableStatisticValueKHR
  }
  deriving (Typeable)
deriving instance Show PipelineExecutableStatisticKHR

instance ToCStruct PipelineExecutableStatisticKHR where
  withCStruct x f = allocaBytesAligned 544 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutableStatisticKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (name)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (description)
    lift $ poke ((p `plusPtr` 528 :: Ptr PipelineExecutableStatisticFormatKHR)) (format)
    ContT $ pokeCStruct ((p `plusPtr` 536 :: Ptr PipelineExecutableStatisticValueKHR)) (value) . ($ ())
    lift $ f
  cStructSize = 544
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (mempty)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (mempty)
    lift $ poke ((p `plusPtr` 528 :: Ptr PipelineExecutableStatisticFormatKHR)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 536 :: Ptr PipelineExecutableStatisticValueKHR)) (zero) . ($ ())
    lift $ f

instance FromCStruct PipelineExecutableStatisticKHR where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))))
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


-- | VkPipelineExecutableInternalRepresentationKHR - Structure describing the
-- textual form of a pipeline executable internal representation
--
-- = Description
--
-- If @pData@ is @NULL@, then the size, in bytes, of the internal
-- representation data is returned in @dataSize@. Otherwise, @dataSize@
-- must be the size of the buffer, in bytes, pointed to by @pData@ and on
-- return @dataSize@ is overwritten with the number of bytes of data
-- actually written to @pData@ including any trailing null character. If
-- @dataSize@ is less than the size, in bytes, of the internal
-- representation data, at most @dataSize@ bytes of data will be written to
-- @pData@ and 'getPipelineExecutableInternalRepresentationsKHR' will
-- return 'Graphics.Vulkan.Core10.Enums.Result.INCOMPLETE'. If @isText@ is
-- 'Graphics.Vulkan.Core10.BaseType.TRUE' and @pData@ is not @NULL@ and
-- @dataSize@ is not zero, the last byte written to @pData@ will be a null
-- character.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPipelineExecutableInternalRepresentationsKHR'
data PipelineExecutableInternalRepresentationKHR = PipelineExecutableInternalRepresentationKHR
  { -- | @name@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is a short human
    -- readable name for this internal representation.
    name :: ByteString
  , -- | @description@ is an array of
    -- 'Graphics.Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@
    -- containing a null-terminated UTF-8 string which is a human readable
    -- description for this internal representation.
    description :: ByteString
  , -- | @isText@ specifies whether the returned data is text or opaque data. If
    -- @isText@ is 'Graphics.Vulkan.Core10.BaseType.TRUE' then the data
    -- returned in @pData@ is text and is guaranteed to be a null-terminated
    -- UTF-8 string.
    isText :: Bool
  , -- | @dataSize@ is an integer related to the size, in bytes, of the internal
    -- representation data, as described below.
    dataSize :: Word64
  , -- | @pData@ is either @NULL@ or a pointer to an block of data into which the
    -- implementation will write the textual form of the internal
    -- representation.
    data' :: Ptr ()
  }
  deriving (Typeable)
deriving instance Show PipelineExecutableInternalRepresentationKHR

instance ToCStruct PipelineExecutableInternalRepresentationKHR where
  withCStruct x f = allocaBytesAligned 552 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineExecutableInternalRepresentationKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (name)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 528 :: Ptr Bool32)) (boolToBool32 (isText))
    poke ((p `plusPtr` 536 :: Ptr CSize)) (CSize (dataSize))
    poke ((p `plusPtr` 544 :: Ptr (Ptr ()))) (data')
    f
  cStructSize = 552
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 272 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 528 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineExecutableInternalRepresentationKHR where
  peekCStruct p = do
    name <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 272 :: Ptr (Data.Vector.Storable.Sized.Vector MAX_DESCRIPTION_SIZE CChar))))
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


-- | VkPipelineExecutableStatisticFormatKHR - Enum describing a pipeline
-- executable statistic
--
-- = See Also
--
-- 'PipelineExecutableStatisticKHR'
newtype PipelineExecutableStatisticFormatKHR = PipelineExecutableStatisticFormatKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR' specifies that the
-- statistic is returned as a 32-bit boolean value which /must/ be either
-- 'Graphics.Vulkan.Core10.BaseType.TRUE' or
-- 'Graphics.Vulkan.Core10.BaseType.FALSE' and /should/ be read from the
-- @b32@ field of 'PipelineExecutableStatisticValueKHR'.
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR = PipelineExecutableStatisticFormatKHR 0
-- | 'PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR' specifies that the
-- statistic is returned as a signed 64-bit integer and /should/ be read
-- from the @i64@ field of 'PipelineExecutableStatisticValueKHR'.
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR = PipelineExecutableStatisticFormatKHR 1
-- | 'PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR' specifies that the
-- statistic is returned as an unsigned 64-bit integer and /should/ be read
-- from the @u64@ field of 'PipelineExecutableStatisticValueKHR'.
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR = PipelineExecutableStatisticFormatKHR 2
-- | 'PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR' specifies that the
-- statistic is returned as a 64-bit floating-point value and /should/ be
-- read from the @f64@ field of 'PipelineExecutableStatisticValueKHR'.
pattern PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR = PipelineExecutableStatisticFormatKHR 3
{-# complete PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR,
             PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR :: PipelineExecutableStatisticFormatKHR #-}

instance Show PipelineExecutableStatisticFormatKHR where
  showsPrec p = \case
    PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR -> showString "PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR"
    PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR -> showString "PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR"
    PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR -> showString "PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR"
    PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR -> showString "PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR"
    PipelineExecutableStatisticFormatKHR x -> showParen (p >= 11) (showString "PipelineExecutableStatisticFormatKHR " . showsPrec 11 x)

instance Read PipelineExecutableStatisticFormatKHR where
  readPrec = parens (choose [("PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR", pure PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR)
                            , ("PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR", pure PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR)
                            , ("PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR", pure PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR)
                            , ("PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR", pure PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineExecutableStatisticFormatKHR")
                       v <- step readPrec
                       pure (PipelineExecutableStatisticFormatKHR v)))


type KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION"
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION = 1


type KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME = "VK_KHR_pipeline_executable_properties"

-- No documentation found for TopLevel "VK_KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME"
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME = "VK_KHR_pipeline_executable_properties"

