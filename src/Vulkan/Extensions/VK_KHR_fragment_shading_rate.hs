{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_fragment_shading_rate"
module Vulkan.Extensions.VK_KHR_fragment_shading_rate  ( cmdSetFragmentShadingRateKHR
                                                       , getPhysicalDeviceFragmentShadingRatesKHR
                                                       , pattern IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR
                                                       , pattern ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR
                                                       , pattern IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                       , pattern PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                       , FragmentShadingRateAttachmentInfoKHR(..)
                                                       , PipelineFragmentShadingRateStateCreateInfoKHR(..)
                                                       , PhysicalDeviceFragmentShadingRateFeaturesKHR(..)
                                                       , PhysicalDeviceFragmentShadingRatePropertiesKHR(..)
                                                       , PhysicalDeviceFragmentShadingRateKHR(..)
                                                       , FragmentShadingRateCombinerOpKHR( FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR
                                                                                         , FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR
                                                                                         , FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR
                                                                                         , FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR
                                                                                         , FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR
                                                                                         , ..
                                                                                         )
                                                       , KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION
                                                       , pattern KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION
                                                       , KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME
                                                       , pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentReference2)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetFragmentShadingRateKHR))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceFragmentShadingRatesKHR))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV))
import Vulkan.Core10.Enums.ImageLayout (ImageLayout(IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetFragmentShadingRateKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr Extent2D -> Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR) -> IO ()) -> Ptr CommandBuffer_T -> Ptr Extent2D -> Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR) -> IO ()

-- No documentation found for TopLevel "vkCmdSetFragmentShadingRateKHR"
cmdSetFragmentShadingRateKHR :: forall io
                              . (MonadIO io)
                             => -- No documentation found for Nested "vkCmdSetFragmentShadingRateKHR" "commandBuffer"
                                CommandBuffer
                             -> -- No documentation found for Nested "vkCmdSetFragmentShadingRateKHR" "pFragmentSize"
                                ("fragmentSize" ::: Extent2D)
                             -> -- No documentation found for Nested "vkCmdSetFragmentShadingRateKHR" "combinerOps"
                                ("combinerOps" ::: (FragmentShadingRateCombinerOpKHR, FragmentShadingRateCombinerOpKHR))
                             -> io ()
cmdSetFragmentShadingRateKHR commandBuffer fragmentSize combinerOps = liftIO . evalContT $ do
  let vkCmdSetFragmentShadingRateKHRPtr = pVkCmdSetFragmentShadingRateKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetFragmentShadingRateKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetFragmentShadingRateKHR is null" Nothing Nothing
  let vkCmdSetFragmentShadingRateKHR' = mkVkCmdSetFragmentShadingRateKHR vkCmdSetFragmentShadingRateKHRPtr
  pFragmentSize <- ContT $ withCStruct (fragmentSize)
  pCombinerOps <- ContT $ allocaBytesAligned @(FixedArray 2 FragmentShadingRateCombinerOpKHR) 8 4
  let pCombinerOps' = lowerArrayPtr pCombinerOps
  lift $ case (combinerOps) of
    (e0, e1) -> do
      poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
      poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
  lift $ vkCmdSetFragmentShadingRateKHR' (commandBufferHandle (commandBuffer)) pFragmentSize (pCombinerOps)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFragmentShadingRatesKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr PhysicalDeviceFragmentShadingRateKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr PhysicalDeviceFragmentShadingRateKHR -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceFragmentShadingRatesKHR"
getPhysicalDeviceFragmentShadingRatesKHR :: forall io
                                          . (MonadIO io)
                                         => -- No documentation found for Nested "vkGetPhysicalDeviceFragmentShadingRatesKHR" "physicalDevice"
                                            PhysicalDevice
                                         -> io (Result, ("fragmentShadingRates" ::: Vector PhysicalDeviceFragmentShadingRateKHR))
getPhysicalDeviceFragmentShadingRatesKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceFragmentShadingRatesKHRPtr = pVkGetPhysicalDeviceFragmentShadingRatesKHR (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceFragmentShadingRatesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceFragmentShadingRatesKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceFragmentShadingRatesKHR' = mkVkGetPhysicalDeviceFragmentShadingRatesKHR vkGetPhysicalDeviceFragmentShadingRatesKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPFragmentShadingRateCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceFragmentShadingRatesKHR' physicalDevice' (pPFragmentShadingRateCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFragmentShadingRateCount <- lift $ peek @Word32 pPFragmentShadingRateCount
  pPFragmentShadingRates <- ContT $ bracket (callocBytes @PhysicalDeviceFragmentShadingRateKHR ((fromIntegral (pFragmentShadingRateCount)) * 32)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPFragmentShadingRates `advancePtrBytes` (i * 32) :: Ptr PhysicalDeviceFragmentShadingRateKHR) . ($ ())) [0..(fromIntegral (pFragmentShadingRateCount)) - 1]
  r' <- lift $ vkGetPhysicalDeviceFragmentShadingRatesKHR' physicalDevice' (pPFragmentShadingRateCount) ((pPFragmentShadingRates))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pFragmentShadingRateCount' <- lift $ peek @Word32 pPFragmentShadingRateCount
  pFragmentShadingRates' <- lift $ generateM (fromIntegral (pFragmentShadingRateCount')) (\i -> peekCStruct @PhysicalDeviceFragmentShadingRateKHR (((pPFragmentShadingRates) `advancePtrBytes` (32 * (i)) :: Ptr PhysicalDeviceFragmentShadingRateKHR)))
  pure $ ((r'), pFragmentShadingRates')


-- No documentation found for TopLevel "VK_IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR"
pattern IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR = IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV


-- No documentation found for TopLevel "VK_ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR"
pattern ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR = ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV


-- No documentation found for TopLevel "VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV



-- No documentation found for TopLevel "VkFragmentShadingRateAttachmentInfoKHR"
data FragmentShadingRateAttachmentInfoKHR = FragmentShadingRateAttachmentInfoKHR
  { -- No documentation found for Nested "VkFragmentShadingRateAttachmentInfoKHR" "pFragmentShadingRateAttachment"
    fragmentShadingRateAttachment :: SomeStruct AttachmentReference2
  , -- No documentation found for Nested "VkFragmentShadingRateAttachmentInfoKHR" "shadingRateAttachmentTexelSize"
    shadingRateAttachmentTexelSize :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FragmentShadingRateAttachmentInfoKHR)
#endif
deriving instance Show FragmentShadingRateAttachmentInfoKHR

instance ToCStruct FragmentShadingRateAttachmentInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FragmentShadingRateAttachmentInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pFragmentShadingRateAttachment'' <- ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 (fragmentShadingRateAttachment) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (AttachmentReference2 _)))) pFragmentShadingRateAttachment''
    lift $ poke ((p `plusPtr` 24 :: Ptr Extent2D)) (shadingRateAttachmentTexelSize)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pFragmentShadingRateAttachment'' <- ContT @_ @_ @(Ptr (AttachmentReference2 '[])) $ \cont -> withSomeCStruct @AttachmentReference2 ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (AttachmentReference2 _)))) pFragmentShadingRateAttachment''
    lift $ poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    lift $ f

instance FromCStruct FragmentShadingRateAttachmentInfoKHR where
  peekCStruct p = do
    pFragmentShadingRateAttachment <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (AttachmentReference2 a))))
    shadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    pure $ FragmentShadingRateAttachmentInfoKHR
             pFragmentShadingRateAttachment shadingRateAttachmentTexelSize

instance Zero FragmentShadingRateAttachmentInfoKHR where
  zero = FragmentShadingRateAttachmentInfoKHR
           (SomeStruct zero)
           zero



-- No documentation found for TopLevel "VkPipelineFragmentShadingRateStateCreateInfoKHR"
data PipelineFragmentShadingRateStateCreateInfoKHR = PipelineFragmentShadingRateStateCreateInfoKHR
  { -- No documentation found for Nested "VkPipelineFragmentShadingRateStateCreateInfoKHR" "fragmentSize"
    fragmentSize :: Extent2D
  , -- No documentation found for Nested "VkPipelineFragmentShadingRateStateCreateInfoKHR" "combinerOps"
    combinerOps :: (FragmentShadingRateCombinerOpKHR, FragmentShadingRateCombinerOpKHR)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineFragmentShadingRateStateCreateInfoKHR)
#endif
deriving instance Show PipelineFragmentShadingRateStateCreateInfoKHR

instance ToCStruct PipelineFragmentShadingRateStateCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineFragmentShadingRateStateCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (fragmentSize)
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    case (combinerOps) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    f

instance FromCStruct PipelineFragmentShadingRateStateCreateInfoKHR where
  peekCStruct p = do
    fragmentSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    let pcombinerOps = lowerArrayPtr @FragmentShadingRateCombinerOpKHR ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    combinerOps0 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 0 :: Ptr FragmentShadingRateCombinerOpKHR))
    combinerOps1 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 4 :: Ptr FragmentShadingRateCombinerOpKHR))
    pure $ PipelineFragmentShadingRateStateCreateInfoKHR
             fragmentSize ((combinerOps0, combinerOps1))


instance Storable PipelineFragmentShadingRateStateCreateInfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineFragmentShadingRateStateCreateInfoKHR where
  zero = PipelineFragmentShadingRateStateCreateInfoKHR
           zero
           (zero, zero)



-- No documentation found for TopLevel "VkPhysicalDeviceFragmentShadingRateFeaturesKHR"
data PhysicalDeviceFragmentShadingRateFeaturesKHR = PhysicalDeviceFragmentShadingRateFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateFeaturesKHR" "pipelineFragmentShadingRate"
    pipelineFragmentShadingRate :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateFeaturesKHR" "primitiveFragmentShadingRate"
    primitiveFragmentShadingRate :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateFeaturesKHR" "attachmentFragmentShadingRate"
    attachmentFragmentShadingRate :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRateFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRateFeaturesKHR

instance ToCStruct PhysicalDeviceFragmentShadingRateFeaturesKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRateFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (pipelineFragmentShadingRate))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (primitiveFragmentShadingRate))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (attachmentFragmentShadingRate))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShadingRateFeaturesKHR where
  peekCStruct p = do
    pipelineFragmentShadingRate <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    primitiveFragmentShadingRate <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    attachmentFragmentShadingRate <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShadingRateFeaturesKHR
             (bool32ToBool pipelineFragmentShadingRate) (bool32ToBool primitiveFragmentShadingRate) (bool32ToBool attachmentFragmentShadingRate)


instance Storable PhysicalDeviceFragmentShadingRateFeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRateFeaturesKHR where
  zero = PhysicalDeviceFragmentShadingRateFeaturesKHR
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceFragmentShadingRatePropertiesKHR"
data PhysicalDeviceFragmentShadingRatePropertiesKHR = PhysicalDeviceFragmentShadingRatePropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "minFragmentShadingRateAttachmentTexelSize"
    minFragmentShadingRateAttachmentTexelSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentShadingRateAttachmentTexelSize"
    maxFragmentShadingRateAttachmentTexelSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentShadingRateAttachmentTexelSizeAspectRatio"
    maxFragmentShadingRateAttachmentTexelSizeAspectRatio :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "primitiveFragmentShadingRateWithMultipleViewports"
    primitiveFragmentShadingRateWithMultipleViewports :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "layeredShadingRateAttachments"
    layeredShadingRateAttachments :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateNonTrivialCombinerOps"
    fragmentShadingRateNonTrivialCombinerOps :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentSize"
    maxFragmentSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentSizeAspectRatio"
    maxFragmentSizeAspectRatio :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentShadingRateCoverageSamples"
    maxFragmentShadingRateCoverageSamples :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "maxFragmentShadingRateRasterizationSamples"
    maxFragmentShadingRateRasterizationSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithShaderDepthStencilWrites"
    fragmentShadingRateWithShaderDepthStencilWrites :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithSampleMask"
    fragmentShadingRateWithSampleMask :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithShaderSampleMask"
    fragmentShadingRateWithShaderSampleMask :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithConservativeRasterization"
    fragmentShadingRateWithConservativeRasterization :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithFragmentShaderInterlock"
    fragmentShadingRateWithFragmentShaderInterlock :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateWithCustomSampleLocations"
    fragmentShadingRateWithCustomSampleLocations :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRatePropertiesKHR" "fragmentShadingRateStrictMultiplyCombiner"
    fragmentShadingRateStrictMultiplyCombiner :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRatePropertiesKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRatePropertiesKHR

instance ToCStruct PhysicalDeviceFragmentShadingRatePropertiesKHR where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRatePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (minFragmentShadingRateAttachmentTexelSize)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (maxFragmentShadingRateAttachmentTexelSize)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (maxFragmentShadingRateAttachmentTexelSizeAspectRatio)
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (primitiveFragmentShadingRateWithMultipleViewports))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (layeredShadingRateAttachments))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateNonTrivialCombinerOps))
    poke ((p `plusPtr` 48 :: Ptr Extent2D)) (maxFragmentSize)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxFragmentSizeAspectRatio)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (maxFragmentShadingRateCoverageSamples)
    poke ((p `plusPtr` 64 :: Ptr SampleCountFlagBits)) (maxFragmentShadingRateRasterizationSamples)
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithShaderDepthStencilWrites))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithSampleMask))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithShaderSampleMask))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithConservativeRasterization))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithFragmentShaderInterlock))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateWithCustomSampleLocations))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateStrictMultiplyCombiner))
    f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 60 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 64 :: Ptr SampleCountFlagBits)) (zero)
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShadingRatePropertiesKHR where
  peekCStruct p = do
    minFragmentShadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    maxFragmentShadingRateAttachmentTexelSize <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    maxFragmentShadingRateAttachmentTexelSizeAspectRatio <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    primitiveFragmentShadingRateWithMultipleViewports <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    layeredShadingRateAttachments <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    fragmentShadingRateNonTrivialCombinerOps <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    maxFragmentSize <- peekCStruct @Extent2D ((p `plusPtr` 48 :: Ptr Extent2D))
    maxFragmentSizeAspectRatio <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    maxFragmentShadingRateCoverageSamples <- peek @Word32 ((p `plusPtr` 60 :: Ptr Word32))
    maxFragmentShadingRateRasterizationSamples <- peek @SampleCountFlagBits ((p `plusPtr` 64 :: Ptr SampleCountFlagBits))
    fragmentShadingRateWithShaderDepthStencilWrites <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    fragmentShadingRateWithSampleMask <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    fragmentShadingRateWithShaderSampleMask <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    fragmentShadingRateWithConservativeRasterization <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    fragmentShadingRateWithFragmentShaderInterlock <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    fragmentShadingRateWithCustomSampleLocations <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    fragmentShadingRateStrictMultiplyCombiner <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShadingRatePropertiesKHR
             minFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSize maxFragmentShadingRateAttachmentTexelSizeAspectRatio (bool32ToBool primitiveFragmentShadingRateWithMultipleViewports) (bool32ToBool layeredShadingRateAttachments) (bool32ToBool fragmentShadingRateNonTrivialCombinerOps) maxFragmentSize maxFragmentSizeAspectRatio maxFragmentShadingRateCoverageSamples maxFragmentShadingRateRasterizationSamples (bool32ToBool fragmentShadingRateWithShaderDepthStencilWrites) (bool32ToBool fragmentShadingRateWithSampleMask) (bool32ToBool fragmentShadingRateWithShaderSampleMask) (bool32ToBool fragmentShadingRateWithConservativeRasterization) (bool32ToBool fragmentShadingRateWithFragmentShaderInterlock) (bool32ToBool fragmentShadingRateWithCustomSampleLocations) (bool32ToBool fragmentShadingRateStrictMultiplyCombiner)


instance Storable PhysicalDeviceFragmentShadingRatePropertiesKHR where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRatePropertiesKHR where
  zero = PhysicalDeviceFragmentShadingRatePropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceFragmentShadingRateKHR"
data PhysicalDeviceFragmentShadingRateKHR = PhysicalDeviceFragmentShadingRateKHR
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateKHR" "sampleCounts"
    sampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentShadingRateKHR" "fragmentSize"
    fragmentSize :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRateKHR)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRateKHR

instance ToCStruct PhysicalDeviceFragmentShadingRateKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRateKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (sampleCounts)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (fragmentSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentShadingRateKHR where
  peekCStruct p = do
    sampleCounts <- peek @SampleCountFlags ((p `plusPtr` 16 :: Ptr SampleCountFlags))
    fragmentSize <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    pure $ PhysicalDeviceFragmentShadingRateKHR
             sampleCounts fragmentSize


instance Storable PhysicalDeviceFragmentShadingRateKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRateKHR where
  zero = PhysicalDeviceFragmentShadingRateKHR
           zero
           zero


-- No documentation found for TopLevel "VkFragmentShadingRateCombinerOpKHR"
newtype FragmentShadingRateCombinerOpKHR = FragmentShadingRateCombinerOpKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFragmentShadingRateCombinerOpKHR" "VK_FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR"
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR    = FragmentShadingRateCombinerOpKHR 0
-- No documentation found for Nested "VkFragmentShadingRateCombinerOpKHR" "VK_FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR"
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR = FragmentShadingRateCombinerOpKHR 1
-- No documentation found for Nested "VkFragmentShadingRateCombinerOpKHR" "VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR"
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR     = FragmentShadingRateCombinerOpKHR 2
-- No documentation found for Nested "VkFragmentShadingRateCombinerOpKHR" "VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR"
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR     = FragmentShadingRateCombinerOpKHR 3
-- No documentation found for Nested "VkFragmentShadingRateCombinerOpKHR" "VK_FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR"
pattern FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR     = FragmentShadingRateCombinerOpKHR 4
{-# complete FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR,
             FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR :: FragmentShadingRateCombinerOpKHR #-}

conNameFragmentShadingRateCombinerOpKHR :: String
conNameFragmentShadingRateCombinerOpKHR = "FragmentShadingRateCombinerOpKHR"

enumPrefixFragmentShadingRateCombinerOpKHR :: String
enumPrefixFragmentShadingRateCombinerOpKHR = "FRAGMENT_SHADING_RATE_COMBINER_OP_"

showTableFragmentShadingRateCombinerOpKHR :: [(FragmentShadingRateCombinerOpKHR, String)]
showTableFragmentShadingRateCombinerOpKHR =
  [ (FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR   , "KEEP_KHR")
  , (FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR, "REPLACE_KHR")
  , (FRAGMENT_SHADING_RATE_COMBINER_OP_MIN_KHR    , "MIN_KHR")
  , (FRAGMENT_SHADING_RATE_COMBINER_OP_MAX_KHR    , "MAX_KHR")
  , (FRAGMENT_SHADING_RATE_COMBINER_OP_MUL_KHR    , "MUL_KHR")
  ]


instance Show FragmentShadingRateCombinerOpKHR where
showsPrec = enumShowsPrec enumPrefixFragmentShadingRateCombinerOpKHR
                          showTableFragmentShadingRateCombinerOpKHR
                          conNameFragmentShadingRateCombinerOpKHR
                          (\(FragmentShadingRateCombinerOpKHR x) -> x)
                          (showsPrec 11)


instance Read FragmentShadingRateCombinerOpKHR where
  readPrec = enumReadPrec enumPrefixFragmentShadingRateCombinerOpKHR
                          showTableFragmentShadingRateCombinerOpKHR
                          conNameFragmentShadingRateCombinerOpKHR
                          FragmentShadingRateCombinerOpKHR


type KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION"
pattern KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION = 1


type KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME = "VK_KHR_fragment_shading_rate"

-- No documentation found for TopLevel "VK_KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME"
pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME = "VK_KHR_fragment_shading_rate"

