{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_sample_locations"
module Vulkan.Extensions.VK_EXT_sample_locations  ( cmdSetSampleLocationsEXT
                                                  , getPhysicalDeviceMultisamplePropertiesEXT
                                                  , SampleLocationEXT(..)
                                                  , SampleLocationsInfoEXT(..)
                                                  , AttachmentSampleLocationsEXT(..)
                                                  , SubpassSampleLocationsEXT(..)
                                                  , RenderPassSampleLocationsBeginInfoEXT(..)
                                                  , PipelineSampleLocationsStateCreateInfoEXT(..)
                                                  , PhysicalDeviceSampleLocationsPropertiesEXT(..)
                                                  , MultisamplePropertiesEXT(..)
                                                  , EXT_SAMPLE_LOCATIONS_SPEC_VERSION
                                                  , pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION
                                                  , EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
                                                  , pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME
                                                  ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
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
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetSampleLocationsEXT))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceMultisamplePropertiesEXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetSampleLocationsEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr SampleLocationsInfoEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr SampleLocationsInfoEXT -> IO ()

-- No documentation found for TopLevel "vkCmdSetSampleLocationsEXT"
cmdSetSampleLocationsEXT :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCmdSetSampleLocationsEXT" "commandBuffer"
                            CommandBuffer
                         -> -- No documentation found for Nested "vkCmdSetSampleLocationsEXT" "pSampleLocationsInfo"
                            SampleLocationsInfoEXT
                         -> io ()
cmdSetSampleLocationsEXT commandBuffer sampleLocationsInfo = liftIO . evalContT $ do
  let vkCmdSetSampleLocationsEXTPtr = pVkCmdSetSampleLocationsEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetSampleLocationsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetSampleLocationsEXT is null" Nothing Nothing
  let vkCmdSetSampleLocationsEXT' = mkVkCmdSetSampleLocationsEXT vkCmdSetSampleLocationsEXTPtr
  pSampleLocationsInfo <- ContT $ withCStruct (sampleLocationsInfo)
  lift $ vkCmdSetSampleLocationsEXT' (commandBufferHandle (commandBuffer)) pSampleLocationsInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMultisamplePropertiesEXT
  :: FunPtr (Ptr PhysicalDevice_T -> SampleCountFlagBits -> Ptr MultisamplePropertiesEXT -> IO ()) -> Ptr PhysicalDevice_T -> SampleCountFlagBits -> Ptr MultisamplePropertiesEXT -> IO ()

-- No documentation found for TopLevel "vkGetPhysicalDeviceMultisamplePropertiesEXT"
getPhysicalDeviceMultisamplePropertiesEXT :: forall io
                                           . (MonadIO io)
                                          => -- No documentation found for Nested "vkGetPhysicalDeviceMultisamplePropertiesEXT" "physicalDevice"
                                             PhysicalDevice
                                          -> -- No documentation found for Nested "vkGetPhysicalDeviceMultisamplePropertiesEXT" "samples"
                                             ("samples" ::: SampleCountFlagBits)
                                          -> io (MultisamplePropertiesEXT)
getPhysicalDeviceMultisamplePropertiesEXT physicalDevice samples = liftIO . evalContT $ do
  let vkGetPhysicalDeviceMultisamplePropertiesEXTPtr = pVkGetPhysicalDeviceMultisamplePropertiesEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceMultisamplePropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceMultisamplePropertiesEXT is null" Nothing Nothing
  let vkGetPhysicalDeviceMultisamplePropertiesEXT' = mkVkGetPhysicalDeviceMultisamplePropertiesEXT vkGetPhysicalDeviceMultisamplePropertiesEXTPtr
  pPMultisampleProperties <- ContT (withZeroCStruct @MultisamplePropertiesEXT)
  lift $ vkGetPhysicalDeviceMultisamplePropertiesEXT' (physicalDeviceHandle (physicalDevice)) (samples) (pPMultisampleProperties)
  pMultisampleProperties <- lift $ peekCStruct @MultisamplePropertiesEXT pPMultisampleProperties
  pure $ (pMultisampleProperties)



-- No documentation found for TopLevel "VkSampleLocationEXT"
data SampleLocationEXT = SampleLocationEXT
  { -- No documentation found for Nested "VkSampleLocationEXT" "x"
    x :: Float
  , -- No documentation found for Nested "VkSampleLocationEXT" "y"
    y :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SampleLocationEXT)
#endif
deriving instance Show SampleLocationEXT

instance ToCStruct SampleLocationEXT where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SampleLocationEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct SampleLocationEXT where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    pure $ SampleLocationEXT
             ((\(CFloat a) -> a) x) ((\(CFloat a) -> a) y)


instance Storable SampleLocationEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SampleLocationEXT where
  zero = SampleLocationEXT
           zero
           zero



-- No documentation found for TopLevel "VkSampleLocationsInfoEXT"
data SampleLocationsInfoEXT = SampleLocationsInfoEXT
  { -- No documentation found for Nested "VkSampleLocationsInfoEXT" "sampleLocationsPerPixel"
    sampleLocationsPerPixel :: SampleCountFlagBits
  , -- No documentation found for Nested "VkSampleLocationsInfoEXT" "sampleLocationGridSize"
    sampleLocationGridSize :: Extent2D
  , -- No documentation found for Nested "VkSampleLocationsInfoEXT" "pSampleLocations"
    sampleLocations :: Vector SampleLocationEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SampleLocationsInfoEXT)
#endif
deriving instance Show SampleLocationsInfoEXT

instance ToCStruct SampleLocationsInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SampleLocationsInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr SampleCountFlagBits)) (sampleLocationsPerPixel)
    lift $ poke ((p `plusPtr` 20 :: Ptr Extent2D)) (sampleLocationGridSize)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (sampleLocations)) :: Word32))
    pPSampleLocations' <- ContT $ allocaBytesAligned @SampleLocationEXT ((Data.Vector.length (sampleLocations)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSampleLocations' `plusPtr` (8 * (i)) :: Ptr SampleLocationEXT) (e)) (sampleLocations)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SampleLocationEXT))) (pPSampleLocations')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 20 :: Ptr Extent2D)) (zero)
    pPSampleLocations' <- ContT $ allocaBytesAligned @SampleLocationEXT ((Data.Vector.length (mempty)) * 8) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSampleLocations' `plusPtr` (8 * (i)) :: Ptr SampleLocationEXT) (e)) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SampleLocationEXT))) (pPSampleLocations')
    lift $ f

instance FromCStruct SampleLocationsInfoEXT where
  peekCStruct p = do
    sampleLocationsPerPixel <- peek @SampleCountFlagBits ((p `plusPtr` 16 :: Ptr SampleCountFlagBits))
    sampleLocationGridSize <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    sampleLocationsCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pSampleLocations <- peek @(Ptr SampleLocationEXT) ((p `plusPtr` 32 :: Ptr (Ptr SampleLocationEXT)))
    pSampleLocations' <- generateM (fromIntegral sampleLocationsCount) (\i -> peekCStruct @SampleLocationEXT ((pSampleLocations `advancePtrBytes` (8 * (i)) :: Ptr SampleLocationEXT)))
    pure $ SampleLocationsInfoEXT
             sampleLocationsPerPixel sampleLocationGridSize pSampleLocations'

instance Zero SampleLocationsInfoEXT where
  zero = SampleLocationsInfoEXT
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkAttachmentSampleLocationsEXT"
data AttachmentSampleLocationsEXT = AttachmentSampleLocationsEXT
  { -- No documentation found for Nested "VkAttachmentSampleLocationsEXT" "attachmentIndex"
    attachmentIndex :: Word32
  , -- No documentation found for Nested "VkAttachmentSampleLocationsEXT" "sampleLocationsInfo"
    sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentSampleLocationsEXT)
#endif
deriving instance Show AttachmentSampleLocationsEXT

instance ToCStruct AttachmentSampleLocationsEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentSampleLocationsEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (attachmentIndex)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT)) (sampleLocationsInfo) . ($ ())
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT)) (zero) . ($ ())
    lift $ f

instance FromCStruct AttachmentSampleLocationsEXT where
  peekCStruct p = do
    attachmentIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    sampleLocationsInfo <- peekCStruct @SampleLocationsInfoEXT ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT))
    pure $ AttachmentSampleLocationsEXT
             attachmentIndex sampleLocationsInfo

instance Zero AttachmentSampleLocationsEXT where
  zero = AttachmentSampleLocationsEXT
           zero
           zero



-- No documentation found for TopLevel "VkSubpassSampleLocationsEXT"
data SubpassSampleLocationsEXT = SubpassSampleLocationsEXT
  { -- No documentation found for Nested "VkSubpassSampleLocationsEXT" "subpassIndex"
    subpassIndex :: Word32
  , -- No documentation found for Nested "VkSubpassSampleLocationsEXT" "sampleLocationsInfo"
    sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubpassSampleLocationsEXT)
#endif
deriving instance Show SubpassSampleLocationsEXT

instance ToCStruct SubpassSampleLocationsEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubpassSampleLocationsEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (subpassIndex)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT)) (sampleLocationsInfo) . ($ ())
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT)) (zero) . ($ ())
    lift $ f

instance FromCStruct SubpassSampleLocationsEXT where
  peekCStruct p = do
    subpassIndex <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    sampleLocationsInfo <- peekCStruct @SampleLocationsInfoEXT ((p `plusPtr` 8 :: Ptr SampleLocationsInfoEXT))
    pure $ SubpassSampleLocationsEXT
             subpassIndex sampleLocationsInfo

instance Zero SubpassSampleLocationsEXT where
  zero = SubpassSampleLocationsEXT
           zero
           zero



-- No documentation found for TopLevel "VkRenderPassSampleLocationsBeginInfoEXT"
data RenderPassSampleLocationsBeginInfoEXT = RenderPassSampleLocationsBeginInfoEXT
  { -- No documentation found for Nested "VkRenderPassSampleLocationsBeginInfoEXT" "pAttachmentInitialSampleLocations"
    attachmentInitialSampleLocations :: Vector AttachmentSampleLocationsEXT
  , -- No documentation found for Nested "VkRenderPassSampleLocationsBeginInfoEXT" "pPostSubpassSampleLocations"
    postSubpassSampleLocations :: Vector SubpassSampleLocationsEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassSampleLocationsBeginInfoEXT)
#endif
deriving instance Show RenderPassSampleLocationsBeginInfoEXT

instance ToCStruct RenderPassSampleLocationsBeginInfoEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassSampleLocationsBeginInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachmentInitialSampleLocations)) :: Word32))
    pPAttachmentInitialSampleLocations' <- ContT $ allocaBytesAligned @AttachmentSampleLocationsEXT ((Data.Vector.length (attachmentInitialSampleLocations)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachmentInitialSampleLocations' `plusPtr` (48 * (i)) :: Ptr AttachmentSampleLocationsEXT) (e) . ($ ())) (attachmentInitialSampleLocations)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AttachmentSampleLocationsEXT))) (pPAttachmentInitialSampleLocations')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (postSubpassSampleLocations)) :: Word32))
    pPPostSubpassSampleLocations' <- ContT $ allocaBytesAligned @SubpassSampleLocationsEXT ((Data.Vector.length (postSubpassSampleLocations)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPPostSubpassSampleLocations' `plusPtr` (48 * (i)) :: Ptr SubpassSampleLocationsEXT) (e) . ($ ())) (postSubpassSampleLocations)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SubpassSampleLocationsEXT))) (pPPostSubpassSampleLocations')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPAttachmentInitialSampleLocations' <- ContT $ allocaBytesAligned @AttachmentSampleLocationsEXT ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachmentInitialSampleLocations' `plusPtr` (48 * (i)) :: Ptr AttachmentSampleLocationsEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AttachmentSampleLocationsEXT))) (pPAttachmentInitialSampleLocations')
    pPPostSubpassSampleLocations' <- ContT $ allocaBytesAligned @SubpassSampleLocationsEXT ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPPostSubpassSampleLocations' `plusPtr` (48 * (i)) :: Ptr SubpassSampleLocationsEXT) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SubpassSampleLocationsEXT))) (pPPostSubpassSampleLocations')
    lift $ f

instance FromCStruct RenderPassSampleLocationsBeginInfoEXT where
  peekCStruct p = do
    attachmentInitialSampleLocationsCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAttachmentInitialSampleLocations <- peek @(Ptr AttachmentSampleLocationsEXT) ((p `plusPtr` 24 :: Ptr (Ptr AttachmentSampleLocationsEXT)))
    pAttachmentInitialSampleLocations' <- generateM (fromIntegral attachmentInitialSampleLocationsCount) (\i -> peekCStruct @AttachmentSampleLocationsEXT ((pAttachmentInitialSampleLocations `advancePtrBytes` (48 * (i)) :: Ptr AttachmentSampleLocationsEXT)))
    postSubpassSampleLocationsCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPostSubpassSampleLocations <- peek @(Ptr SubpassSampleLocationsEXT) ((p `plusPtr` 40 :: Ptr (Ptr SubpassSampleLocationsEXT)))
    pPostSubpassSampleLocations' <- generateM (fromIntegral postSubpassSampleLocationsCount) (\i -> peekCStruct @SubpassSampleLocationsEXT ((pPostSubpassSampleLocations `advancePtrBytes` (48 * (i)) :: Ptr SubpassSampleLocationsEXT)))
    pure $ RenderPassSampleLocationsBeginInfoEXT
             pAttachmentInitialSampleLocations' pPostSubpassSampleLocations'

instance Zero RenderPassSampleLocationsBeginInfoEXT where
  zero = RenderPassSampleLocationsBeginInfoEXT
           mempty
           mempty



-- No documentation found for TopLevel "VkPipelineSampleLocationsStateCreateInfoEXT"
data PipelineSampleLocationsStateCreateInfoEXT = PipelineSampleLocationsStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsEnable"
    sampleLocationsEnable :: Bool
  , -- No documentation found for Nested "VkPipelineSampleLocationsStateCreateInfoEXT" "sampleLocationsInfo"
    sampleLocationsInfo :: SampleLocationsInfoEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineSampleLocationsStateCreateInfoEXT)
#endif
deriving instance Show PipelineSampleLocationsStateCreateInfoEXT

instance ToCStruct PipelineSampleLocationsStateCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineSampleLocationsStateCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (sampleLocationsEnable))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr SampleLocationsInfoEXT)) (sampleLocationsInfo) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr SampleLocationsInfoEXT)) (zero) . ($ ())
    lift $ f

instance FromCStruct PipelineSampleLocationsStateCreateInfoEXT where
  peekCStruct p = do
    sampleLocationsEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    sampleLocationsInfo <- peekCStruct @SampleLocationsInfoEXT ((p `plusPtr` 24 :: Ptr SampleLocationsInfoEXT))
    pure $ PipelineSampleLocationsStateCreateInfoEXT
             (bool32ToBool sampleLocationsEnable) sampleLocationsInfo

instance Zero PipelineSampleLocationsStateCreateInfoEXT where
  zero = PipelineSampleLocationsStateCreateInfoEXT
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceSampleLocationsPropertiesEXT"
data PhysicalDeviceSampleLocationsPropertiesEXT = PhysicalDeviceSampleLocationsPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSampleCounts"
    sampleLocationSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "maxSampleLocationGridSize"
    maxSampleLocationGridSize :: Extent2D
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationCoordinateRange"
    sampleLocationCoordinateRange :: (Float, Float)
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "sampleLocationSubPixelBits"
    sampleLocationSubPixelBits :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSampleLocationsPropertiesEXT" "variableSampleLocations"
    variableSampleLocations :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSampleLocationsPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceSampleLocationsPropertiesEXT

instance ToCStruct PhysicalDeviceSampleLocationsPropertiesEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSampleLocationsPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (sampleLocationSampleCounts)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (maxSampleLocationGridSize)
    let pSampleLocationCoordinateRange' = lowerArrayPtr ((p `plusPtr` 28 :: Ptr (FixedArray 2 CFloat)))
    case (sampleLocationCoordinateRange) of
      (e0, e1) -> do
        poke (pSampleLocationCoordinateRange' :: Ptr CFloat) (CFloat (e0))
        poke (pSampleLocationCoordinateRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    poke ((p `plusPtr` 36 :: Ptr Word32)) (sampleLocationSubPixelBits)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (variableSampleLocations))
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlags)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (zero)
    let pSampleLocationCoordinateRange' = lowerArrayPtr ((p `plusPtr` 28 :: Ptr (FixedArray 2 CFloat)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pSampleLocationCoordinateRange' :: Ptr CFloat) (CFloat (e0))
        poke (pSampleLocationCoordinateRange' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSampleLocationsPropertiesEXT where
  peekCStruct p = do
    sampleLocationSampleCounts <- peek @SampleCountFlags ((p `plusPtr` 16 :: Ptr SampleCountFlags))
    maxSampleLocationGridSize <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    let psampleLocationCoordinateRange = lowerArrayPtr @CFloat ((p `plusPtr` 28 :: Ptr (FixedArray 2 CFloat)))
    sampleLocationCoordinateRange0 <- peek @CFloat ((psampleLocationCoordinateRange `advancePtrBytes` 0 :: Ptr CFloat))
    sampleLocationCoordinateRange1 <- peek @CFloat ((psampleLocationCoordinateRange `advancePtrBytes` 4 :: Ptr CFloat))
    sampleLocationSubPixelBits <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    variableSampleLocations <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ PhysicalDeviceSampleLocationsPropertiesEXT
             sampleLocationSampleCounts maxSampleLocationGridSize ((((\(CFloat a) -> a) sampleLocationCoordinateRange0), ((\(CFloat a) -> a) sampleLocationCoordinateRange1))) sampleLocationSubPixelBits (bool32ToBool variableSampleLocations)


instance Storable PhysicalDeviceSampleLocationsPropertiesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSampleLocationsPropertiesEXT where
  zero = PhysicalDeviceSampleLocationsPropertiesEXT
           zero
           zero
           (zero, zero)
           zero
           zero



-- No documentation found for TopLevel "VkMultisamplePropertiesEXT"
data MultisamplePropertiesEXT = MultisamplePropertiesEXT
  { -- No documentation found for Nested "VkMultisamplePropertiesEXT" "maxSampleLocationGridSize"
    maxSampleLocationGridSize :: Extent2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MultisamplePropertiesEXT)
#endif
deriving instance Show MultisamplePropertiesEXT

instance ToCStruct MultisamplePropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MultisamplePropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (maxSampleLocationGridSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct MultisamplePropertiesEXT where
  peekCStruct p = do
    maxSampleLocationGridSize <- peekCStruct @Extent2D ((p `plusPtr` 16 :: Ptr Extent2D))
    pure $ MultisamplePropertiesEXT
             maxSampleLocationGridSize


instance Storable MultisamplePropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MultisamplePropertiesEXT where
  zero = MultisamplePropertiesEXT
           zero


type EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_SPEC_VERSION"
pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SAMPLE_LOCATIONS_SPEC_VERSION = 1


type EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = "VK_EXT_sample_locations"

-- No documentation found for TopLevel "VK_EXT_SAMPLE_LOCATIONS_EXTENSION_NAME"
pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SAMPLE_LOCATIONS_EXTENSION_NAME = "VK_EXT_sample_locations"

