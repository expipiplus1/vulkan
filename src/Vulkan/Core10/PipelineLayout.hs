{-# language CPP #-}
-- No documentation found for Chapter "PipelineLayout"
module Vulkan.Core10.PipelineLayout  ( createPipelineLayout
                                     , withPipelineLayout
                                     , destroyPipelineLayout
                                     , PushConstantRange(..)
                                     , PipelineLayoutCreateInfo(..)
                                     , PipelineLayout(..)
                                     ) where

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
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreatePipelineLayout))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPipelineLayout))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.Core10.Enums.PipelineLayoutCreateFlags (PipelineLayoutCreateFlags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (PipelineLayout(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineLayout
  :: FunPtr (Ptr Device_T -> Ptr PipelineLayoutCreateInfo -> Ptr AllocationCallbacks -> Ptr PipelineLayout -> IO Result) -> Ptr Device_T -> Ptr PipelineLayoutCreateInfo -> Ptr AllocationCallbacks -> Ptr PipelineLayout -> IO Result

-- No documentation found for TopLevel "vkCreatePipelineLayout"
createPipelineLayout :: forall io
                      . (MonadIO io)
                     => -- No documentation found for Nested "vkCreatePipelineLayout" "device"
                        Device
                     -> -- No documentation found for Nested "vkCreatePipelineLayout" "pCreateInfo"
                        PipelineLayoutCreateInfo
                     -> -- No documentation found for Nested "vkCreatePipelineLayout" "pAllocator"
                        ("allocator" ::: Maybe AllocationCallbacks)
                     -> io (PipelineLayout)
createPipelineLayout device createInfo allocator = liftIO . evalContT $ do
  let vkCreatePipelineLayoutPtr = pVkCreatePipelineLayout (deviceCmds (device :: Device))
  lift $ unless (vkCreatePipelineLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreatePipelineLayout is null" Nothing Nothing
  let vkCreatePipelineLayout' = mkVkCreatePipelineLayout vkCreatePipelineLayoutPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelineLayout <- ContT $ bracket (callocBytes @PipelineLayout 8) free
  r <- lift $ vkCreatePipelineLayout' (deviceHandle (device)) pCreateInfo pAllocator (pPPipelineLayout)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelineLayout <- lift $ peek @PipelineLayout pPPipelineLayout
  pure $ (pPipelineLayout)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createPipelineLayout' and 'destroyPipelineLayout'
--
-- To ensure that 'destroyPipelineLayout' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withPipelineLayout :: forall io r . MonadIO io => Device -> PipelineLayoutCreateInfo -> Maybe AllocationCallbacks -> (io PipelineLayout -> (PipelineLayout -> io ()) -> r) -> r
withPipelineLayout device pCreateInfo pAllocator b =
  b (createPipelineLayout device pCreateInfo pAllocator)
    (\(o0) -> destroyPipelineLayout device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineLayout
  :: FunPtr (Ptr Device_T -> PipelineLayout -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> PipelineLayout -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyPipelineLayout"
destroyPipelineLayout :: forall io
                       . (MonadIO io)
                      => -- No documentation found for Nested "vkDestroyPipelineLayout" "device"
                         Device
                      -> -- No documentation found for Nested "vkDestroyPipelineLayout" "pipelineLayout"
                         PipelineLayout
                      -> -- No documentation found for Nested "vkDestroyPipelineLayout" "pAllocator"
                         ("allocator" ::: Maybe AllocationCallbacks)
                      -> io ()
destroyPipelineLayout device pipelineLayout allocator = liftIO . evalContT $ do
  let vkDestroyPipelineLayoutPtr = pVkDestroyPipelineLayout (deviceCmds (device :: Device))
  lift $ unless (vkDestroyPipelineLayoutPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPipelineLayout is null" Nothing Nothing
  let vkDestroyPipelineLayout' = mkVkDestroyPipelineLayout vkDestroyPipelineLayoutPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyPipelineLayout' (deviceHandle (device)) (pipelineLayout) pAllocator
  pure $ ()



-- No documentation found for TopLevel "VkPushConstantRange"
data PushConstantRange = PushConstantRange
  { -- No documentation found for Nested "VkPushConstantRange" "stageFlags"
    stageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "VkPushConstantRange" "offset"
    offset :: Word32
  , -- No documentation found for Nested "VkPushConstantRange" "size"
    size :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PushConstantRange)
#endif
deriving instance Show PushConstantRange

instance ToCStruct PushConstantRange where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PushConstantRange{..} f = do
    poke ((p `plusPtr` 0 :: Ptr ShaderStageFlags)) (stageFlags)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (offset)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (size)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr ShaderStageFlags)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct PushConstantRange where
  peekCStruct p = do
    stageFlags <- peek @ShaderStageFlags ((p `plusPtr` 0 :: Ptr ShaderStageFlags))
    offset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    size <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ PushConstantRange
             stageFlags offset size


instance Storable PushConstantRange where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PushConstantRange where
  zero = PushConstantRange
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPipelineLayoutCreateInfo"
data PipelineLayoutCreateInfo = PipelineLayoutCreateInfo
  { -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "flags"
    flags :: PipelineLayoutCreateFlags
  , -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "pSetLayouts"
    setLayouts :: Vector DescriptorSetLayout
  , -- No documentation found for Nested "VkPipelineLayoutCreateInfo" "pPushConstantRanges"
    pushConstantRanges :: Vector PushConstantRange
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineLayoutCreateInfo)
#endif
deriving instance Show PipelineLayoutCreateInfo

instance ToCStruct PipelineLayoutCreateInfo where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineLayoutCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineLayoutCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (setLayouts)) :: Word32))
    pPSetLayouts' <- ContT $ allocaBytesAligned @DescriptorSetLayout ((Data.Vector.length (setLayouts)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSetLayouts' `plusPtr` (8 * (i)) :: Ptr DescriptorSetLayout) (e)) (setLayouts)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayout))) (pPSetLayouts')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (pushConstantRanges)) :: Word32))
    pPPushConstantRanges' <- ContT $ allocaBytesAligned @PushConstantRange ((Data.Vector.length (pushConstantRanges)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPushConstantRanges' `plusPtr` (12 * (i)) :: Ptr PushConstantRange) (e)) (pushConstantRanges)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr PushConstantRange))) (pPPushConstantRanges')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPSetLayouts' <- ContT $ allocaBytesAligned @DescriptorSetLayout ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPSetLayouts' `plusPtr` (8 * (i)) :: Ptr DescriptorSetLayout) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayout))) (pPSetLayouts')
    pPPushConstantRanges' <- ContT $ allocaBytesAligned @PushConstantRange ((Data.Vector.length (mempty)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPPushConstantRanges' `plusPtr` (12 * (i)) :: Ptr PushConstantRange) (e)) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr PushConstantRange))) (pPPushConstantRanges')
    lift $ f

instance FromCStruct PipelineLayoutCreateInfo where
  peekCStruct p = do
    flags <- peek @PipelineLayoutCreateFlags ((p `plusPtr` 16 :: Ptr PipelineLayoutCreateFlags))
    setLayoutCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pSetLayouts <- peek @(Ptr DescriptorSetLayout) ((p `plusPtr` 24 :: Ptr (Ptr DescriptorSetLayout)))
    pSetLayouts' <- generateM (fromIntegral setLayoutCount) (\i -> peek @DescriptorSetLayout ((pSetLayouts `advancePtrBytes` (8 * (i)) :: Ptr DescriptorSetLayout)))
    pushConstantRangeCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPushConstantRanges <- peek @(Ptr PushConstantRange) ((p `plusPtr` 40 :: Ptr (Ptr PushConstantRange)))
    pPushConstantRanges' <- generateM (fromIntegral pushConstantRangeCount) (\i -> peekCStruct @PushConstantRange ((pPushConstantRanges `advancePtrBytes` (12 * (i)) :: Ptr PushConstantRange)))
    pure $ PipelineLayoutCreateInfo
             flags pSetLayouts' pPushConstantRanges'

instance Zero PipelineLayoutCreateInfo where
  zero = PipelineLayoutCreateInfo
           zero
           mempty
           mempty

