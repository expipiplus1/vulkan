{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_descriptor_update_template"
module Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template  ( createDescriptorUpdateTemplate
                                                                      , withDescriptorUpdateTemplate
                                                                      , destroyDescriptorUpdateTemplate
                                                                      , updateDescriptorSetWithTemplate
                                                                      , DescriptorUpdateTemplateEntry(..)
                                                                      , DescriptorUpdateTemplateCreateInfo(..)
                                                                      , DescriptorUpdateTemplate(..)
                                                                      , DescriptorUpdateTemplateCreateFlags(..)
                                                                      , StructureType(..)
                                                                      , DescriptorUpdateTemplateType(..)
                                                                      , ObjectType(..)
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
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (DescriptorSet)
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags (DescriptorUpdateTemplateCreateFlags)
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateDescriptorUpdateTemplate))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDescriptorUpdateTemplate))
import Vulkan.Dynamic (DeviceCmds(pVkUpdateDescriptorSetWithTemplate))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags (DescriptorUpdateTemplateCreateFlags(..))
import Vulkan.Core11.Enums.DescriptorUpdateTemplateType (DescriptorUpdateTemplateType(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType(..))
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorUpdateTemplate
  :: FunPtr (Ptr Device_T -> Ptr DescriptorUpdateTemplateCreateInfo -> Ptr AllocationCallbacks -> Ptr DescriptorUpdateTemplate -> IO Result) -> Ptr Device_T -> Ptr DescriptorUpdateTemplateCreateInfo -> Ptr AllocationCallbacks -> Ptr DescriptorUpdateTemplate -> IO Result

-- No documentation found for TopLevel "vkCreateDescriptorUpdateTemplate"
createDescriptorUpdateTemplate :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "vkCreateDescriptorUpdateTemplate" "device"
                                  Device
                               -> -- No documentation found for Nested "vkCreateDescriptorUpdateTemplate" "pCreateInfo"
                                  DescriptorUpdateTemplateCreateInfo
                               -> -- No documentation found for Nested "vkCreateDescriptorUpdateTemplate" "pAllocator"
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io (DescriptorUpdateTemplate)
createDescriptorUpdateTemplate device createInfo allocator = liftIO . evalContT $ do
  let vkCreateDescriptorUpdateTemplatePtr = pVkCreateDescriptorUpdateTemplate (deviceCmds (device :: Device))
  lift $ unless (vkCreateDescriptorUpdateTemplatePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDescriptorUpdateTemplate is null" Nothing Nothing
  let vkCreateDescriptorUpdateTemplate' = mkVkCreateDescriptorUpdateTemplate vkCreateDescriptorUpdateTemplatePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDescriptorUpdateTemplate <- ContT $ bracket (callocBytes @DescriptorUpdateTemplate 8) free
  r <- lift $ vkCreateDescriptorUpdateTemplate' (deviceHandle (device)) pCreateInfo pAllocator (pPDescriptorUpdateTemplate)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDescriptorUpdateTemplate <- lift $ peek @DescriptorUpdateTemplate pPDescriptorUpdateTemplate
  pure $ (pDescriptorUpdateTemplate)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDescriptorUpdateTemplate' and 'destroyDescriptorUpdateTemplate'
--
-- To ensure that 'destroyDescriptorUpdateTemplate' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDescriptorUpdateTemplate :: forall io r . MonadIO io => Device -> DescriptorUpdateTemplateCreateInfo -> Maybe AllocationCallbacks -> (io DescriptorUpdateTemplate -> (DescriptorUpdateTemplate -> io ()) -> r) -> r
withDescriptorUpdateTemplate device pCreateInfo pAllocator b =
  b (createDescriptorUpdateTemplate device pCreateInfo pAllocator)
    (\(o0) -> destroyDescriptorUpdateTemplate device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorUpdateTemplate
  :: FunPtr (Ptr Device_T -> DescriptorUpdateTemplate -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> DescriptorUpdateTemplate -> Ptr AllocationCallbacks -> IO ()

-- No documentation found for TopLevel "vkDestroyDescriptorUpdateTemplate"
destroyDescriptorUpdateTemplate :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkDestroyDescriptorUpdateTemplate" "device"
                                   Device
                                -> -- No documentation found for Nested "vkDestroyDescriptorUpdateTemplate" "descriptorUpdateTemplate"
                                   DescriptorUpdateTemplate
                                -> -- No documentation found for Nested "vkDestroyDescriptorUpdateTemplate" "pAllocator"
                                   ("allocator" ::: Maybe AllocationCallbacks)
                                -> io ()
destroyDescriptorUpdateTemplate device descriptorUpdateTemplate allocator = liftIO . evalContT $ do
  let vkDestroyDescriptorUpdateTemplatePtr = pVkDestroyDescriptorUpdateTemplate (deviceCmds (device :: Device))
  lift $ unless (vkDestroyDescriptorUpdateTemplatePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDescriptorUpdateTemplate is null" Nothing Nothing
  let vkDestroyDescriptorUpdateTemplate' = mkVkDestroyDescriptorUpdateTemplate vkDestroyDescriptorUpdateTemplatePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyDescriptorUpdateTemplate' (deviceHandle (device)) (descriptorUpdateTemplate) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSetWithTemplate
  :: FunPtr (Ptr Device_T -> DescriptorSet -> DescriptorUpdateTemplate -> Ptr () -> IO ()) -> Ptr Device_T -> DescriptorSet -> DescriptorUpdateTemplate -> Ptr () -> IO ()

-- No documentation found for TopLevel "vkUpdateDescriptorSetWithTemplate"
updateDescriptorSetWithTemplate :: forall io
                                 . (MonadIO io)
                                => -- No documentation found for Nested "vkUpdateDescriptorSetWithTemplate" "device"
                                   Device
                                -> -- No documentation found for Nested "vkUpdateDescriptorSetWithTemplate" "descriptorSet"
                                   DescriptorSet
                                -> -- No documentation found for Nested "vkUpdateDescriptorSetWithTemplate" "descriptorUpdateTemplate"
                                   DescriptorUpdateTemplate
                                -> -- No documentation found for Nested "vkUpdateDescriptorSetWithTemplate" "pData"
                                   ("data" ::: Ptr ())
                                -> io ()
updateDescriptorSetWithTemplate device descriptorSet descriptorUpdateTemplate data' = liftIO $ do
  let vkUpdateDescriptorSetWithTemplatePtr = pVkUpdateDescriptorSetWithTemplate (deviceCmds (device :: Device))
  unless (vkUpdateDescriptorSetWithTemplatePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkUpdateDescriptorSetWithTemplate is null" Nothing Nothing
  let vkUpdateDescriptorSetWithTemplate' = mkVkUpdateDescriptorSetWithTemplate vkUpdateDescriptorSetWithTemplatePtr
  vkUpdateDescriptorSetWithTemplate' (deviceHandle (device)) (descriptorSet) (descriptorUpdateTemplate) (data')
  pure $ ()



-- No documentation found for TopLevel "VkDescriptorUpdateTemplateEntry"
data DescriptorUpdateTemplateEntry = DescriptorUpdateTemplateEntry
  { -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "dstBinding"
    dstBinding :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "dstArrayElement"
    dstArrayElement :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "descriptorCount"
    descriptorCount :: Word32
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "descriptorType"
    descriptorType :: DescriptorType
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "offset"
    offset :: Word64
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateEntry" "stride"
    stride :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorUpdateTemplateEntry)
#endif
deriving instance Show DescriptorUpdateTemplateEntry

instance ToCStruct DescriptorUpdateTemplateEntry where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorUpdateTemplateEntry{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (dstBinding)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (dstArrayElement)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (descriptorCount)
    poke ((p `plusPtr` 12 :: Ptr DescriptorType)) (descriptorType)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (offset))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (stride))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr DescriptorType)) (zero)
    poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 24 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct DescriptorUpdateTemplateEntry where
  peekCStruct p = do
    dstBinding <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    dstArrayElement <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    descriptorCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    descriptorType <- peek @DescriptorType ((p `plusPtr` 12 :: Ptr DescriptorType))
    offset <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    stride <- peek @CSize ((p `plusPtr` 24 :: Ptr CSize))
    pure $ DescriptorUpdateTemplateEntry
             dstBinding dstArrayElement descriptorCount descriptorType ((\(CSize a) -> a) offset) ((\(CSize a) -> a) stride)


instance Storable DescriptorUpdateTemplateEntry where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DescriptorUpdateTemplateEntry where
  zero = DescriptorUpdateTemplateEntry
           zero
           zero
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkDescriptorUpdateTemplateCreateInfo"
data DescriptorUpdateTemplateCreateInfo = DescriptorUpdateTemplateCreateInfo
  { -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "flags"
    flags :: DescriptorUpdateTemplateCreateFlags
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "pDescriptorUpdateEntries"
    descriptorUpdateEntries :: Vector DescriptorUpdateTemplateEntry
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "templateType"
    templateType :: DescriptorUpdateTemplateType
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "descriptorSetLayout"
    descriptorSetLayout :: DescriptorSetLayout
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "pipelineBindPoint"
    pipelineBindPoint :: PipelineBindPoint
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "pipelineLayout"
    pipelineLayout :: PipelineLayout
  , -- No documentation found for Nested "VkDescriptorUpdateTemplateCreateInfo" "set"
    set :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DescriptorUpdateTemplateCreateInfo)
#endif
deriving instance Show DescriptorUpdateTemplateCreateInfo

instance ToCStruct DescriptorUpdateTemplateCreateInfo where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DescriptorUpdateTemplateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (descriptorUpdateEntries)) :: Word32))
    pPDescriptorUpdateEntries' <- ContT $ allocaBytesAligned @DescriptorUpdateTemplateEntry ((Data.Vector.length (descriptorUpdateEntries)) * 32) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorUpdateEntries' `plusPtr` (32 * (i)) :: Ptr DescriptorUpdateTemplateEntry) (e)) (descriptorUpdateEntries)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorUpdateTemplateEntry))) (pPDescriptorUpdateEntries')
    lift $ poke ((p `plusPtr` 32 :: Ptr DescriptorUpdateTemplateType)) (templateType)
    lift $ poke ((p `plusPtr` 40 :: Ptr DescriptorSetLayout)) (descriptorSetLayout)
    lift $ poke ((p `plusPtr` 48 :: Ptr PipelineBindPoint)) (pipelineBindPoint)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (pipelineLayout)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (set)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDescriptorUpdateEntries' <- ContT $ allocaBytesAligned @DescriptorUpdateTemplateEntry ((Data.Vector.length (mempty)) * 32) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDescriptorUpdateEntries' `plusPtr` (32 * (i)) :: Ptr DescriptorUpdateTemplateEntry) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DescriptorUpdateTemplateEntry))) (pPDescriptorUpdateEntries')
    lift $ poke ((p `plusPtr` 32 :: Ptr DescriptorUpdateTemplateType)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr DescriptorSetLayout)) (zero)
    lift $ poke ((p `plusPtr` 48 :: Ptr PipelineBindPoint)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) (zero)
    lift $ f

instance FromCStruct DescriptorUpdateTemplateCreateInfo where
  peekCStruct p = do
    flags <- peek @DescriptorUpdateTemplateCreateFlags ((p `plusPtr` 16 :: Ptr DescriptorUpdateTemplateCreateFlags))
    descriptorUpdateEntryCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pDescriptorUpdateEntries <- peek @(Ptr DescriptorUpdateTemplateEntry) ((p `plusPtr` 24 :: Ptr (Ptr DescriptorUpdateTemplateEntry)))
    pDescriptorUpdateEntries' <- generateM (fromIntegral descriptorUpdateEntryCount) (\i -> peekCStruct @DescriptorUpdateTemplateEntry ((pDescriptorUpdateEntries `advancePtrBytes` (32 * (i)) :: Ptr DescriptorUpdateTemplateEntry)))
    templateType <- peek @DescriptorUpdateTemplateType ((p `plusPtr` 32 :: Ptr DescriptorUpdateTemplateType))
    descriptorSetLayout <- peek @DescriptorSetLayout ((p `plusPtr` 40 :: Ptr DescriptorSetLayout))
    pipelineBindPoint <- peek @PipelineBindPoint ((p `plusPtr` 48 :: Ptr PipelineBindPoint))
    pipelineLayout <- peek @PipelineLayout ((p `plusPtr` 56 :: Ptr PipelineLayout))
    set <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pure $ DescriptorUpdateTemplateCreateInfo
             flags pDescriptorUpdateEntries' templateType descriptorSetLayout pipelineBindPoint pipelineLayout set

instance Zero DescriptorUpdateTemplateCreateInfo where
  zero = DescriptorUpdateTemplateCreateInfo
           zero
           mempty
           zero
           zero
           zero
           zero
           zero

