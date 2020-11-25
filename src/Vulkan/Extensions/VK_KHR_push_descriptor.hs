{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_push_descriptor"
module Vulkan.Extensions.VK_KHR_push_descriptor  ( cmdPushDescriptorSetKHR
                                                 , cmdPushDescriptorSetWithTemplateKHR
                                                 , PhysicalDevicePushDescriptorPropertiesKHR(..)
                                                 , KHR_PUSH_DESCRIPTOR_SPEC_VERSION
                                                 , pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION
                                                 , KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
                                                 , pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME
                                                 ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate)
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSetKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdPushDescriptorSetWithTemplateKHR))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(..))
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.DescriptorSet (WriteDescriptorSet)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetKHR
  :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> IO ()) -> Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> Word32 -> Word32 -> Ptr (SomeStruct WriteDescriptorSet) -> IO ()

-- No documentation found for TopLevel "vkCmdPushDescriptorSetKHR"
cmdPushDescriptorSetKHR :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCmdPushDescriptorSetKHR" "commandBuffer"
                           CommandBuffer
                        -> -- No documentation found for Nested "vkCmdPushDescriptorSetKHR" "pipelineBindPoint"
                           PipelineBindPoint
                        -> -- No documentation found for Nested "vkCmdPushDescriptorSetKHR" "layout"
                           PipelineLayout
                        -> -- No documentation found for Nested "vkCmdPushDescriptorSetKHR" "set"
                           ("set" ::: Word32)
                        -> -- No documentation found for Nested "vkCmdPushDescriptorSetKHR" "pDescriptorWrites"
                           ("descriptorWrites" ::: Vector (SomeStruct WriteDescriptorSet))
                        -> io ()
cmdPushDescriptorSetKHR commandBuffer pipelineBindPoint layout set descriptorWrites = liftIO . evalContT $ do
  let vkCmdPushDescriptorSetKHRPtr = pVkCmdPushDescriptorSetKHR (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdPushDescriptorSetKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSetKHR is null" Nothing Nothing
  let vkCmdPushDescriptorSetKHR' = mkVkCmdPushDescriptorSetKHR vkCmdPushDescriptorSetKHRPtr
  pPDescriptorWrites <- ContT $ allocaBytesAligned @(WriteDescriptorSet _) ((Data.Vector.length (descriptorWrites)) * 64) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPDescriptorWrites `plusPtr` (64 * (i)) :: Ptr (WriteDescriptorSet _))) (e) . ($ ())) (descriptorWrites)
  lift $ vkCmdPushDescriptorSetKHR' (commandBufferHandle (commandBuffer)) (pipelineBindPoint) (layout) (set) ((fromIntegral (Data.Vector.length $ (descriptorWrites)) :: Word32)) (forgetExtensions (pPDescriptorWrites))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetWithTemplateKHR
  :: FunPtr (Ptr CommandBuffer_T -> DescriptorUpdateTemplate -> PipelineLayout -> Word32 -> Ptr () -> IO ()) -> Ptr CommandBuffer_T -> DescriptorUpdateTemplate -> PipelineLayout -> Word32 -> Ptr () -> IO ()

-- No documentation found for TopLevel "vkCmdPushDescriptorSetWithTemplateKHR"
cmdPushDescriptorSetWithTemplateKHR :: forall io
                                     . (MonadIO io)
                                    => -- No documentation found for Nested "vkCmdPushDescriptorSetWithTemplateKHR" "commandBuffer"
                                       CommandBuffer
                                    -> -- No documentation found for Nested "vkCmdPushDescriptorSetWithTemplateKHR" "descriptorUpdateTemplate"
                                       DescriptorUpdateTemplate
                                    -> -- No documentation found for Nested "vkCmdPushDescriptorSetWithTemplateKHR" "layout"
                                       PipelineLayout
                                    -> -- No documentation found for Nested "vkCmdPushDescriptorSetWithTemplateKHR" "set"
                                       ("set" ::: Word32)
                                    -> -- No documentation found for Nested "vkCmdPushDescriptorSetWithTemplateKHR" "pData"
                                       ("data" ::: Ptr ())
                                    -> io ()
cmdPushDescriptorSetWithTemplateKHR commandBuffer descriptorUpdateTemplate layout set data' = liftIO $ do
  let vkCmdPushDescriptorSetWithTemplateKHRPtr = pVkCmdPushDescriptorSetWithTemplateKHR (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdPushDescriptorSetWithTemplateKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdPushDescriptorSetWithTemplateKHR is null" Nothing Nothing
  let vkCmdPushDescriptorSetWithTemplateKHR' = mkVkCmdPushDescriptorSetWithTemplateKHR vkCmdPushDescriptorSetWithTemplateKHRPtr
  vkCmdPushDescriptorSetWithTemplateKHR' (commandBufferHandle (commandBuffer)) (descriptorUpdateTemplate) (layout) (set) (data')
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDevicePushDescriptorPropertiesKHR"
data PhysicalDevicePushDescriptorPropertiesKHR = PhysicalDevicePushDescriptorPropertiesKHR
  { -- No documentation found for Nested "VkPhysicalDevicePushDescriptorPropertiesKHR" "maxPushDescriptors"
    maxPushDescriptors :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePushDescriptorPropertiesKHR)
#endif
deriving instance Show PhysicalDevicePushDescriptorPropertiesKHR

instance ToCStruct PhysicalDevicePushDescriptorPropertiesKHR where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePushDescriptorPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxPushDescriptors)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDevicePushDescriptorPropertiesKHR where
  peekCStruct p = do
    maxPushDescriptors <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDevicePushDescriptorPropertiesKHR
             maxPushDescriptors


instance Storable PhysicalDevicePushDescriptorPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePushDescriptorPropertiesKHR where
  zero = PhysicalDevicePushDescriptorPropertiesKHR
           zero


type KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_SPEC_VERSION"
pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PUSH_DESCRIPTOR_SPEC_VERSION = 2


type KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = "VK_KHR_push_descriptor"

-- No documentation found for TopLevel "VK_KHR_PUSH_DESCRIPTOR_EXTENSION_NAME"
pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PUSH_DESCRIPTOR_EXTENSION_NAME = "VK_KHR_push_descriptor"

