{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_scissor_exclusive"
module Vulkan.Extensions.VK_NV_scissor_exclusive  ( cmdSetExclusiveScissorNV
                                                  , PhysicalDeviceExclusiveScissorFeaturesNV(..)
                                                  , PipelineViewportExclusiveScissorStateCreateInfoNV(..)
                                                  , NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
                                                  , pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION
                                                  , NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
                                                  , pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME
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
import Data.Vector (generateM)
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
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetExclusiveScissorNV))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetExclusiveScissorNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Rect2D -> IO ()

-- No documentation found for TopLevel "vkCmdSetExclusiveScissorNV"
cmdSetExclusiveScissorNV :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "vkCmdSetExclusiveScissorNV" "commandBuffer"
                            CommandBuffer
                         -> -- No documentation found for Nested "vkCmdSetExclusiveScissorNV" "firstExclusiveScissor"
                            ("firstExclusiveScissor" ::: Word32)
                         -> -- No documentation found for Nested "vkCmdSetExclusiveScissorNV" "pExclusiveScissors"
                            ("exclusiveScissors" ::: Vector Rect2D)
                         -> io ()
cmdSetExclusiveScissorNV commandBuffer firstExclusiveScissor exclusiveScissors = liftIO . evalContT $ do
  let vkCmdSetExclusiveScissorNVPtr = pVkCmdSetExclusiveScissorNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetExclusiveScissorNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetExclusiveScissorNV is null" Nothing Nothing
  let vkCmdSetExclusiveScissorNV' = mkVkCmdSetExclusiveScissorNV vkCmdSetExclusiveScissorNVPtr
  pPExclusiveScissors <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (exclusiveScissors)) * 16) 4
  lift $ Data.Vector.imapM_ (\i e -> poke (pPExclusiveScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (exclusiveScissors)
  lift $ vkCmdSetExclusiveScissorNV' (commandBufferHandle (commandBuffer)) (firstExclusiveScissor) ((fromIntegral (Data.Vector.length $ (exclusiveScissors)) :: Word32)) (pPExclusiveScissors)
  pure $ ()



-- No documentation found for TopLevel "VkPhysicalDeviceExclusiveScissorFeaturesNV"
data PhysicalDeviceExclusiveScissorFeaturesNV = PhysicalDeviceExclusiveScissorFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceExclusiveScissorFeaturesNV" "exclusiveScissor"
    exclusiveScissor :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExclusiveScissorFeaturesNV)
#endif
deriving instance Show PhysicalDeviceExclusiveScissorFeaturesNV

instance ToCStruct PhysicalDeviceExclusiveScissorFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExclusiveScissorFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (exclusiveScissor))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExclusiveScissorFeaturesNV where
  peekCStruct p = do
    exclusiveScissor <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExclusiveScissorFeaturesNV
             (bool32ToBool exclusiveScissor)


instance Storable PhysicalDeviceExclusiveScissorFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExclusiveScissorFeaturesNV where
  zero = PhysicalDeviceExclusiveScissorFeaturesNV
           zero



-- No documentation found for TopLevel "VkPipelineViewportExclusiveScissorStateCreateInfoNV"
data PipelineViewportExclusiveScissorStateCreateInfoNV = PipelineViewportExclusiveScissorStateCreateInfoNV
  { -- No documentation found for Nested "VkPipelineViewportExclusiveScissorStateCreateInfoNV" "pExclusiveScissors"
    exclusiveScissors :: Vector Rect2D }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportExclusiveScissorStateCreateInfoNV)
#endif
deriving instance Show PipelineViewportExclusiveScissorStateCreateInfoNV

instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportExclusiveScissorStateCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (exclusiveScissors)) :: Word32))
    pPExclusiveScissors' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (exclusiveScissors)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPExclusiveScissors' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (exclusiveScissors)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPExclusiveScissors')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPExclusiveScissors' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (mempty)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPExclusiveScissors' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Rect2D))) (pPExclusiveScissors')
    lift $ f

instance FromCStruct PipelineViewportExclusiveScissorStateCreateInfoNV where
  peekCStruct p = do
    exclusiveScissorCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pExclusiveScissors <- peek @(Ptr Rect2D) ((p `plusPtr` 24 :: Ptr (Ptr Rect2D)))
    pExclusiveScissors' <- generateM (fromIntegral exclusiveScissorCount) (\i -> peekCStruct @Rect2D ((pExclusiveScissors `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ PipelineViewportExclusiveScissorStateCreateInfoNV
             pExclusiveScissors'

instance Zero PipelineViewportExclusiveScissorStateCreateInfoNV where
  zero = PipelineViewportExclusiveScissorStateCreateInfoNV
           mempty


type NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_SPEC_VERSION"
pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SCISSOR_EXCLUSIVE_SPEC_VERSION = 1


type NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = "VK_NV_scissor_exclusive"

-- No documentation found for TopLevel "VK_NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME"
pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SCISSOR_EXCLUSIVE_EXTENSION_NAME = "VK_NV_scissor_exclusive"

