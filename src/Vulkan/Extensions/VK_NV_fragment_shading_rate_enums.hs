{-# language CPP #-}
module Vulkan.Extensions.VK_NV_fragment_shading_rate_enums  ( cmdSetFragmentShadingRateEnumNV
                                                            , PhysicalDeviceFragmentShadingRateEnumsFeaturesNV(..)
                                                            , PhysicalDeviceFragmentShadingRateEnumsPropertiesNV(..)
                                                            , PipelineFragmentShadingRateEnumStateCreateInfoNV(..)
                                                            , FragmentShadingRateNV( FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV
                                                                                   , FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV
                                                                                   , FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV
                                                                                   , ..
                                                                                   )
                                                            , FragmentShadingRateTypeNV( FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV
                                                                                       , FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV
                                                                                       , ..
                                                                                       )
                                                            , NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION
                                                            , pattern NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION
                                                            , NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME
                                                            , pattern NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME
                                                            , FragmentShadingRateCombinerOpKHR(..)
                                                            ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetFragmentShadingRateEnumNV))
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateCombinerOpKHR)
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateCombinerOpKHR(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV))
import Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateCombinerOpKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetFragmentShadingRateEnumNV
  :: FunPtr (Ptr CommandBuffer_T -> FragmentShadingRateNV -> Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR) -> IO ()) -> Ptr CommandBuffer_T -> FragmentShadingRateNV -> Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR) -> IO ()

-- | vkCmdSetFragmentShadingRateEnumNV - Set pipeline fragment shading rate
-- dynamically using enums
--
-- == Valid Usage
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     is not enabled, @shadingRate@ /must/ be
--     'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV'
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-supersampleFragmentShadingRates supersampleFragmentShadingRates>
--     is not enabled, @shadingRate@ /must/ not be
--     'FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV',
--     'FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV',
--     'FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV', or
--     'FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV'
--
-- -   If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-noInvocationFragmentShadingRates noInvocationFragmentShadingRates>
--     is not enabled, @shadingRate@ /must/ not be
--     'FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV'
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentShadingRateEnums fragmentShadingRateEnums>
--     /must/ be enabled
--
-- -   One of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>,
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     /must/ be enabled
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-primitiveFragmentShadingRate primitiveFragmentShadingRate feature>
--     is not enabled, @combinerOps@[0] /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#feature-attachmentFragmentShadingRate attachmentFragmentShadingRate feature>
--     is not enabled, @combinerOps@[1] /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-fragmentShadingRateNonTrivialCombinerOps fragmentSizeNonTrivialCombinerOps>
--     limit is not supported, elements of @combinerOps@ /must/ be either
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   @shadingRate@ /must/ be a valid 'FragmentShadingRateNV' value
--
-- -   Any given element of @combinerOps@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
--     value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR',
-- 'FragmentShadingRateNV'
cmdSetFragmentShadingRateEnumNV :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @shadingRate@ specifies a 'FragmentShadingRateNV' enum indicating the
                                   -- pipeline fragment shading rate for subsequent draw commands.
                                   FragmentShadingRateNV
                                -> -- | @combinerOps@ specifies a
                                   -- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
                                   -- determining how the
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline pipeline>,
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive>,
                                   -- and
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment attachment shading rates>
                                   -- are
                                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-combining combined>
                                   -- for fragments generated by subsequent drawing commands.
                                   ("combinerOps" ::: (FragmentShadingRateCombinerOpKHR, FragmentShadingRateCombinerOpKHR))
                                -> io ()
cmdSetFragmentShadingRateEnumNV commandBuffer shadingRate combinerOps = liftIO . evalContT $ do
  let vkCmdSetFragmentShadingRateEnumNVPtr = pVkCmdSetFragmentShadingRateEnumNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdSetFragmentShadingRateEnumNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetFragmentShadingRateEnumNV is null" Nothing Nothing
  let vkCmdSetFragmentShadingRateEnumNV' = mkVkCmdSetFragmentShadingRateEnumNV vkCmdSetFragmentShadingRateEnumNVPtr
  pCombinerOps <- ContT $ allocaBytesAligned @(FixedArray 2 FragmentShadingRateCombinerOpKHR) 8 4
  let pCombinerOps' = lowerArrayPtr pCombinerOps
  lift $ case (combinerOps) of
    (e0, e1) -> do
      poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
      poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
  lift $ vkCmdSetFragmentShadingRateEnumNV' (commandBufferHandle (commandBuffer)) (shadingRate) (pCombinerOps)
  pure $ ()


-- | VkPhysicalDeviceFragmentShadingRateEnumsFeaturesNV - Structure
-- indicating support for fragment shading rate enums
--
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShadingRateEnumsFeaturesNV = PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
  { -- | @fragmentShadingRateEnums@ indicates that the implementation supports
    -- specifying fragment shading rates using the 'FragmentShadingRateNV'
    -- enumerated type.
    fragmentShadingRateEnums :: Bool
  , -- | @supersampleFragmentShadingRates@ indicates that the implementation
    -- supports fragment shading rate enum values indicating more than one
    -- invocation per fragment.
    supersampleFragmentShadingRates :: Bool
  , -- | @noInvocationFragmentShadingRates@ indicates that the implementation
    -- supports a fragment shading rate enum value indicating that no fragment
    -- shaders should be invoked when that shading rate is used.
    noInvocationFragmentShadingRates :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRateEnumsFeaturesNV)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRateEnumsFeaturesNV

instance ToCStruct PhysicalDeviceFragmentShadingRateEnumsFeaturesNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRateEnumsFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentShadingRateEnums))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (supersampleFragmentShadingRates))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (noInvocationFragmentShadingRates))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentShadingRateEnumsFeaturesNV where
  peekCStruct p = do
    fragmentShadingRateEnums <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    supersampleFragmentShadingRates <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    noInvocationFragmentShadingRates <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
             (bool32ToBool fragmentShadingRateEnums) (bool32ToBool supersampleFragmentShadingRates) (bool32ToBool noInvocationFragmentShadingRates)

instance Storable PhysicalDeviceFragmentShadingRateEnumsFeaturesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRateEnumsFeaturesNV where
  zero = PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
           zero
           zero
           zero


-- | VkPhysicalDeviceFragmentShadingRateEnumsPropertiesNV - Structure
-- describing fragment shading rate limits that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceFragmentShadingRateEnumsPropertiesNV'
-- structure describe the following implementation-dependent properties
-- related to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-primsrast-fragment-shading-rate fragment shading rates>:
--
-- = Description
--
-- If the 'PhysicalDeviceFragmentShadingRateEnumsPropertiesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFragmentShadingRateEnumsPropertiesNV = PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
  { -- | @maxFragmentShadingRateInvocationCount@ indicates the maximum number of
    -- fragment shader invocations per fragment supported in pipeline,
    -- primitive, and attachment fragment shading rates.
    --
    -- @maxFragmentShadingRateInvocationCount@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    maxFragmentShadingRateInvocationCount :: SampleCountFlagBits }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentShadingRateEnumsPropertiesNV)
#endif
deriving instance Show PhysicalDeviceFragmentShadingRateEnumsPropertiesNV

instance ToCStruct PhysicalDeviceFragmentShadingRateEnumsPropertiesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentShadingRateEnumsPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlagBits)) (maxFragmentShadingRateInvocationCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SampleCountFlagBits)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentShadingRateEnumsPropertiesNV where
  peekCStruct p = do
    maxFragmentShadingRateInvocationCount <- peek @SampleCountFlagBits ((p `plusPtr` 16 :: Ptr SampleCountFlagBits))
    pure $ PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
             maxFragmentShadingRateInvocationCount

instance Storable PhysicalDeviceFragmentShadingRateEnumsPropertiesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentShadingRateEnumsPropertiesNV where
  zero = PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
           zero


-- | VkPipelineFragmentShadingRateEnumStateCreateInfoNV - Structure
-- specifying parameters controlling the fragment shading rate using rate
-- enums
--
-- = Description
--
-- If the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo' includes a
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV' structure, then that
-- structure includes parameters that control the pipeline fragment shading
-- rate.
--
-- If this structure is not present, @shadingRateType@ is considered to be
-- equal to 'FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV', @shadingRate@ is
-- considered to be equal to
-- 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV', and both elements of
-- @combinerOps@ are considered to be equal to
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV'
--
-- -   @shadingRateType@ /must/ be a valid 'FragmentShadingRateTypeNV'
--     value
--
-- -   @shadingRate@ /must/ be a valid 'FragmentShadingRateNV' value
--
-- -   Any given element of @combinerOps@ /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
--     value
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR',
-- 'FragmentShadingRateNV', 'FragmentShadingRateTypeNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineFragmentShadingRateEnumStateCreateInfoNV = PipelineFragmentShadingRateEnumStateCreateInfoNV
  { -- | @shadingRateType@ specifies a 'FragmentShadingRateTypeNV' value
    -- indicating whether fragment shading rates are specified using fragment
    -- sizes or 'FragmentShadingRateNV' enums.
    shadingRateType :: FragmentShadingRateTypeNV
  , -- | @shadingRate@ specifies a 'FragmentShadingRateNV' value indicating the
    -- pipeline fragment shading rate.
    shadingRate :: FragmentShadingRateNV
  , -- | @combinerOps@ specifies
    -- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
    -- values determining how the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline pipeline>,
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive>,
    -- and
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment attachment shading rates>
    -- are
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-combining combined>
    -- for fragments generated by drawing commands using the created pipeline.
    combinerOps :: (FragmentShadingRateCombinerOpKHR, FragmentShadingRateCombinerOpKHR)
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineFragmentShadingRateEnumStateCreateInfoNV)
#endif
deriving instance Show PipelineFragmentShadingRateEnumStateCreateInfoNV

instance ToCStruct PipelineFragmentShadingRateEnumStateCreateInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineFragmentShadingRateEnumStateCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FragmentShadingRateTypeNV)) (shadingRateType)
    poke ((p `plusPtr` 20 :: Ptr FragmentShadingRateNV)) (shadingRate)
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    case (combinerOps) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FragmentShadingRateTypeNV)) (zero)
    poke ((p `plusPtr` 20 :: Ptr FragmentShadingRateNV)) (zero)
    let pCombinerOps' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    case ((zero, zero)) of
      (e0, e1) -> do
        poke (pCombinerOps' :: Ptr FragmentShadingRateCombinerOpKHR) (e0)
        poke (pCombinerOps' `plusPtr` 4 :: Ptr FragmentShadingRateCombinerOpKHR) (e1)
    f

instance FromCStruct PipelineFragmentShadingRateEnumStateCreateInfoNV where
  peekCStruct p = do
    shadingRateType <- peek @FragmentShadingRateTypeNV ((p `plusPtr` 16 :: Ptr FragmentShadingRateTypeNV))
    shadingRate <- peek @FragmentShadingRateNV ((p `plusPtr` 20 :: Ptr FragmentShadingRateNV))
    let pcombinerOps = lowerArrayPtr @FragmentShadingRateCombinerOpKHR ((p `plusPtr` 24 :: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)))
    combinerOps0 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 0 :: Ptr FragmentShadingRateCombinerOpKHR))
    combinerOps1 <- peek @FragmentShadingRateCombinerOpKHR ((pcombinerOps `advancePtrBytes` 4 :: Ptr FragmentShadingRateCombinerOpKHR))
    pure $ PipelineFragmentShadingRateEnumStateCreateInfoNV
             shadingRateType shadingRate ((combinerOps0, combinerOps1))

instance Storable PipelineFragmentShadingRateEnumStateCreateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineFragmentShadingRateEnumStateCreateInfoNV where
  zero = PipelineFragmentShadingRateEnumStateCreateInfoNV
           zero
           zero
           (zero, zero)


-- | VkFragmentShadingRateNV - Enumeration with fragment shading rates
--
-- = Description
--
-- To use the shading rates
-- 'FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV',
-- 'FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV',
-- 'FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV', and
-- 'FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV' as a pipeline,
-- primitive, or attachment shading rate, the
-- @supersampleFragmentShadingRates@ feature /must/ be enabled. To use the
-- shading rate 'FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV' as a pipeline,
-- primitive, or attachment shading rate, the
-- @noInvocationFragmentShadingRates@ feature /must/ be enabled.
--
-- = See Also
--
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV',
-- 'cmdSetFragmentShadingRateEnumNV'
newtype FragmentShadingRateNV = FragmentShadingRateNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV = FragmentShadingRateNV 0
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV' specifies a
-- fragment size of 1x2 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV = FragmentShadingRateNV 1
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV' specifies a
-- fragment size of 2x1 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV = FragmentShadingRateNV 4
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV' specifies a
-- fragment size of 2x2 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV = FragmentShadingRateNV 5
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV' specifies a
-- fragment size of 2x4 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV = FragmentShadingRateNV 6
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV' specifies a
-- fragment size of 4x2 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV = FragmentShadingRateNV 9
-- | 'FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV' specifies a
-- fragment size of 4x4 pixels.
pattern FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV = FragmentShadingRateNV 10
-- | 'FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels, with two fragment shader invocations per fragment.
pattern FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV = FragmentShadingRateNV 11
-- | 'FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels, with four fragment shader invocations per fragment.
pattern FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV = FragmentShadingRateNV 12
-- | 'FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels, with eight fragment shader invocations per fragment.
pattern FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV = FragmentShadingRateNV 13
-- | 'FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV' specifies a fragment
-- size of 1x1 pixels, with sixteen fragment shader invocations per
-- fragment.
pattern FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV = FragmentShadingRateNV 14
-- | 'FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV' specifies that any portions of
-- a primitive that use that shading rate should be discarded without
-- invoking any fragment shader.
pattern FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV = FragmentShadingRateNV 15
{-# complete FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV,
             FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV,
             FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV,
             FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV :: FragmentShadingRateNV #-}

instance Show FragmentShadingRateNV where
  showsPrec p = \case
    FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV -> showString "FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV"
    FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV -> showString "FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV"
    FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV -> showString "FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV"
    FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV -> showString "FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV"
    FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV -> showString "FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV"
    FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV -> showString "FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV"
    FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV -> showString "FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV"
    FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV -> showString "FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV"
    FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV -> showString "FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV"
    FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV -> showString "FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV"
    FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV -> showString "FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV"
    FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV -> showString "FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV"
    FragmentShadingRateNV x -> showParen (p >= 11) (showString "FragmentShadingRateNV " . showsPrec 11 x)

instance Read FragmentShadingRateNV where
  readPrec = parens (choose [("FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV", pure FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV)
                            , ("FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV", pure FRAGMENT_SHADING_RATE_1_INVOCATION_PER_1X2_PIXELS_NV)
                            , ("FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV", pure FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X1_PIXELS_NV)
                            , ("FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV", pure FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X2_PIXELS_NV)
                            , ("FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV", pure FRAGMENT_SHADING_RATE_1_INVOCATION_PER_2X4_PIXELS_NV)
                            , ("FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV", pure FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X2_PIXELS_NV)
                            , ("FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV", pure FRAGMENT_SHADING_RATE_1_INVOCATION_PER_4X4_PIXELS_NV)
                            , ("FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV", pure FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV)
                            , ("FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV", pure FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV)
                            , ("FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV", pure FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV)
                            , ("FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV", pure FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV)
                            , ("FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV", pure FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "FragmentShadingRateNV")
                       v <- step readPrec
                       pure (FragmentShadingRateNV v)))


-- | VkFragmentShadingRateTypeNV - Enumeration with fragment shading rate
-- types
--
-- = See Also
--
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV'
newtype FragmentShadingRateTypeNV = FragmentShadingRateTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV' specifies that a graphics
-- pipeline should obtain its pipeline fragment shading rate and shading
-- rate combiner state from the
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
-- structure and that any state specified by the
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV' structure should be
-- ignored.
pattern FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV = FragmentShadingRateTypeNV 0
-- | 'FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV' specifies that a graphics pipeline
-- should obtain its pipeline fragment shading rate and shading rate
-- combiner state from the
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV' structure and that
-- any state specified by the
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
-- structure should be ignored.
pattern FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV = FragmentShadingRateTypeNV 1
{-# complete FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV,
             FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV :: FragmentShadingRateTypeNV #-}

instance Show FragmentShadingRateTypeNV where
  showsPrec p = \case
    FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV -> showString "FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV"
    FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV -> showString "FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV"
    FragmentShadingRateTypeNV x -> showParen (p >= 11) (showString "FragmentShadingRateTypeNV " . showsPrec 11 x)

instance Read FragmentShadingRateTypeNV where
  readPrec = parens (choose [("FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV", pure FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV)
                            , ("FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV", pure FRAGMENT_SHADING_RATE_TYPE_ENUMS_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "FragmentShadingRateTypeNV")
                       v <- step readPrec
                       pure (FragmentShadingRateTypeNV v)))


type NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION"
pattern NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION = 1


type NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME = "VK_NV_fragment_shading_rate_enums"

-- No documentation found for TopLevel "VK_NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME"
pattern NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME = "VK_NV_fragment_shading_rate_enums"

