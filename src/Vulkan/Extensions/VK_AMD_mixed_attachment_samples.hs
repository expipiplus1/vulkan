{-# language CPP #-}
-- | = Name
--
-- VK_AMD_mixed_attachment_samples - device extension
--
-- = VK_AMD_mixed_attachment_samples
--
-- [__Name String__]
--     @VK_AMD_mixed_attachment_samples@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     137
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_mixed_attachment_samples] @anteru%0A*Here describe the issue or question you have about the VK_AMD_mixed_attachment_samples extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-07-24
--
-- [__Contributors__]
--
--     -   Mais Alnasser, AMD
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension enables applications to use multisampled rendering with a
-- depth\/stencil sample count that is larger than the color sample count.
-- Having a depth\/stencil sample count larger than the color sample count
-- allows maintaining geometry and coverage information at a higher sample
-- rate than color information. All samples are depth\/stencil tested, but
-- only the first color sample count number of samples get a corresponding
-- color output.
--
-- == New Structures
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo':
--
--     -   'AttachmentSampleCountInfoAMD'
--
-- == New Enum Constants
--
-- -   'AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME'
--
-- -   'AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2017-07-24 (Daniel Rakos)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMD_mixed_attachment_samples Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_mixed_attachment_samples  ( AttachmentSampleCountInfoAMD(..)
                                                          , AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION
                                                          , pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION
                                                          , AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME
                                                          , pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME
                                                          ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD))
-- | VkAttachmentSampleCountInfoAMD - Structure specifying command buffer
-- inheritance info for dynamic render pass instances
--
-- = Description
--
-- If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@renderPass@
-- is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@flags@, and the
-- @pNext@ chain of
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo' includes
-- 'AttachmentSampleCountInfoAMD', then this structure defines the sample
-- counts of each attachment within the render pass instance. If
-- 'AttachmentSampleCountInfoAMD' is not included, the value of
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.CommandBufferInheritanceRenderingInfo'::@rasterizationSamples@
-- is used as the sample count for each attachment. If
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo'::@renderPass@
-- is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', or
-- 'Vulkan.Core10.Enums.CommandBufferUsageFlagBits.COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
-- is not specified in
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo'::@flags@,
-- parameters of this structure are ignored.
--
-- 'AttachmentSampleCountInfoAMD' /can/ also be included in the @pNext@
-- chain of 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo'.
-- When a graphics pipeline is created without a
-- 'Vulkan.Core10.Handles.RenderPass', if this structure is included in the
-- @pNext@ chain of
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo', it
-- specifies the sample count of attachments used for rendering. If this
-- structure is not specified, and the pipeline does not include a
-- 'Vulkan.Core10.Handles.RenderPass', the value of
-- 'Vulkan.Core10.GraphicsPipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
-- is used as the sample count for each attachment. If a graphics pipeline
-- is created with a valid 'Vulkan.Core10.Handles.RenderPass', parameters
-- of this structure are ignored.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples VK_AMD_mixed_attachment_samples>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data AttachmentSampleCountInfoAMD = AttachmentSampleCountInfoAMD
  { -- | @pColorAttachmentSamples@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' values
    -- defining the sample count of color attachments.
    colorAttachmentSamples :: Vector SampleCountFlagBits
  , -- | @depthStencilAttachmentSamples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    -- defining the sample count of a depth\/stencil attachment.
    depthStencilAttachmentSamples :: SampleCountFlagBits
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AttachmentSampleCountInfoAMD)
#endif
deriving instance Show AttachmentSampleCountInfoAMD

instance ToCStruct AttachmentSampleCountInfoAMD where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AttachmentSampleCountInfoAMD{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentSamples)) :: Word32))
    pPColorAttachmentSamples' <- ContT $ allocaBytes @SampleCountFlagBits ((Data.Vector.length (colorAttachmentSamples)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentSamples' `plusPtr` (4 * (i)) :: Ptr SampleCountFlagBits) (e)) (colorAttachmentSamples)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr SampleCountFlagBits))) (pPColorAttachmentSamples')
    lift $ poke ((p `plusPtr` 32 :: Ptr SampleCountFlagBits)) (depthStencilAttachmentSamples)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ATTACHMENT_SAMPLE_COUNT_INFO_AMD)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct AttachmentSampleCountInfoAMD where
  peekCStruct p = do
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pColorAttachmentSamples <- peek @(Ptr SampleCountFlagBits) ((p `plusPtr` 24 :: Ptr (Ptr SampleCountFlagBits)))
    pColorAttachmentSamples' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @SampleCountFlagBits ((pColorAttachmentSamples `advancePtrBytes` (4 * (i)) :: Ptr SampleCountFlagBits)))
    depthStencilAttachmentSamples <- peek @SampleCountFlagBits ((p `plusPtr` 32 :: Ptr SampleCountFlagBits))
    pure $ AttachmentSampleCountInfoAMD
             pColorAttachmentSamples' depthStencilAttachmentSamples

instance Zero AttachmentSampleCountInfoAMD where
  zero = AttachmentSampleCountInfoAMD
           mempty
           zero


type AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION"
pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION = 1


type AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME = "VK_AMD_mixed_attachment_samples"

-- No documentation found for TopLevel "VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME"
pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME = "VK_AMD_mixed_attachment_samples"

