{-# language CPP #-}
-- | = Name
--
-- VK_KHR_pipeline_library - device extension
--
-- == VK_KHR_pipeline_library
--
-- [__Name String__]
--     @VK_KHR_pipeline_library@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     291
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_pipeline_library] @pixeljetstream%0A*Here describe the issue or question you have about the VK_KHR_pipeline_library extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-01-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   See contributors to @VK_KHR_ray_tracing_pipeline@
--
-- == Description
--
-- A pipeline library is a special pipeline that cannot be bound, instead
-- it defines a set of shaders and shader groups which can be linked into
-- other pipelines. This extension defines the infrastructure for pipeline
-- libraries, but does not specify the creation or usage of pipeline
-- libraries. This is left to additional dependent extensions.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineLibraryCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PIPELINE_LIBRARY_EXTENSION_NAME'
--
-- -   'KHR_PIPELINE_LIBRARY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-01-08 (Christoph Kubisch)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PipelineLibraryCreateInfoKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_pipeline_library Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_pipeline_library  ( PipelineLibraryCreateInfoKHR(..)
                                                  , KHR_PIPELINE_LIBRARY_SPEC_VERSION
                                                  , pattern KHR_PIPELINE_LIBRARY_SPEC_VERSION
                                                  , KHR_PIPELINE_LIBRARY_EXTENSION_NAME
                                                  , pattern KHR_PIPELINE_LIBRARY_EXTENSION_NAME
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
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR))
-- | VkPipelineLibraryCreateInfoKHR - Structure specifying pipeline libraries
-- to use when creating a pipeline
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pLibraries-03381# Each element
--     of @pLibraries@ /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pLibraries-06855# If any
--     library in @pLibraries@ was created with a shader stage with
--     'Vulkan.Extensions.VK_EXT_shader_module_identifier.PipelineShaderStageModuleIdentifierCreateInfoEXT'
--     and @identifierSize@ not equal to 0, the pipeline /must/ be created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
--     flag set
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pLibraries-08096# If any
--     element of @pLibraries@ was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     all elements /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pipeline-07404# If @pipeline@
--     is being created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT',
--     every element of @pLibraries@ /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT'
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pipeline-07405# If @pipeline@
--     is being created without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT',
--     every element of @pLibraries@ /must/ have been created without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT_EXT'
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pipeline-07406# If @pipeline@
--     is being created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT',
--     every element of @pLibraries@ /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT'
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pipeline-07407# If @pipeline@
--     is being created without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT',
--     every element of @pLibraries@ /must/ have been created without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR'
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pLibraries-parameter# If
--     @libraryCount@ is not @0@, @pLibraries@ /must/ be a valid pointer to
--     an array of @libraryCount@ valid 'Vulkan.Core10.Handles.Pipeline'
--     handles
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_library VK_KHR_pipeline_library>,
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.ExecutionGraphPipelineCreateInfoAMDX',
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineLibraryCreateInfoKHR = PipelineLibraryCreateInfoKHR
  { -- | @pLibraries@ is a pointer to an array of
    -- 'Vulkan.Core10.Handles.Pipeline' structures specifying pipeline
    -- libraries to use when creating a pipeline.
    libraries :: Vector Pipeline }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineLibraryCreateInfoKHR)
#endif
deriving instance Show PipelineLibraryCreateInfoKHR

instance ToCStruct PipelineLibraryCreateInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineLibraryCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (libraries)) :: Word32))
    pPLibraries' <- ContT $ allocaBytes @Pipeline ((Data.Vector.length (libraries)) * 8)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPLibraries' `plusPtr` (8 * (i)) :: Ptr Pipeline) (e)) (libraries)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Pipeline))) (pPLibraries')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineLibraryCreateInfoKHR where
  peekCStruct p = do
    libraryCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pLibraries <- peek @(Ptr Pipeline) ((p `plusPtr` 24 :: Ptr (Ptr Pipeline)))
    pLibraries' <- generateM (fromIntegral libraryCount) (\i -> peek @Pipeline ((pLibraries `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
    pure $ PipelineLibraryCreateInfoKHR
             pLibraries'

instance Zero PipelineLibraryCreateInfoKHR where
  zero = PipelineLibraryCreateInfoKHR
           mempty


type KHR_PIPELINE_LIBRARY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PIPELINE_LIBRARY_SPEC_VERSION"
pattern KHR_PIPELINE_LIBRARY_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PIPELINE_LIBRARY_SPEC_VERSION = 1


type KHR_PIPELINE_LIBRARY_EXTENSION_NAME = "VK_KHR_pipeline_library"

-- No documentation found for TopLevel "VK_KHR_PIPELINE_LIBRARY_EXTENSION_NAME"
pattern KHR_PIPELINE_LIBRARY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PIPELINE_LIBRARY_EXTENSION_NAME = "VK_KHR_pipeline_library"

