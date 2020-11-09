{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_pipeline_library  ( PipelineLibraryCreateInfoKHR(..)
                                                  , KHR_PIPELINE_LIBRARY_SPEC_VERSION
                                                  , pattern KHR_PIPELINE_LIBRARY_SPEC_VERSION
                                                  , KHR_PIPELINE_LIBRARY_EXTENSION_NAME
                                                  , pattern KHR_PIPELINE_LIBRARY_EXTENSION_NAME
                                                  ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR'
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkPipelineLibraryCreateInfoKHR-pLibraries-parameter# If
--     @libraryCount@ is not @0@, @pLibraries@ /must/ be a valid pointer to
--     an array of @libraryCount@ valid 'Vulkan.Core10.Handles.Pipeline'
--     handles
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineLibraryCreateInfoKHR = PipelineLibraryCreateInfoKHR
  { -- | @pLibraries@ is an array of pipeline libraries to use when creating a
    -- pipeline.
    libraries :: Vector Pipeline }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineLibraryCreateInfoKHR)
#endif
deriving instance Show PipelineLibraryCreateInfoKHR

instance ToCStruct PipelineLibraryCreateInfoKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineLibraryCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (libraries)) :: Word32))
    pPLibraries' <- ContT $ allocaBytesAligned @Pipeline ((Data.Vector.length (libraries)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPLibraries' `plusPtr` (8 * (i)) :: Ptr Pipeline) (e)) (libraries)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Pipeline))) (pPLibraries')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPLibraries' <- ContT $ allocaBytesAligned @Pipeline ((Data.Vector.length (mempty)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPLibraries' `plusPtr` (8 * (i)) :: Ptr Pipeline) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Pipeline))) (pPLibraries')
    lift $ f

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

