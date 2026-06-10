module Vulkan.Utils.Shader
  ( shaderStage
  , shaderModuleStage
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.ByteString (ByteString)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Pipeline.Specialization (Specialization, allocateSpecialization)
import Vulkan.Zero (zero)

{- | Build a 'PipelineShaderStageCreateInfo' for a single SPIR-V module with
entry point @main@. The returned 'ReleaseKey' frees the module — release it
once the pipeline is built.

The @spec@ argument supplies specialization constants (see
'Vulkan.Utils.Pipeline.Specialization'); pass @()@ for none.
-}
shaderStage
  :: (MonadResource m, Specialization spec)
  => Vk.Device
  -> Vk.ShaderStageFlagBits
  -> spec
  -> ByteString
  -> m (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo)
shaderStage dev stage spec code = do
  specializationInfo <- allocateSpecialization spec
  shaderModuleStage dev stage specializationInfo code

{- | Lower-level companion to 'shaderStage' taking an already-built
'Vk.SpecializationInfo' (or 'Nothing'). Useful when one specialization is shared
across several stages — build it once with
'Vulkan.Utils.Pipeline.Specialization.withSpecialization' and pass it to each
stage rather than re-packing per stage.
-}
shaderModuleStage
  :: (MonadResource m)
  => Vk.Device
  -> Vk.ShaderStageFlagBits
  -> Maybe Vk.SpecializationInfo
  -> ByteString
  -> m (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo)
shaderModuleStage dev stage specializationInfo code = do
  (key, module') <- Vk.withShaderModule dev zero{Vk.code = code} Nothing allocate
  pure
    ( key
    , SomeStruct
        zero
          { Vk.stage
          , Vk.module'
          , Vk.name = "main"
          , Vk.specializationInfo
          }
    )
