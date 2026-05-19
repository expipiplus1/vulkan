module Vulkan.Utils.Shader
  ( shaderStage
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.ByteString (ByteString)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)

{- | Build a 'PipelineShaderStageCreateInfo' for a single SPIR-V module with
entry point @main@. The returned 'ReleaseKey' frees the module — release it
once the pipeline is built.
-}
shaderStage
  :: (MonadResource m)
  => Vk.Device
  -> Vk.ShaderStageFlagBits
  -> ByteString
  -> m (ReleaseKey, SomeStruct Vk.PipelineShaderStageCreateInfo)
shaderStage dev stage code = do
  (key, module') <- Vk.withShaderModule dev zero{Vk.code = code} Nothing allocate
  pure
    ( key
    , SomeStruct
        zero
          { Vk.stage
          , Vk.module'
          , Vk.name = "main"
          }
    )
