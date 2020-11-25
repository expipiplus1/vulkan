{-# language CPP #-}
-- No documentation found for Chapter "PipelineBindPoint"
module Vulkan.Core10.Enums.PipelineBindPoint  (PipelineBindPoint( PIPELINE_BIND_POINT_GRAPHICS
                                                                , PIPELINE_BIND_POINT_COMPUTE
                                                                , PIPELINE_BIND_POINT_RAY_TRACING_KHR
                                                                , ..
                                                                )) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkPipelineBindPoint - Specify the bind point of a pipeline object to a
-- command buffer
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsMemoryRequirementsInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutCreateInfoNV',
-- 'Vulkan.Core10.Pass.SubpassDescription',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdBindPipelineShaderGroupNV',
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetKHR'
newtype PipelineBindPoint = PipelineBindPoint Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'PIPELINE_BIND_POINT_GRAPHICS' specifies binding as a graphics pipeline.
pattern PIPELINE_BIND_POINT_GRAPHICS        = PipelineBindPoint 0
-- | 'PIPELINE_BIND_POINT_COMPUTE' specifies binding as a compute pipeline.
pattern PIPELINE_BIND_POINT_COMPUTE         = PipelineBindPoint 1
-- | 'PIPELINE_BIND_POINT_RAY_TRACING_KHR' specifies binding as a ray tracing
-- pipeline.
pattern PIPELINE_BIND_POINT_RAY_TRACING_KHR = PipelineBindPoint 1000165000
{-# complete PIPELINE_BIND_POINT_GRAPHICS,
             PIPELINE_BIND_POINT_COMPUTE,
             PIPELINE_BIND_POINT_RAY_TRACING_KHR :: PipelineBindPoint #-}

conNamePipelineBindPoint :: String
conNamePipelineBindPoint = "PipelineBindPoint"

enumPrefixPipelineBindPoint :: String
enumPrefixPipelineBindPoint = "PIPELINE_BIND_POINT_"

showTablePipelineBindPoint :: [(PipelineBindPoint, String)]
showTablePipelineBindPoint =
  [ (PIPELINE_BIND_POINT_GRAPHICS       , "GRAPHICS")
  , (PIPELINE_BIND_POINT_COMPUTE        , "COMPUTE")
  , (PIPELINE_BIND_POINT_RAY_TRACING_KHR, "RAY_TRACING_KHR")
  ]

instance Show PipelineBindPoint where
  showsPrec p e = case lookup e showTablePipelineBindPoint of
    Just s -> showString enumPrefixPipelineBindPoint . showString s
    Nothing ->
      let PipelineBindPoint x = e
      in  showParen (p >= 11) (showString conNamePipelineBindPoint . showString " " . showsPrec 11 x)

instance Read PipelineBindPoint where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineBindPoint
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineBindPoint)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineBindPoint)
            v <- step readPrec
            pure (PipelineBindPoint v)
          )
    )

