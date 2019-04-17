{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Pipeline
  ( BlendFactor
  , BlendOp
  , ColorComponentFlagBits
  , ColorComponentFlags
  , CompareOp
  , withCStructComputePipelineCreateInfo
  , fromCStructComputePipelineCreateInfo
  , ComputePipelineCreateInfo(..)
  , CullModeFlagBits
  , CullModeFlags
  , DynamicState
  , withCStructExtent2D
  , fromCStructExtent2D
  , Extent2D(..)
  , FrontFace
  , withCStructGraphicsPipelineCreateInfo
  , fromCStructGraphicsPipelineCreateInfo
  , GraphicsPipelineCreateInfo(..)
  , LogicOp
  , withCStructOffset2D
  , fromCStructOffset2D
  , Offset2D(..)
  , Pipeline
  , withCStructPipelineColorBlendAttachmentState
  , fromCStructPipelineColorBlendAttachmentState
  , PipelineColorBlendAttachmentState(..)
  , PipelineColorBlendStateCreateFlags
  , withCStructPipelineColorBlendStateCreateInfo
  , fromCStructPipelineColorBlendStateCreateInfo
  , PipelineColorBlendStateCreateInfo(..)
  , PipelineCreateFlagBits
  , PipelineCreateFlags
  , PipelineDepthStencilStateCreateFlags
  , withCStructPipelineDepthStencilStateCreateInfo
  , fromCStructPipelineDepthStencilStateCreateInfo
  , PipelineDepthStencilStateCreateInfo(..)
  , PipelineDynamicStateCreateFlags
  , withCStructPipelineDynamicStateCreateInfo
  , fromCStructPipelineDynamicStateCreateInfo
  , PipelineDynamicStateCreateInfo(..)
  , PipelineInputAssemblyStateCreateFlags
  , withCStructPipelineInputAssemblyStateCreateInfo
  , fromCStructPipelineInputAssemblyStateCreateInfo
  , PipelineInputAssemblyStateCreateInfo(..)
  , PipelineLayout
  , PipelineMultisampleStateCreateFlags
  , withCStructPipelineMultisampleStateCreateInfo
  , fromCStructPipelineMultisampleStateCreateInfo
  , PipelineMultisampleStateCreateInfo(..)
  , PipelineRasterizationStateCreateFlags
  , withCStructPipelineRasterizationStateCreateInfo
  , fromCStructPipelineRasterizationStateCreateInfo
  , PipelineRasterizationStateCreateInfo(..)
  , PipelineShaderStageCreateFlags
  , withCStructPipelineShaderStageCreateInfo
  , fromCStructPipelineShaderStageCreateInfo
  , PipelineShaderStageCreateInfo(..)
  , PipelineTessellationStateCreateFlags
  , withCStructPipelineTessellationStateCreateInfo
  , fromCStructPipelineTessellationStateCreateInfo
  , PipelineTessellationStateCreateInfo(..)
  , PipelineVertexInputStateCreateFlags
  , withCStructPipelineVertexInputStateCreateInfo
  , fromCStructPipelineVertexInputStateCreateInfo
  , PipelineVertexInputStateCreateInfo(..)
  , PipelineViewportStateCreateFlags
  , withCStructPipelineViewportStateCreateInfo
  , fromCStructPipelineViewportStateCreateInfo
  , PipelineViewportStateCreateInfo(..)
  , PolygonMode
  , PrimitiveTopology
  , withCStructRect2D
  , fromCStructRect2D
  , Rect2D(..)
  , RenderPass
  , SampleMask
  , ShaderStageFlagBits
  , withCStructSpecializationInfo
  , fromCStructSpecializationInfo
  , SpecializationInfo(..)
  , withCStructSpecializationMapEntry
  , fromCStructSpecializationMapEntry
  , SpecializationMapEntry(..)
  , StencilOp
  , withCStructStencilOpState
  , fromCStructStencilOpState
  , StencilOpState(..)
  , withCStructVertexInputAttributeDescription
  , fromCStructVertexInputAttributeDescription
  , VertexInputAttributeDescription(..)
  , withCStructVertexInputBindingDescription
  , fromCStructVertexInputBindingDescription
  , VertexInputBindingDescription(..)
  , VertexInputRate
  , withCStructViewport
  , fromCStructViewport
  , Viewport(..)
  , createComputePipelines
  , createGraphicsPipelines
  , destroyPipeline
  , withComputePipelines
  , withGraphicsPipelines
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Bits
  ( zeroBits
  )
import Data.ByteString
  ( ByteString
  , packCString
  , packCStringLen
  , useAsCString
  )
import qualified Data.ByteString
  ( empty
  , length
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
  )
import Data.Coerce
  ( coerce
  )
import Data.Function
  ( (&)
  )
import Data.Int
  ( Int32
  )
import Data.Maybe
  ( maybe
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Vector.Generic.Sized
  ( fromTuple
  )
import qualified Data.Vector.Storable.Sized
  ( unsafeIndex
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  , CSize(..)
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createComputePipelines
  , createGraphicsPipelines
  , destroyPipeline
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkSampleCountFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkBlendFactor(..)
  , VkBlendOp(..)
  , VkColorComponentFlagBits(..)
  , VkCompareOp(..)
  , VkComputePipelineCreateInfo(..)
  , VkCullModeFlagBits(..)
  , VkDynamicState(..)
  , VkExtent2D(..)
  , VkFrontFace(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkLogicOp(..)
  , VkOffset2D(..)
  , VkPipelineColorBlendAttachmentState(..)
  , VkPipelineColorBlendStateCreateFlags(..)
  , VkPipelineColorBlendStateCreateInfo(..)
  , VkPipelineCreateFlagBits(..)
  , VkPipelineDepthStencilStateCreateFlags(..)
  , VkPipelineDepthStencilStateCreateInfo(..)
  , VkPipelineDynamicStateCreateFlags(..)
  , VkPipelineDynamicStateCreateInfo(..)
  , VkPipelineInputAssemblyStateCreateFlags(..)
  , VkPipelineInputAssemblyStateCreateInfo(..)
  , VkPipelineMultisampleStateCreateFlags(..)
  , VkPipelineMultisampleStateCreateInfo(..)
  , VkPipelineRasterizationStateCreateFlags(..)
  , VkPipelineRasterizationStateCreateInfo(..)
  , VkPipelineShaderStageCreateFlags(..)
  , VkPipelineShaderStageCreateInfo(..)
  , VkPipelineTessellationStateCreateFlags(..)
  , VkPipelineTessellationStateCreateInfo(..)
  , VkPipelineVertexInputStateCreateFlags(..)
  , VkPipelineVertexInputStateCreateInfo(..)
  , VkPipelineViewportStateCreateFlags(..)
  , VkPipelineViewportStateCreateInfo(..)
  , VkPolygonMode(..)
  , VkPrimitiveTopology(..)
  , VkRect2D(..)
  , VkShaderStageFlagBits(..)
  , VkSpecializationInfo(..)
  , VkSpecializationMapEntry(..)
  , VkStencilOp(..)
  , VkStencilOpState(..)
  , VkVertexInputAttributeDescription(..)
  , VkVertexInputBindingDescription(..)
  , VkVertexInputRate(..)
  , VkViewport(..)
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  , VkSampleMask
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , SampleCountFlagBits
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  )
import Graphics.Vulkan.Core10.Shader
  ( ShaderModule
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( padVector
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "BlendFactor"
type BlendFactor = VkBlendFactor
-- No documentation found for TopLevel "BlendOp"
type BlendOp = VkBlendOp
-- No documentation found for TopLevel "ColorComponentFlagBits"
type ColorComponentFlagBits = VkColorComponentFlagBits
-- No documentation found for TopLevel "ColorComponentFlags"
type ColorComponentFlags = ColorComponentFlagBits
-- No documentation found for TopLevel "CompareOp"
type CompareOp = VkCompareOp
-- No documentation found for TopLevel "ComputePipelineCreateInfo"
data ComputePipelineCreateInfo = ComputePipelineCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "ComputePipelineCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "flags"
  vkFlags :: PipelineCreateFlags
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "stage"
  vkStage :: PipelineShaderStageCreateInfo
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "layout"
  vkLayout :: PipelineLayout
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "basePipelineHandle"
  vkBasePipelineHandle :: Pipeline
  , -- No documentation found for Nested "ComputePipelineCreateInfo" "basePipelineIndex"
  vkBasePipelineIndex :: Int32
  }
  deriving (Show, Eq)
withCStructComputePipelineCreateInfo :: ComputePipelineCreateInfo -> (VkComputePipelineCreateInfo -> IO a) -> IO a
withCStructComputePipelineCreateInfo from cont = withCStructPipelineShaderStageCreateInfo (vkStage (from :: ComputePipelineCreateInfo)) (\stage -> maybeWith withSomeVkStruct (vkPNext (from :: ComputePipelineCreateInfo)) (\pPNext -> cont (VkComputePipelineCreateInfo VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO pPNext (vkFlags (from :: ComputePipelineCreateInfo)) stage (vkLayout (from :: ComputePipelineCreateInfo)) (vkBasePipelineHandle (from :: ComputePipelineCreateInfo)) (vkBasePipelineIndex (from :: ComputePipelineCreateInfo)))))
fromCStructComputePipelineCreateInfo :: VkComputePipelineCreateInfo -> IO ComputePipelineCreateInfo
fromCStructComputePipelineCreateInfo c = ComputePipelineCreateInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkComputePipelineCreateInfo)))
                                                                   <*> pure (vkFlags (c :: VkComputePipelineCreateInfo))
                                                                   <*> (fromCStructPipelineShaderStageCreateInfo (vkStage (c :: VkComputePipelineCreateInfo)))
                                                                   <*> pure (vkLayout (c :: VkComputePipelineCreateInfo))
                                                                   <*> pure (vkBasePipelineHandle (c :: VkComputePipelineCreateInfo))
                                                                   <*> pure (vkBasePipelineIndex (c :: VkComputePipelineCreateInfo))
instance Zero ComputePipelineCreateInfo where
  zero = ComputePipelineCreateInfo Nothing
                                   zero
                                   zero
                                   zero
                                   zero
                                   zero
-- No documentation found for TopLevel "CullModeFlagBits"
type CullModeFlagBits = VkCullModeFlagBits
-- No documentation found for TopLevel "CullModeFlags"
type CullModeFlags = CullModeFlagBits
-- No documentation found for TopLevel "DynamicState"
type DynamicState = VkDynamicState
-- No documentation found for TopLevel "Extent2D"
data Extent2D = Extent2D
  { -- No documentation found for Nested "Extent2D" "width"
  vkWidth :: Word32
  , -- No documentation found for Nested "Extent2D" "height"
  vkHeight :: Word32
  }
  deriving (Show, Eq)
withCStructExtent2D :: Extent2D -> (VkExtent2D -> IO a) -> IO a
withCStructExtent2D from cont = cont (VkExtent2D (vkWidth (from :: Extent2D)) (vkHeight (from :: Extent2D)))
fromCStructExtent2D :: VkExtent2D -> IO Extent2D
fromCStructExtent2D c = Extent2D <$> pure (vkWidth (c :: VkExtent2D))
                                 <*> pure (vkHeight (c :: VkExtent2D))
instance Zero Extent2D where
  zero = Extent2D zero
                  zero
-- No documentation found for TopLevel "FrontFace"
type FrontFace = VkFrontFace
-- No documentation found for TopLevel "GraphicsPipelineCreateInfo"
data GraphicsPipelineCreateInfo = GraphicsPipelineCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "flags"
  vkFlags :: PipelineCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pStages"
  vkPStages :: Vector PipelineShaderStageCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pVertexInputState"
  vkPVertexInputState :: Maybe PipelineVertexInputStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pInputAssemblyState"
  vkPInputAssemblyState :: Maybe PipelineInputAssemblyStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pTessellationState"
  vkPTessellationState :: Maybe PipelineTessellationStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pViewportState"
  vkPViewportState :: Maybe PipelineViewportStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pRasterizationState"
  vkPRasterizationState :: PipelineRasterizationStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pMultisampleState"
  vkPMultisampleState :: Maybe PipelineMultisampleStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pDepthStencilState"
  vkPDepthStencilState :: Maybe PipelineDepthStencilStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pColorBlendState"
  vkPColorBlendState :: Maybe PipelineColorBlendStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "pDynamicState"
  vkPDynamicState :: Maybe PipelineDynamicStateCreateInfo
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "layout"
  vkLayout :: PipelineLayout
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "renderPass"
  vkRenderPass :: RenderPass
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "subpass"
  vkSubpass :: Word32
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "basePipelineHandle"
  vkBasePipelineHandle :: Pipeline
  , -- No documentation found for Nested "GraphicsPipelineCreateInfo" "basePipelineIndex"
  vkBasePipelineIndex :: Int32
  }
  deriving (Show, Eq)
withCStructGraphicsPipelineCreateInfo :: GraphicsPipelineCreateInfo -> (VkGraphicsPipelineCreateInfo -> IO a) -> IO a
withCStructGraphicsPipelineCreateInfo from cont = maybeWith (\a -> withCStructPipelineDynamicStateCreateInfo a . flip with) (vkPDynamicState (from :: GraphicsPipelineCreateInfo)) (\pDynamicState -> maybeWith (\a -> withCStructPipelineColorBlendStateCreateInfo a . flip with) (vkPColorBlendState (from :: GraphicsPipelineCreateInfo)) (\pColorBlendState -> maybeWith (\a -> withCStructPipelineDepthStencilStateCreateInfo a . flip with) (vkPDepthStencilState (from :: GraphicsPipelineCreateInfo)) (\pDepthStencilState -> maybeWith (\a -> withCStructPipelineMultisampleStateCreateInfo a . flip with) (vkPMultisampleState (from :: GraphicsPipelineCreateInfo)) (\pMultisampleState -> (\a -> withCStructPipelineRasterizationStateCreateInfo a . flip with) (vkPRasterizationState (from :: GraphicsPipelineCreateInfo)) (\pRasterizationState -> maybeWith (\a -> withCStructPipelineViewportStateCreateInfo a . flip with) (vkPViewportState (from :: GraphicsPipelineCreateInfo)) (\pViewportState -> maybeWith (\a -> withCStructPipelineTessellationStateCreateInfo a . flip with) (vkPTessellationState (from :: GraphicsPipelineCreateInfo)) (\pTessellationState -> maybeWith (\a -> withCStructPipelineInputAssemblyStateCreateInfo a . flip with) (vkPInputAssemblyState (from :: GraphicsPipelineCreateInfo)) (\pInputAssemblyState -> maybeWith (\a -> withCStructPipelineVertexInputStateCreateInfo a . flip with) (vkPVertexInputState (from :: GraphicsPipelineCreateInfo)) (\pVertexInputState -> withVec withCStructPipelineShaderStageCreateInfo (vkPStages (from :: GraphicsPipelineCreateInfo)) (\pStages -> maybeWith withSomeVkStruct (vkPNext (from :: GraphicsPipelineCreateInfo)) (\pPNext -> cont (VkGraphicsPipelineCreateInfo VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO pPNext (vkFlags (from :: GraphicsPipelineCreateInfo)) (fromIntegral (Data.Vector.length (vkPStages (from :: GraphicsPipelineCreateInfo)))) pStages pVertexInputState pInputAssemblyState pTessellationState pViewportState pRasterizationState pMultisampleState pDepthStencilState pColorBlendState pDynamicState (vkLayout (from :: GraphicsPipelineCreateInfo)) (vkRenderPass (from :: GraphicsPipelineCreateInfo)) (vkSubpass (from :: GraphicsPipelineCreateInfo)) (vkBasePipelineHandle (from :: GraphicsPipelineCreateInfo)) (vkBasePipelineIndex (from :: GraphicsPipelineCreateInfo))))))))))))))
fromCStructGraphicsPipelineCreateInfo :: VkGraphicsPipelineCreateInfo -> IO GraphicsPipelineCreateInfo
fromCStructGraphicsPipelineCreateInfo c = GraphicsPipelineCreateInfo <$> -- Univalued Member elided
                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkGraphicsPipelineCreateInfo)))
                                                                     <*> pure (vkFlags (c :: VkGraphicsPipelineCreateInfo))
                                                                     -- Length valued member elided
                                                                     <*> (Data.Vector.generateM (fromIntegral (vkStageCount (c :: VkGraphicsPipelineCreateInfo))) (((fromCStructPipelineShaderStageCreateInfo <=<) . peekElemOff) (vkPStages (c :: VkGraphicsPipelineCreateInfo))))
                                                                     <*> maybePeek (fromCStructPipelineVertexInputStateCreateInfo <=< peek) (vkPVertexInputState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineInputAssemblyStateCreateInfo <=< peek) (vkPInputAssemblyState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineTessellationStateCreateInfo <=< peek) (vkPTessellationState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineViewportStateCreateInfo <=< peek) (vkPViewportState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> (fromCStructPipelineRasterizationStateCreateInfo <=< peek) (vkPRasterizationState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineMultisampleStateCreateInfo <=< peek) (vkPMultisampleState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineDepthStencilStateCreateInfo <=< peek) (vkPDepthStencilState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineColorBlendStateCreateInfo <=< peek) (vkPColorBlendState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> maybePeek (fromCStructPipelineDynamicStateCreateInfo <=< peek) (vkPDynamicState (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkLayout (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkRenderPass (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkSubpass (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkBasePipelineHandle (c :: VkGraphicsPipelineCreateInfo))
                                                                     <*> pure (vkBasePipelineIndex (c :: VkGraphicsPipelineCreateInfo))
instance Zero GraphicsPipelineCreateInfo where
  zero = GraphicsPipelineCreateInfo Nothing
                                    zero
                                    Data.Vector.empty
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    zero
                                    Nothing
                                    Nothing
                                    Nothing
                                    Nothing
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
-- No documentation found for TopLevel "LogicOp"
type LogicOp = VkLogicOp
-- No documentation found for TopLevel "Offset2D"
data Offset2D = Offset2D
  { -- No documentation found for Nested "Offset2D" "x"
  vkX :: Int32
  , -- No documentation found for Nested "Offset2D" "y"
  vkY :: Int32
  }
  deriving (Show, Eq)
withCStructOffset2D :: Offset2D -> (VkOffset2D -> IO a) -> IO a
withCStructOffset2D from cont = cont (VkOffset2D (vkX (from :: Offset2D)) (vkY (from :: Offset2D)))
fromCStructOffset2D :: VkOffset2D -> IO Offset2D
fromCStructOffset2D c = Offset2D <$> pure (vkX (c :: VkOffset2D))
                                 <*> pure (vkY (c :: VkOffset2D))
instance Zero Offset2D where
  zero = Offset2D zero
                  zero
-- No documentation found for TopLevel "Pipeline"
type Pipeline = VkPipeline
-- No documentation found for TopLevel "PipelineColorBlendAttachmentState"
data PipelineColorBlendAttachmentState = PipelineColorBlendAttachmentState
  { -- No documentation found for Nested "PipelineColorBlendAttachmentState" "blendEnable"
  vkBlendEnable :: Bool
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "srcColorBlendFactor"
  vkSrcColorBlendFactor :: BlendFactor
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "dstColorBlendFactor"
  vkDstColorBlendFactor :: BlendFactor
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "colorBlendOp"
  vkColorBlendOp :: BlendOp
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "srcAlphaBlendFactor"
  vkSrcAlphaBlendFactor :: BlendFactor
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "dstAlphaBlendFactor"
  vkDstAlphaBlendFactor :: BlendFactor
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "alphaBlendOp"
  vkAlphaBlendOp :: BlendOp
  , -- No documentation found for Nested "PipelineColorBlendAttachmentState" "colorWriteMask"
  vkColorWriteMask :: ColorComponentFlags
  }
  deriving (Show, Eq)
withCStructPipelineColorBlendAttachmentState :: PipelineColorBlendAttachmentState -> (VkPipelineColorBlendAttachmentState -> IO a) -> IO a
withCStructPipelineColorBlendAttachmentState from cont = cont (VkPipelineColorBlendAttachmentState (boolToBool32 (vkBlendEnable (from :: PipelineColorBlendAttachmentState))) (vkSrcColorBlendFactor (from :: PipelineColorBlendAttachmentState)) (vkDstColorBlendFactor (from :: PipelineColorBlendAttachmentState)) (vkColorBlendOp (from :: PipelineColorBlendAttachmentState)) (vkSrcAlphaBlendFactor (from :: PipelineColorBlendAttachmentState)) (vkDstAlphaBlendFactor (from :: PipelineColorBlendAttachmentState)) (vkAlphaBlendOp (from :: PipelineColorBlendAttachmentState)) (vkColorWriteMask (from :: PipelineColorBlendAttachmentState)))
fromCStructPipelineColorBlendAttachmentState :: VkPipelineColorBlendAttachmentState -> IO PipelineColorBlendAttachmentState
fromCStructPipelineColorBlendAttachmentState c = PipelineColorBlendAttachmentState <$> pure (bool32ToBool (vkBlendEnable (c :: VkPipelineColorBlendAttachmentState)))
                                                                                   <*> pure (vkSrcColorBlendFactor (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkDstColorBlendFactor (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkColorBlendOp (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkSrcAlphaBlendFactor (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkDstAlphaBlendFactor (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkAlphaBlendOp (c :: VkPipelineColorBlendAttachmentState))
                                                                                   <*> pure (vkColorWriteMask (c :: VkPipelineColorBlendAttachmentState))
instance Zero PipelineColorBlendAttachmentState where
  zero = PipelineColorBlendAttachmentState False
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
                                           zero
-- No documentation found for TopLevel "PipelineColorBlendStateCreateFlags"
type PipelineColorBlendStateCreateFlags = VkPipelineColorBlendStateCreateFlags
-- No documentation found for TopLevel "PipelineColorBlendStateCreateInfo"
data PipelineColorBlendStateCreateInfo = PipelineColorBlendStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "flags"
  vkFlags :: PipelineColorBlendStateCreateFlags
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "logicOpEnable"
  vkLogicOpEnable :: Bool
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "logicOp"
  vkLogicOp :: LogicOp
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "pAttachments"
  vkPAttachments :: Vector PipelineColorBlendAttachmentState
  , -- No documentation found for Nested "PipelineColorBlendStateCreateInfo" "blendConstants"
  vkBlendConstants :: (CFloat, CFloat, CFloat, CFloat)
  }
  deriving (Show, Eq)
withCStructPipelineColorBlendStateCreateInfo :: PipelineColorBlendStateCreateInfo -> (VkPipelineColorBlendStateCreateInfo -> IO a) -> IO a
withCStructPipelineColorBlendStateCreateInfo from cont = withVec withCStructPipelineColorBlendAttachmentState (vkPAttachments (from :: PipelineColorBlendStateCreateInfo)) (\pAttachments -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineColorBlendStateCreateInfo)) (\pPNext -> cont (VkPipelineColorBlendStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineColorBlendStateCreateInfo)) (boolToBool32 (vkLogicOpEnable (from :: PipelineColorBlendStateCreateInfo))) (vkLogicOp (from :: PipelineColorBlendStateCreateInfo)) (fromIntegral (Data.Vector.length (vkPAttachments (from :: PipelineColorBlendStateCreateInfo)))) pAttachments (fromTuple (vkBlendConstants (from :: PipelineColorBlendStateCreateInfo))))))
fromCStructPipelineColorBlendStateCreateInfo :: VkPipelineColorBlendStateCreateInfo -> IO PipelineColorBlendStateCreateInfo
fromCStructPipelineColorBlendStateCreateInfo c = PipelineColorBlendStateCreateInfo <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineColorBlendStateCreateInfo)))
                                                                                   <*> pure (vkFlags (c :: VkPipelineColorBlendStateCreateInfo))
                                                                                   <*> pure (bool32ToBool (vkLogicOpEnable (c :: VkPipelineColorBlendStateCreateInfo)))
                                                                                   <*> pure (vkLogicOp (c :: VkPipelineColorBlendStateCreateInfo))
                                                                                   -- Length valued member elided
                                                                                   <*> (Data.Vector.generateM (fromIntegral (vkAttachmentCount (c :: VkPipelineColorBlendStateCreateInfo))) (((fromCStructPipelineColorBlendAttachmentState <=<) . peekElemOff) (vkPAttachments (c :: VkPipelineColorBlendStateCreateInfo))))
                                                                                   <*> pure (let x = (vkBlendConstants (c :: VkPipelineColorBlendStateCreateInfo)) in ( Data.Vector.Storable.Sized.unsafeIndex x 0
                                                                                   , Data.Vector.Storable.Sized.unsafeIndex x 1
                                                                                   , Data.Vector.Storable.Sized.unsafeIndex x 2
                                                                                   , Data.Vector.Storable.Sized.unsafeIndex x 3 ))
instance Zero PipelineColorBlendStateCreateInfo where
  zero = PipelineColorBlendStateCreateInfo Nothing
                                           zero
                                           False
                                           zero
                                           Data.Vector.empty
                                           (zero, zero, zero, zero)
-- No documentation found for TopLevel "PipelineCreateFlagBits"
type PipelineCreateFlagBits = VkPipelineCreateFlagBits
-- No documentation found for TopLevel "PipelineCreateFlags"
type PipelineCreateFlags = PipelineCreateFlagBits
-- No documentation found for TopLevel "PipelineDepthStencilStateCreateFlags"
type PipelineDepthStencilStateCreateFlags = VkPipelineDepthStencilStateCreateFlags
-- No documentation found for TopLevel "PipelineDepthStencilStateCreateInfo"
data PipelineDepthStencilStateCreateInfo = PipelineDepthStencilStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "flags"
  vkFlags :: PipelineDepthStencilStateCreateFlags
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "depthTestEnable"
  vkDepthTestEnable :: Bool
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "depthWriteEnable"
  vkDepthWriteEnable :: Bool
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "depthCompareOp"
  vkDepthCompareOp :: CompareOp
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "depthBoundsTestEnable"
  vkDepthBoundsTestEnable :: Bool
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "stencilTestEnable"
  vkStencilTestEnable :: Bool
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "front"
  vkFront :: StencilOpState
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "back"
  vkBack :: StencilOpState
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "minDepthBounds"
  vkMinDepthBounds :: CFloat
  , -- No documentation found for Nested "PipelineDepthStencilStateCreateInfo" "maxDepthBounds"
  vkMaxDepthBounds :: CFloat
  }
  deriving (Show, Eq)
withCStructPipelineDepthStencilStateCreateInfo :: PipelineDepthStencilStateCreateInfo -> (VkPipelineDepthStencilStateCreateInfo -> IO a) -> IO a
withCStructPipelineDepthStencilStateCreateInfo from cont = withCStructStencilOpState (vkBack (from :: PipelineDepthStencilStateCreateInfo)) (\back -> withCStructStencilOpState (vkFront (from :: PipelineDepthStencilStateCreateInfo)) (\front -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineDepthStencilStateCreateInfo)) (\pPNext -> cont (VkPipelineDepthStencilStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineDepthStencilStateCreateInfo)) (boolToBool32 (vkDepthTestEnable (from :: PipelineDepthStencilStateCreateInfo))) (boolToBool32 (vkDepthWriteEnable (from :: PipelineDepthStencilStateCreateInfo))) (vkDepthCompareOp (from :: PipelineDepthStencilStateCreateInfo)) (boolToBool32 (vkDepthBoundsTestEnable (from :: PipelineDepthStencilStateCreateInfo))) (boolToBool32 (vkStencilTestEnable (from :: PipelineDepthStencilStateCreateInfo))) front back (vkMinDepthBounds (from :: PipelineDepthStencilStateCreateInfo)) (vkMaxDepthBounds (from :: PipelineDepthStencilStateCreateInfo))))))
fromCStructPipelineDepthStencilStateCreateInfo :: VkPipelineDepthStencilStateCreateInfo -> IO PipelineDepthStencilStateCreateInfo
fromCStructPipelineDepthStencilStateCreateInfo c = PipelineDepthStencilStateCreateInfo <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (vkFlags (c :: VkPipelineDepthStencilStateCreateInfo))
                                                                                       <*> pure (bool32ToBool (vkDepthTestEnable (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (bool32ToBool (vkDepthWriteEnable (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (vkDepthCompareOp (c :: VkPipelineDepthStencilStateCreateInfo))
                                                                                       <*> pure (bool32ToBool (vkDepthBoundsTestEnable (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (bool32ToBool (vkStencilTestEnable (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> (fromCStructStencilOpState (vkFront (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> (fromCStructStencilOpState (vkBack (c :: VkPipelineDepthStencilStateCreateInfo)))
                                                                                       <*> pure (vkMinDepthBounds (c :: VkPipelineDepthStencilStateCreateInfo))
                                                                                       <*> pure (vkMaxDepthBounds (c :: VkPipelineDepthStencilStateCreateInfo))
instance Zero PipelineDepthStencilStateCreateInfo where
  zero = PipelineDepthStencilStateCreateInfo Nothing
                                             zero
                                             False
                                             False
                                             zero
                                             False
                                             False
                                             zero
                                             zero
                                             zero
                                             zero
-- No documentation found for TopLevel "PipelineDynamicStateCreateFlags"
type PipelineDynamicStateCreateFlags = VkPipelineDynamicStateCreateFlags
-- No documentation found for TopLevel "PipelineDynamicStateCreateInfo"
data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "flags"
  vkFlags :: PipelineDynamicStateCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineDynamicStateCreateInfo" "pDynamicStates"
  vkPDynamicStates :: Vector DynamicState
  }
  deriving (Show, Eq)
withCStructPipelineDynamicStateCreateInfo :: PipelineDynamicStateCreateInfo -> (VkPipelineDynamicStateCreateInfo -> IO a) -> IO a
withCStructPipelineDynamicStateCreateInfo from cont = withVec (&) (vkPDynamicStates (from :: PipelineDynamicStateCreateInfo)) (\pDynamicStates -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineDynamicStateCreateInfo)) (\pPNext -> cont (VkPipelineDynamicStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineDynamicStateCreateInfo)) (fromIntegral (Data.Vector.length (vkPDynamicStates (from :: PipelineDynamicStateCreateInfo)))) pDynamicStates)))
fromCStructPipelineDynamicStateCreateInfo :: VkPipelineDynamicStateCreateInfo -> IO PipelineDynamicStateCreateInfo
fromCStructPipelineDynamicStateCreateInfo c = PipelineDynamicStateCreateInfo <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineDynamicStateCreateInfo)))
                                                                             <*> pure (vkFlags (c :: VkPipelineDynamicStateCreateInfo))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkDynamicStateCount (c :: VkPipelineDynamicStateCreateInfo))) (peekElemOff (vkPDynamicStates (c :: VkPipelineDynamicStateCreateInfo))))
instance Zero PipelineDynamicStateCreateInfo where
  zero = PipelineDynamicStateCreateInfo Nothing
                                        zero
                                        Data.Vector.empty
-- No documentation found for TopLevel "PipelineInputAssemblyStateCreateFlags"
type PipelineInputAssemblyStateCreateFlags = VkPipelineInputAssemblyStateCreateFlags
-- No documentation found for TopLevel "PipelineInputAssemblyStateCreateInfo"
data PipelineInputAssemblyStateCreateInfo = PipelineInputAssemblyStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "flags"
  vkFlags :: PipelineInputAssemblyStateCreateFlags
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "topology"
  vkTopology :: PrimitiveTopology
  , -- No documentation found for Nested "PipelineInputAssemblyStateCreateInfo" "primitiveRestartEnable"
  vkPrimitiveRestartEnable :: Bool
  }
  deriving (Show, Eq)
withCStructPipelineInputAssemblyStateCreateInfo :: PipelineInputAssemblyStateCreateInfo -> (VkPipelineInputAssemblyStateCreateInfo -> IO a) -> IO a
withCStructPipelineInputAssemblyStateCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineInputAssemblyStateCreateInfo)) (\pPNext -> cont (VkPipelineInputAssemblyStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineInputAssemblyStateCreateInfo)) (vkTopology (from :: PipelineInputAssemblyStateCreateInfo)) (boolToBool32 (vkPrimitiveRestartEnable (from :: PipelineInputAssemblyStateCreateInfo)))))
fromCStructPipelineInputAssemblyStateCreateInfo :: VkPipelineInputAssemblyStateCreateInfo -> IO PipelineInputAssemblyStateCreateInfo
fromCStructPipelineInputAssemblyStateCreateInfo c = PipelineInputAssemblyStateCreateInfo <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineInputAssemblyStateCreateInfo)))
                                                                                         <*> pure (vkFlags (c :: VkPipelineInputAssemblyStateCreateInfo))
                                                                                         <*> pure (vkTopology (c :: VkPipelineInputAssemblyStateCreateInfo))
                                                                                         <*> pure (bool32ToBool (vkPrimitiveRestartEnable (c :: VkPipelineInputAssemblyStateCreateInfo)))
instance Zero PipelineInputAssemblyStateCreateInfo where
  zero = PipelineInputAssemblyStateCreateInfo Nothing
                                              zero
                                              zero
                                              False
-- No documentation found for TopLevel "PipelineLayout"
type PipelineLayout = VkPipelineLayout
-- No documentation found for TopLevel "PipelineMultisampleStateCreateFlags"
type PipelineMultisampleStateCreateFlags = VkPipelineMultisampleStateCreateFlags
-- No documentation found for TopLevel "PipelineMultisampleStateCreateInfo"
data PipelineMultisampleStateCreateInfo = PipelineMultisampleStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "flags"
  vkFlags :: PipelineMultisampleStateCreateFlags
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "rasterizationSamples"
  vkRasterizationSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "sampleShadingEnable"
  vkSampleShadingEnable :: Bool
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "minSampleShading"
  vkMinSampleShading :: CFloat
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "pSampleMask"
  vkPSampleMask :: Vector SampleMask
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "alphaToCoverageEnable"
  vkAlphaToCoverageEnable :: Bool
  , -- No documentation found for Nested "PipelineMultisampleStateCreateInfo" "alphaToOneEnable"
  vkAlphaToOneEnable :: Bool
  }
  deriving (Show, Eq)
withCStructPipelineMultisampleStateCreateInfo :: PipelineMultisampleStateCreateInfo -> (VkPipelineMultisampleStateCreateInfo -> IO a) -> IO a
withCStructPipelineMultisampleStateCreateInfo from cont = withVec (flip ($)) (padVector zeroBits (fromIntegral ((coerce (vkRasterizationSamples (from :: PipelineMultisampleStateCreateInfo)) :: VkFlags) + 31) `quot` 32) (vkPSampleMask (from :: PipelineMultisampleStateCreateInfo))) (\pSampleMask -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineMultisampleStateCreateInfo)) (\pPNext -> cont (VkPipelineMultisampleStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineMultisampleStateCreateInfo)) (vkRasterizationSamples (from :: PipelineMultisampleStateCreateInfo)) (boolToBool32 (vkSampleShadingEnable (from :: PipelineMultisampleStateCreateInfo))) (vkMinSampleShading (from :: PipelineMultisampleStateCreateInfo)) pSampleMask (boolToBool32 (vkAlphaToCoverageEnable (from :: PipelineMultisampleStateCreateInfo))) (boolToBool32 (vkAlphaToOneEnable (from :: PipelineMultisampleStateCreateInfo))))))
fromCStructPipelineMultisampleStateCreateInfo :: VkPipelineMultisampleStateCreateInfo -> IO PipelineMultisampleStateCreateInfo
fromCStructPipelineMultisampleStateCreateInfo c = PipelineMultisampleStateCreateInfo <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineMultisampleStateCreateInfo)))
                                                                                     <*> pure (vkFlags (c :: VkPipelineMultisampleStateCreateInfo))
                                                                                     <*> pure (vkRasterizationSamples (c :: VkPipelineMultisampleStateCreateInfo))
                                                                                     <*> pure (bool32ToBool (vkSampleShadingEnable (c :: VkPipelineMultisampleStateCreateInfo)))
                                                                                     <*> pure (vkMinSampleShading (c :: VkPipelineMultisampleStateCreateInfo))
                                                                                     <*> (Data.Vector.generateM (fromIntegral (((coerce (vkRasterizationSamples (c :: VkPipelineMultisampleStateCreateInfo)) :: VkFlags) + 31) `quot` 32)) (peekElemOff (vkPSampleMask (c :: VkPipelineMultisampleStateCreateInfo))))
                                                                                     <*> pure (bool32ToBool (vkAlphaToCoverageEnable (c :: VkPipelineMultisampleStateCreateInfo)))
                                                                                     <*> pure (bool32ToBool (vkAlphaToOneEnable (c :: VkPipelineMultisampleStateCreateInfo)))
instance Zero PipelineMultisampleStateCreateInfo where
  zero = PipelineMultisampleStateCreateInfo Nothing
                                            zero
                                            zero
                                            False
                                            zero
                                            Data.Vector.empty
                                            False
                                            False
-- No documentation found for TopLevel "PipelineRasterizationStateCreateFlags"
type PipelineRasterizationStateCreateFlags = VkPipelineRasterizationStateCreateFlags
-- No documentation found for TopLevel "PipelineRasterizationStateCreateInfo"
data PipelineRasterizationStateCreateInfo = PipelineRasterizationStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "flags"
  vkFlags :: PipelineRasterizationStateCreateFlags
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthClampEnable"
  vkDepthClampEnable :: Bool
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "rasterizerDiscardEnable"
  vkRasterizerDiscardEnable :: Bool
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "polygonMode"
  vkPolygonMode :: PolygonMode
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "cullMode"
  vkCullMode :: CullModeFlags
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "frontFace"
  vkFrontFace :: FrontFace
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasEnable"
  vkDepthBiasEnable :: Bool
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasConstantFactor"
  vkDepthBiasConstantFactor :: CFloat
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasClamp"
  vkDepthBiasClamp :: CFloat
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "depthBiasSlopeFactor"
  vkDepthBiasSlopeFactor :: CFloat
  , -- No documentation found for Nested "PipelineRasterizationStateCreateInfo" "lineWidth"
  vkLineWidth :: CFloat
  }
  deriving (Show, Eq)
withCStructPipelineRasterizationStateCreateInfo :: PipelineRasterizationStateCreateInfo -> (VkPipelineRasterizationStateCreateInfo -> IO a) -> IO a
withCStructPipelineRasterizationStateCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineRasterizationStateCreateInfo)) (\pPNext -> cont (VkPipelineRasterizationStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineRasterizationStateCreateInfo)) (boolToBool32 (vkDepthClampEnable (from :: PipelineRasterizationStateCreateInfo))) (boolToBool32 (vkRasterizerDiscardEnable (from :: PipelineRasterizationStateCreateInfo))) (vkPolygonMode (from :: PipelineRasterizationStateCreateInfo)) (vkCullMode (from :: PipelineRasterizationStateCreateInfo)) (vkFrontFace (from :: PipelineRasterizationStateCreateInfo)) (boolToBool32 (vkDepthBiasEnable (from :: PipelineRasterizationStateCreateInfo))) (vkDepthBiasConstantFactor (from :: PipelineRasterizationStateCreateInfo)) (vkDepthBiasClamp (from :: PipelineRasterizationStateCreateInfo)) (vkDepthBiasSlopeFactor (from :: PipelineRasterizationStateCreateInfo)) (vkLineWidth (from :: PipelineRasterizationStateCreateInfo))))
fromCStructPipelineRasterizationStateCreateInfo :: VkPipelineRasterizationStateCreateInfo -> IO PipelineRasterizationStateCreateInfo
fromCStructPipelineRasterizationStateCreateInfo c = PipelineRasterizationStateCreateInfo <$> -- Univalued Member elided
                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineRasterizationStateCreateInfo)))
                                                                                         <*> pure (vkFlags (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (bool32ToBool (vkDepthClampEnable (c :: VkPipelineRasterizationStateCreateInfo)))
                                                                                         <*> pure (bool32ToBool (vkRasterizerDiscardEnable (c :: VkPipelineRasterizationStateCreateInfo)))
                                                                                         <*> pure (vkPolygonMode (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkCullMode (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkFrontFace (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (bool32ToBool (vkDepthBiasEnable (c :: VkPipelineRasterizationStateCreateInfo)))
                                                                                         <*> pure (vkDepthBiasConstantFactor (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkDepthBiasClamp (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkDepthBiasSlopeFactor (c :: VkPipelineRasterizationStateCreateInfo))
                                                                                         <*> pure (vkLineWidth (c :: VkPipelineRasterizationStateCreateInfo))
instance Zero PipelineRasterizationStateCreateInfo where
  zero = PipelineRasterizationStateCreateInfo Nothing
                                              zero
                                              False
                                              False
                                              zero
                                              zero
                                              zero
                                              False
                                              zero
                                              zero
                                              zero
                                              zero
-- No documentation found for TopLevel "PipelineShaderStageCreateFlags"
type PipelineShaderStageCreateFlags = VkPipelineShaderStageCreateFlags
-- No documentation found for TopLevel "PipelineShaderStageCreateInfo"
data PipelineShaderStageCreateInfo = PipelineShaderStageCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineShaderStageCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "flags"
  vkFlags :: PipelineShaderStageCreateFlags
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "stage"
  vkStage :: ShaderStageFlagBits
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "module"
  vkModule :: ShaderModule
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "pName"
  vkPName :: ByteString
  , -- No documentation found for Nested "PipelineShaderStageCreateInfo" "pSpecializationInfo"
  vkPSpecializationInfo :: Maybe SpecializationInfo
  }
  deriving (Show, Eq)
withCStructPipelineShaderStageCreateInfo :: PipelineShaderStageCreateInfo -> (VkPipelineShaderStageCreateInfo -> IO a) -> IO a
withCStructPipelineShaderStageCreateInfo from cont = maybeWith (\a -> withCStructSpecializationInfo a . flip with) (vkPSpecializationInfo (from :: PipelineShaderStageCreateInfo)) (\pSpecializationInfo -> useAsCString (vkPName (from :: PipelineShaderStageCreateInfo)) (\pName -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineShaderStageCreateInfo)) (\pPNext -> cont (VkPipelineShaderStageCreateInfo VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO pPNext (vkFlags (from :: PipelineShaderStageCreateInfo)) (vkStage (from :: PipelineShaderStageCreateInfo)) (vkModule (from :: PipelineShaderStageCreateInfo)) pName pSpecializationInfo))))
fromCStructPipelineShaderStageCreateInfo :: VkPipelineShaderStageCreateInfo -> IO PipelineShaderStageCreateInfo
fromCStructPipelineShaderStageCreateInfo c = PipelineShaderStageCreateInfo <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineShaderStageCreateInfo)))
                                                                           <*> pure (vkFlags (c :: VkPipelineShaderStageCreateInfo))
                                                                           <*> pure (vkStage (c :: VkPipelineShaderStageCreateInfo))
                                                                           <*> pure (vkModule (c :: VkPipelineShaderStageCreateInfo))
                                                                           <*> packCString (vkPName (c :: VkPipelineShaderStageCreateInfo))
                                                                           <*> maybePeek (fromCStructSpecializationInfo <=< peek) (vkPSpecializationInfo (c :: VkPipelineShaderStageCreateInfo))
instance Zero PipelineShaderStageCreateInfo where
  zero = PipelineShaderStageCreateInfo Nothing
                                       zero
                                       zero
                                       zero
                                       Data.ByteString.empty
                                       Nothing
-- No documentation found for TopLevel "PipelineTessellationStateCreateFlags"
type PipelineTessellationStateCreateFlags = VkPipelineTessellationStateCreateFlags
-- No documentation found for TopLevel "PipelineTessellationStateCreateInfo"
data PipelineTessellationStateCreateInfo = PipelineTessellationStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "flags"
  vkFlags :: PipelineTessellationStateCreateFlags
  , -- No documentation found for Nested "PipelineTessellationStateCreateInfo" "patchControlPoints"
  vkPatchControlPoints :: Word32
  }
  deriving (Show, Eq)
withCStructPipelineTessellationStateCreateInfo :: PipelineTessellationStateCreateInfo -> (VkPipelineTessellationStateCreateInfo -> IO a) -> IO a
withCStructPipelineTessellationStateCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineTessellationStateCreateInfo)) (\pPNext -> cont (VkPipelineTessellationStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineTessellationStateCreateInfo)) (vkPatchControlPoints (from :: PipelineTessellationStateCreateInfo))))
fromCStructPipelineTessellationStateCreateInfo :: VkPipelineTessellationStateCreateInfo -> IO PipelineTessellationStateCreateInfo
fromCStructPipelineTessellationStateCreateInfo c = PipelineTessellationStateCreateInfo <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineTessellationStateCreateInfo)))
                                                                                       <*> pure (vkFlags (c :: VkPipelineTessellationStateCreateInfo))
                                                                                       <*> pure (vkPatchControlPoints (c :: VkPipelineTessellationStateCreateInfo))
instance Zero PipelineTessellationStateCreateInfo where
  zero = PipelineTessellationStateCreateInfo Nothing
                                             zero
                                             zero
-- No documentation found for TopLevel "PipelineVertexInputStateCreateFlags"
type PipelineVertexInputStateCreateFlags = VkPipelineVertexInputStateCreateFlags
-- No documentation found for TopLevel "PipelineVertexInputStateCreateInfo"
data PipelineVertexInputStateCreateInfo = PipelineVertexInputStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "flags"
  vkFlags :: PipelineVertexInputStateCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pVertexBindingDescriptions"
  vkPVertexBindingDescriptions :: Vector VertexInputBindingDescription
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineVertexInputStateCreateInfo" "pVertexAttributeDescriptions"
  vkPVertexAttributeDescriptions :: Vector VertexInputAttributeDescription
  }
  deriving (Show, Eq)
withCStructPipelineVertexInputStateCreateInfo :: PipelineVertexInputStateCreateInfo -> (VkPipelineVertexInputStateCreateInfo -> IO a) -> IO a
withCStructPipelineVertexInputStateCreateInfo from cont = withVec withCStructVertexInputAttributeDescription (vkPVertexAttributeDescriptions (from :: PipelineVertexInputStateCreateInfo)) (\pVertexAttributeDescriptions -> withVec withCStructVertexInputBindingDescription (vkPVertexBindingDescriptions (from :: PipelineVertexInputStateCreateInfo)) (\pVertexBindingDescriptions -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineVertexInputStateCreateInfo)) (\pPNext -> cont (VkPipelineVertexInputStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineVertexInputStateCreateInfo)) (fromIntegral (Data.Vector.length (vkPVertexBindingDescriptions (from :: PipelineVertexInputStateCreateInfo)))) pVertexBindingDescriptions (fromIntegral (Data.Vector.length (vkPVertexAttributeDescriptions (from :: PipelineVertexInputStateCreateInfo)))) pVertexAttributeDescriptions))))
fromCStructPipelineVertexInputStateCreateInfo :: VkPipelineVertexInputStateCreateInfo -> IO PipelineVertexInputStateCreateInfo
fromCStructPipelineVertexInputStateCreateInfo c = PipelineVertexInputStateCreateInfo <$> -- Univalued Member elided
                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineVertexInputStateCreateInfo)))
                                                                                     <*> pure (vkFlags (c :: VkPipelineVertexInputStateCreateInfo))
                                                                                     -- Length valued member elided
                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkVertexBindingDescriptionCount (c :: VkPipelineVertexInputStateCreateInfo))) (((fromCStructVertexInputBindingDescription <=<) . peekElemOff) (vkPVertexBindingDescriptions (c :: VkPipelineVertexInputStateCreateInfo))))
                                                                                     -- Length valued member elided
                                                                                     <*> (Data.Vector.generateM (fromIntegral (vkVertexAttributeDescriptionCount (c :: VkPipelineVertexInputStateCreateInfo))) (((fromCStructVertexInputAttributeDescription <=<) . peekElemOff) (vkPVertexAttributeDescriptions (c :: VkPipelineVertexInputStateCreateInfo))))
instance Zero PipelineVertexInputStateCreateInfo where
  zero = PipelineVertexInputStateCreateInfo Nothing
                                            zero
                                            Data.Vector.empty
                                            Data.Vector.empty
-- No documentation found for TopLevel "PipelineViewportStateCreateFlags"
type PipelineViewportStateCreateFlags = VkPipelineViewportStateCreateFlags
-- No documentation found for TopLevel "PipelineViewportStateCreateInfo"
data PipelineViewportStateCreateInfo = PipelineViewportStateCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "flags"
  vkFlags :: PipelineViewportStateCreateFlags
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pViewports"
  vkPViewports :: Maybe (Vector Viewport)
  -- Optional length valued member elided
  , -- No documentation found for Nested "PipelineViewportStateCreateInfo" "pScissors"
  vkPScissors :: Maybe (Vector Rect2D)
  }
  deriving (Show, Eq)
withCStructPipelineViewportStateCreateInfo :: PipelineViewportStateCreateInfo -> (VkPipelineViewportStateCreateInfo -> IO a) -> IO a
withCStructPipelineViewportStateCreateInfo from cont = maybeWith (withVec withCStructRect2D) (vkPScissors (from :: PipelineViewportStateCreateInfo)) (\pScissors -> maybeWith (withVec withCStructViewport) (vkPViewports (from :: PipelineViewportStateCreateInfo)) (\pViewports -> maybeWith withSomeVkStruct (vkPNext (from :: PipelineViewportStateCreateInfo)) (\pPNext -> cont (VkPipelineViewportStateCreateInfo VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO pPNext (vkFlags (from :: PipelineViewportStateCreateInfo)) (maybe 0 (fromIntegral . Data.Vector.length) (vkPViewports (from :: PipelineViewportStateCreateInfo))) pViewports (maybe 0 (fromIntegral . Data.Vector.length) (vkPScissors (from :: PipelineViewportStateCreateInfo))) pScissors))))
fromCStructPipelineViewportStateCreateInfo :: VkPipelineViewportStateCreateInfo -> IO PipelineViewportStateCreateInfo
fromCStructPipelineViewportStateCreateInfo c = PipelineViewportStateCreateInfo <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineViewportStateCreateInfo)))
                                                                               <*> pure (vkFlags (c :: VkPipelineViewportStateCreateInfo))
                                                                               -- Optional length valued member elided
                                                                               <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkViewportCount (c :: VkPipelineViewportStateCreateInfo))) (((fromCStructViewport <=<) . peekElemOff) p)) (vkPViewports (c :: VkPipelineViewportStateCreateInfo))
                                                                               -- Optional length valued member elided
                                                                               <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkScissorCount (c :: VkPipelineViewportStateCreateInfo))) (((fromCStructRect2D <=<) . peekElemOff) p)) (vkPScissors (c :: VkPipelineViewportStateCreateInfo))
instance Zero PipelineViewportStateCreateInfo where
  zero = PipelineViewportStateCreateInfo Nothing
                                         zero
                                         Nothing
                                         Nothing
-- No documentation found for TopLevel "PolygonMode"
type PolygonMode = VkPolygonMode
-- No documentation found for TopLevel "PrimitiveTopology"
type PrimitiveTopology = VkPrimitiveTopology
-- No documentation found for TopLevel "Rect2D"
data Rect2D = Rect2D
  { -- No documentation found for Nested "Rect2D" "offset"
  vkOffset :: Offset2D
  , -- No documentation found for Nested "Rect2D" "extent"
  vkExtent :: Extent2D
  }
  deriving (Show, Eq)
withCStructRect2D :: Rect2D -> (VkRect2D -> IO a) -> IO a
withCStructRect2D from cont = withCStructExtent2D (vkExtent (from :: Rect2D)) (\extent -> withCStructOffset2D (vkOffset (from :: Rect2D)) (\offset -> cont (VkRect2D offset extent)))
fromCStructRect2D :: VkRect2D -> IO Rect2D
fromCStructRect2D c = Rect2D <$> (fromCStructOffset2D (vkOffset (c :: VkRect2D)))
                             <*> (fromCStructExtent2D (vkExtent (c :: VkRect2D)))
instance Zero Rect2D where
  zero = Rect2D zero
                zero
-- No documentation found for TopLevel "RenderPass"
type RenderPass = VkRenderPass
-- No documentation found for TopLevel "SampleMask"
type SampleMask = VkSampleMask
  
-- No documentation found for TopLevel "ShaderStageFlagBits"
type ShaderStageFlagBits = VkShaderStageFlagBits
-- No documentation found for TopLevel "SpecializationInfo"
data SpecializationInfo = SpecializationInfo
  { -- Length valued member elided
  -- No documentation found for Nested "SpecializationInfo" "pMapEntries"
  vkPMapEntries :: Vector SpecializationMapEntry
  -- Bytestring length valued member elided
  , -- No documentation found for Nested "SpecializationInfo" "pData"
  vkPData :: ByteString
  }
  deriving (Show, Eq)
withCStructSpecializationInfo :: SpecializationInfo -> (VkSpecializationInfo -> IO a) -> IO a
withCStructSpecializationInfo from cont = unsafeUseAsCString (vkPData (from :: SpecializationInfo)) (\pData -> withVec withCStructSpecializationMapEntry (vkPMapEntries (from :: SpecializationInfo)) (\pMapEntries -> cont (VkSpecializationInfo (fromIntegral (Data.Vector.length (vkPMapEntries (from :: SpecializationInfo)))) pMapEntries (fromIntegral (Data.ByteString.length (vkPData (from :: SpecializationInfo)))) (castPtr pData))))
fromCStructSpecializationInfo :: VkSpecializationInfo -> IO SpecializationInfo
fromCStructSpecializationInfo c = SpecializationInfo <$> -- Length valued member elided
                                                     (Data.Vector.generateM (fromIntegral (vkMapEntryCount (c :: VkSpecializationInfo))) (((fromCStructSpecializationMapEntry <=<) . peekElemOff) (vkPMapEntries (c :: VkSpecializationInfo))))
                                                     -- Bytestring length valued member elided
                                                     <*> packCStringLen (castPtr (vkPData (c :: VkSpecializationInfo)), fromIntegral (vkDataSize (c :: VkSpecializationInfo)))
instance Zero SpecializationInfo where
  zero = SpecializationInfo Data.Vector.empty
                            Data.ByteString.empty
-- No documentation found for TopLevel "SpecializationMapEntry"
data SpecializationMapEntry = SpecializationMapEntry
  { -- No documentation found for Nested "SpecializationMapEntry" "constantID"
  vkConstantID :: Word32
  , -- No documentation found for Nested "SpecializationMapEntry" "offset"
  vkOffset :: Word32
  , -- No documentation found for Nested "SpecializationMapEntry" "size"
  vkSize :: CSize
  }
  deriving (Show, Eq)
withCStructSpecializationMapEntry :: SpecializationMapEntry -> (VkSpecializationMapEntry -> IO a) -> IO a
withCStructSpecializationMapEntry from cont = cont (VkSpecializationMapEntry (vkConstantID (from :: SpecializationMapEntry)) (vkOffset (from :: SpecializationMapEntry)) (vkSize (from :: SpecializationMapEntry)))
fromCStructSpecializationMapEntry :: VkSpecializationMapEntry -> IO SpecializationMapEntry
fromCStructSpecializationMapEntry c = SpecializationMapEntry <$> pure (vkConstantID (c :: VkSpecializationMapEntry))
                                                             <*> pure (vkOffset (c :: VkSpecializationMapEntry))
                                                             <*> pure (vkSize (c :: VkSpecializationMapEntry))
instance Zero SpecializationMapEntry where
  zero = SpecializationMapEntry zero
                                zero
                                zero
-- No documentation found for TopLevel "StencilOp"
type StencilOp = VkStencilOp
-- No documentation found for TopLevel "StencilOpState"
data StencilOpState = StencilOpState
  { -- No documentation found for Nested "StencilOpState" "failOp"
  vkFailOp :: StencilOp
  , -- No documentation found for Nested "StencilOpState" "passOp"
  vkPassOp :: StencilOp
  , -- No documentation found for Nested "StencilOpState" "depthFailOp"
  vkDepthFailOp :: StencilOp
  , -- No documentation found for Nested "StencilOpState" "compareOp"
  vkCompareOp :: CompareOp
  , -- No documentation found for Nested "StencilOpState" "compareMask"
  vkCompareMask :: Word32
  , -- No documentation found for Nested "StencilOpState" "writeMask"
  vkWriteMask :: Word32
  , -- No documentation found for Nested "StencilOpState" "reference"
  vkReference :: Word32
  }
  deriving (Show, Eq)
withCStructStencilOpState :: StencilOpState -> (VkStencilOpState -> IO a) -> IO a
withCStructStencilOpState from cont = cont (VkStencilOpState (vkFailOp (from :: StencilOpState)) (vkPassOp (from :: StencilOpState)) (vkDepthFailOp (from :: StencilOpState)) (vkCompareOp (from :: StencilOpState)) (vkCompareMask (from :: StencilOpState)) (vkWriteMask (from :: StencilOpState)) (vkReference (from :: StencilOpState)))
fromCStructStencilOpState :: VkStencilOpState -> IO StencilOpState
fromCStructStencilOpState c = StencilOpState <$> pure (vkFailOp (c :: VkStencilOpState))
                                             <*> pure (vkPassOp (c :: VkStencilOpState))
                                             <*> pure (vkDepthFailOp (c :: VkStencilOpState))
                                             <*> pure (vkCompareOp (c :: VkStencilOpState))
                                             <*> pure (vkCompareMask (c :: VkStencilOpState))
                                             <*> pure (vkWriteMask (c :: VkStencilOpState))
                                             <*> pure (vkReference (c :: VkStencilOpState))
instance Zero StencilOpState where
  zero = StencilOpState zero
                        zero
                        zero
                        zero
                        zero
                        zero
                        zero
-- No documentation found for TopLevel "VertexInputAttributeDescription"
data VertexInputAttributeDescription = VertexInputAttributeDescription
  { -- No documentation found for Nested "VertexInputAttributeDescription" "location"
  vkLocation :: Word32
  , -- No documentation found for Nested "VertexInputAttributeDescription" "binding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VertexInputAttributeDescription" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "VertexInputAttributeDescription" "offset"
  vkOffset :: Word32
  }
  deriving (Show, Eq)
withCStructVertexInputAttributeDescription :: VertexInputAttributeDescription -> (VkVertexInputAttributeDescription -> IO a) -> IO a
withCStructVertexInputAttributeDescription from cont = cont (VkVertexInputAttributeDescription (vkLocation (from :: VertexInputAttributeDescription)) (vkBinding (from :: VertexInputAttributeDescription)) (vkFormat (from :: VertexInputAttributeDescription)) (vkOffset (from :: VertexInputAttributeDescription)))
fromCStructVertexInputAttributeDescription :: VkVertexInputAttributeDescription -> IO VertexInputAttributeDescription
fromCStructVertexInputAttributeDescription c = VertexInputAttributeDescription <$> pure (vkLocation (c :: VkVertexInputAttributeDescription))
                                                                               <*> pure (vkBinding (c :: VkVertexInputAttributeDescription))
                                                                               <*> pure (vkFormat (c :: VkVertexInputAttributeDescription))
                                                                               <*> pure (vkOffset (c :: VkVertexInputAttributeDescription))
instance Zero VertexInputAttributeDescription where
  zero = VertexInputAttributeDescription zero
                                         zero
                                         zero
                                         zero
-- No documentation found for TopLevel "VertexInputBindingDescription"
data VertexInputBindingDescription = VertexInputBindingDescription
  { -- No documentation found for Nested "VertexInputBindingDescription" "binding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VertexInputBindingDescription" "stride"
  vkStride :: Word32
  , -- No documentation found for Nested "VertexInputBindingDescription" "inputRate"
  vkInputRate :: VertexInputRate
  }
  deriving (Show, Eq)
withCStructVertexInputBindingDescription :: VertexInputBindingDescription -> (VkVertexInputBindingDescription -> IO a) -> IO a
withCStructVertexInputBindingDescription from cont = cont (VkVertexInputBindingDescription (vkBinding (from :: VertexInputBindingDescription)) (vkStride (from :: VertexInputBindingDescription)) (vkInputRate (from :: VertexInputBindingDescription)))
fromCStructVertexInputBindingDescription :: VkVertexInputBindingDescription -> IO VertexInputBindingDescription
fromCStructVertexInputBindingDescription c = VertexInputBindingDescription <$> pure (vkBinding (c :: VkVertexInputBindingDescription))
                                                                           <*> pure (vkStride (c :: VkVertexInputBindingDescription))
                                                                           <*> pure (vkInputRate (c :: VkVertexInputBindingDescription))
instance Zero VertexInputBindingDescription where
  zero = VertexInputBindingDescription zero
                                       zero
                                       zero
-- No documentation found for TopLevel "VertexInputRate"
type VertexInputRate = VkVertexInputRate
-- No documentation found for TopLevel "Viewport"
data Viewport = Viewport
  { -- No documentation found for Nested "Viewport" "x"
  vkX :: CFloat
  , -- No documentation found for Nested "Viewport" "y"
  vkY :: CFloat
  , -- No documentation found for Nested "Viewport" "width"
  vkWidth :: CFloat
  , -- No documentation found for Nested "Viewport" "height"
  vkHeight :: CFloat
  , -- No documentation found for Nested "Viewport" "minDepth"
  vkMinDepth :: CFloat
  , -- No documentation found for Nested "Viewport" "maxDepth"
  vkMaxDepth :: CFloat
  }
  deriving (Show, Eq)
withCStructViewport :: Viewport -> (VkViewport -> IO a) -> IO a
withCStructViewport from cont = cont (VkViewport (vkX (from :: Viewport)) (vkY (from :: Viewport)) (vkWidth (from :: Viewport)) (vkHeight (from :: Viewport)) (vkMinDepth (from :: Viewport)) (vkMaxDepth (from :: Viewport)))
fromCStructViewport :: VkViewport -> IO Viewport
fromCStructViewport c = Viewport <$> pure (vkX (c :: VkViewport))
                                 <*> pure (vkY (c :: VkViewport))
                                 <*> pure (vkWidth (c :: VkViewport))
                                 <*> pure (vkHeight (c :: VkViewport))
                                 <*> pure (vkMinDepth (c :: VkViewport))
                                 <*> pure (vkMaxDepth (c :: VkViewport))
instance Zero Viewport where
  zero = Viewport zero
                  zero
                  zero
                  zero
                  zero
                  zero

-- | Wrapper for 'vkCreateComputePipelines'
createComputePipelines :: Device ->  PipelineCache ->  Vector ComputePipelineCreateInfo ->  Maybe AllocationCallbacks ->  IO (Vector Pipeline)
createComputePipelines = \(Device device commandTable) -> \pipelineCache -> \createInfos -> \allocator -> allocaArray ((Data.Vector.length createInfos)) (\pPipelines -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> withVec withCStructComputePipelineCreateInfo createInfos (\pCreateInfos -> Graphics.Vulkan.C.Dynamic.createComputePipelines commandTable device pipelineCache (fromIntegral $ Data.Vector.length createInfos) pCreateInfos pAllocator pPipelines >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((Data.Vector.generateM ((Data.Vector.length createInfos)) (peekElemOff pPipelines)))))))

-- | Wrapper for 'vkCreateGraphicsPipelines'
createGraphicsPipelines :: Device ->  PipelineCache ->  Vector GraphicsPipelineCreateInfo ->  Maybe AllocationCallbacks ->  IO (Vector Pipeline)
createGraphicsPipelines = \(Device device commandTable) -> \pipelineCache -> \createInfos -> \allocator -> allocaArray ((Data.Vector.length createInfos)) (\pPipelines -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> withVec withCStructGraphicsPipelineCreateInfo createInfos (\pCreateInfos -> Graphics.Vulkan.C.Dynamic.createGraphicsPipelines commandTable device pipelineCache (fromIntegral $ Data.Vector.length createInfos) pCreateInfos pAllocator pPipelines >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((Data.Vector.generateM ((Data.Vector.length createInfos)) (peekElemOff pPipelines)))))))

-- | Wrapper for 'vkDestroyPipeline'
destroyPipeline :: Device ->  Pipeline ->  Maybe AllocationCallbacks ->  IO ()
destroyPipeline = \(Device device commandTable) -> \pipeline -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyPipeline commandTable device pipeline pAllocator *> (pure ()))
-- | Wrapper for 'createComputePipelines' and 'destroyPipeline' using 'bracket'
withComputePipelines
  :: Device -> PipelineCache -> Vector (ComputePipelineCreateInfo) -> Maybe (AllocationCallbacks) -> (Vector (Pipeline) -> IO a) -> IO a
withComputePipelines device pipelineCache computePipelineCreateInfo allocationCallbacks = bracket
  (createComputePipelines device pipelineCache computePipelineCreateInfo allocationCallbacks)
  (traverse (\o -> destroyPipeline device o allocationCallbacks))
-- | Wrapper for 'createGraphicsPipelines' and 'destroyPipeline' using 'bracket'
withGraphicsPipelines
  :: Device -> PipelineCache -> Vector (GraphicsPipelineCreateInfo) -> Maybe (AllocationCallbacks) -> (Vector (Pipeline) -> IO a) -> IO a
withGraphicsPipelines device pipelineCache graphicsPipelineCreateInfo allocationCallbacks = bracket
  (createGraphicsPipelines device pipelineCache graphicsPipelineCreateInfo allocationCallbacks)
  (traverse (\o -> destroyPipeline device o allocationCallbacks))
