module VK.Bracket
  ( brackets
  ) where

import           Data.List                      ( (\\) )
import qualified Data.Map                      as Map
import qualified Data.Text.Extra               as T
import           Data.Vector                    ( Vector )
import           Polysemy
import           Relude                  hiding ( Handle
                                                , Type
                                                )

import           Bracket
import           CType
import           Error
import           Marshal.Command
import           Marshal.Scheme
import           Render.Element
import           Render.Names
import           Render.SpecInfo
import           Render.Utils
import           Spec.Parse

brackets
  :: forall r
   . (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderedNames r)
  => Vector MarshaledCommand
  -> Vector Handle
  -> Sem r (Vector (CName, CName, RenderElement))
  -- ^ (Creating command, Bracket command, RenderElem)
brackets marshaledCommands handles = context "brackets" $ do
  let getMarshaledCommand =
        let mcMap = Map.fromList
              [ (mcName, m)
              | m@MarshaledCommand {..} <- toList marshaledCommands
              ]
        in  \n ->
              note ("Unable to find marshaled command " <> show n)
                . (`Map.lookup` mcMap)
                $ n
      autoBracket' bracketType create destroy with = do
        create'  <- getMarshaledCommand create
        destroy' <- getMarshaledCommand destroy
        autoBracket bracketType create' destroy' with
      cdBracket h = autoBracket' BracketCPS
                                 (CName ("vkCreate" <> h))
                                 (CName ("vkDestroy" <> h))
                                 (CName ("vkWith" <> h))
      afBracket h = autoBracket' BracketCPS
                                 (CName ("vkAllocate" <> h))
                                 (CName ("vkFree" <> h))
                                 (CName ("vkWith" <> h))
      cmdBeBracket h = autoBracket' BracketBookend
                                    (CName ("vkCmdBegin" <> h))
                                    (CName ("vkCmdEnd" <> h))
                                    (CName ("vkCmdUse" <> h))

  -- TODO: Missing functions here should be warnings, because we might be
  -- generating a different version of the spec.
  bs <- sequenceV $
    [ cdBracket "Instance"
    , cdBracket "Device"
    , cdBracket "CommandPool"
    , cdBracket "Buffer"
    , cdBracket "BufferView"
    , cdBracket "Image"
    , cdBracket "ImageView"
    , cdBracket "ShaderModule"
    , cdBracket "PipelineLayout"
    , cdBracket "Sampler"
    , cdBracket "DescriptorSetLayout"
    , cdBracket "DescriptorPool"
    , cdBracket "Fence"
    , cdBracket "Semaphore"
    , cdBracket "Event"
    , cdBracket "QueryPool"
    , cdBracket "Framebuffer"
    , cdBracket "RenderPass"
    , cdBracket "PipelineCache"
    , cdBracket "IndirectCommandsLayoutNV"
    , cdBracket "DescriptorUpdateTemplate"
    , cdBracket "SamplerYcbcrConversion"
    , cdBracket "ValidationCacheEXT"
    , cdBracket "AccelerationStructureKHR"
    , cdBracket "AccelerationStructureNV"
      --  ^ TODO: remove when generating from a pre 1.2.162 spec
    , cdBracket "SwapchainKHR"
    , cdBracket "DebugReportCallbackEXT"
    , cdBracket "DebugUtilsMessengerEXT"
    , cdBracket "DeferredOperationKHR"
    , cdBracket "PrivateDataSlot"
    -- , cdBracket "VideoSessionKHR"
    -- , cdBracket "VideoSessionParametersKHR"
    , cdBracket "CuModuleNVX"
    , cdBracket "CuFunctionNVX"
    , cdBracket "BufferCollectionFUCHSIA"
    , cdBracket "OpticalFlowSessionNV"
    , cdBracket "MicromapEXT"
    , pure withCommmandBuffers
    , afBracket "Memory"
    , pure withDescriptorSets
    , autoBracket' BracketCPS
                   "vkCreateGraphicsPipelines"
                   "vkDestroyPipeline"
                   "vkWithGraphicsPipelines"
    , autoBracket' BracketCPS
                   "vkCreateComputePipelines"
                   "vkDestroyPipeline"
                   "vkWithComputePipelines"
    , autoBracket' BracketCPS
                   "vkCreateRayTracingPipelinesKHR"
                   "vkDestroyPipeline"
                   "vkWithRayTracingPipelinesKHR"
    , autoBracket' BracketCPS
                   "vkCreateRayTracingPipelinesNV"
                   "vkDestroyPipeline"
                   "vkWithRayTracingPipelinesNV"
    , autoBracket' BracketCPS "vkMapMemory" "vkUnmapMemory" "vkWithMappedMemory"
    , autoBracket' BracketBookend
                   "vkBeginCommandBuffer"
                   "vkEndCommandBuffer"
                   "vkUseCommandBuffer"
    , cmdBeBracket "Query"
    , cmdBeBracket "ConditionalRenderingEXT"
    , cmdBeBracket "RenderPass"
    , cmdBeBracket "DebugUtilsLabelEXT"
    , cmdBeBracket "RenderPass2"
    , cmdBeBracket "TransformFeedbackEXT"
    , cmdBeBracket "QueryIndexedEXT"
    , cmdBeBracket "Rendering"
    , autoBracket' BracketCPS
                   "vkCreateShadersEXT"
                   "vkDestroyShaderEXT"
                   "vkWithShadersEXT"
    -- , cdBracket "SemaphoreSciSyncPoolNV"
    ] <>
    [ cdBracket "CudaModuleNV"
    | "VkCudaModuleNV" `elem` (hName <$> handles)
    ] <>
    [ cdBracket "CudaFunctionNV"
    | "VkCudaFunctionNV" `elem` (hName <$> handles)
    ]

  --
  -- Check that we can generate all the handles
  --
  let ignoredHandles =
        [ "VkPhysicalDevice"
        , "VkQueue"
        , "VkDisplayKHR"
        , "VkDisplayModeKHR"
        , "VkSurfaceKHR"
        , "VkPerformanceConfigurationINTEL"
        ]
      handleNames = hName <$> handles
      -- A crude way of getting all the type names we generate
      createdBracketNames =
        [ n
        | Bracket {..} <- bs
        , TypeName n   <-
          [ t | Normal t <- bInnerTypes ]
          <> [ t | Vector _ (Normal t) <- bInnerTypes ]
        ]
      unhandledHandles =
        toList handleNames \\ (createdBracketNames ++ ignoredHandles)
  unless (null unhandledHandles)
    $ throw ("Unbracketed handles: " <> show unhandledHandles)

  fromList <$> traverseV (renderBracket paramName) bs

withCommmandBuffers :: Bracket
withCommmandBuffers = Bracket
  { bInnerTypes = [Vector NotNullable (Normal (TypeName "VkCommandBuffer"))]
  , bWrapperName         = "vkWithCommandBuffers"
  , bCreate              = "vkAllocateCommandBuffers"
  , bDestroy             = "vkFreeCommandBuffers"
  , bCreateArguments     =
    [ Provided "device"        (Normal (TypeName "VkDevice"))
    , Provided "pAllocateInfo" (Normal (TypeName "VkCommandBufferAllocateInfo"))
    ]
  , bDestroyArguments    = [ Provided "device" (Normal (TypeName "VkDevice"))
                           , Member "pAllocateInfo" "commandPool"
                           , Resource IdentityResource 0
                           ]
  , bDestroyIndividually = DoNotDestroyIndividually
  , bBracketType         = BracketCPS
  , bDestroyReturnTypes  = []
  }

withDescriptorSets :: Bracket
withDescriptorSets = Bracket
  { bInnerTypes = [Vector NotNullable (Normal (TypeName "VkDescriptorSet"))]
  , bWrapperName         = "vkWithDescriptorSets"
  , bCreate              = "vkAllocateDescriptorSets"
  , bDestroy             = "vkFreeDescriptorSets"
  , bCreateArguments     =
    [ Provided "device"        (Normal (TypeName "VkDevice"))
    , Provided "pAllocateInfo" (Normal (TypeName "VkDescriptorSetAllocateInfo"))
    ]
  , bDestroyArguments    = [ Provided "device" (Normal (TypeName "VkDevice"))
                           , Member "pAllocateInfo" "descriptorPool"
                           , Resource IdentityResource 0
                           ]
  , bDestroyIndividually = DoNotDestroyIndividually
  , bBracketType         = BracketCPS
  , bDestroyReturnTypes  = []
  }

dropVk :: Text -> Text
dropVk t = if "vk" `T.isPrefixOf` T.toLower t
  then T.dropWhile (== '_') . T.drop 2 $ t
  else t

paramName :: Text -> Text
paramName = unReservedWord . T.lowerCaseFirst . dropVk
