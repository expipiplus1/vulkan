module VK.Bracket
  ( brackets
  ) where

import           Relude                  hiding ( Handle
                                                , Type
                                                )
import           Data.List.Extra                ( (\\) )
import qualified Data.Text.Extra               as T
import           Data.Char                      ( isUpper )
import           Polysemy
import           Data.Vector                    ( Vector )

import           Render.Element
import           Render.Utils
import           Render.SpecInfo
import           Render.Names
import           Spec.Parse
import           Error
import           CType
import           Bracket

brackets
  :: (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderedNames r)
  => Vector Handle
  -> Sem r (Vector (CName, CName, RenderElement))
  -- ^ (Creating command, Bracket command, RenderElem)
brackets handles = do
  autoBrackets <- sequenceV
    [ autoBracketBeginEndWith "vkCmdBeginQuery"
    -- , autoBracketBeginEndWith "vkCmdBeginConditionalRenderingEXT"
    -- , autoBracketBeginEndWith "vkCmdBeginRenderPass"
    -- , autoBracketBeginEndWith "vkCmdBeginDebugUtilsLabelEXT"
    -- , autoBracketBeginEndWith "vkCmdBeginRenderPass2"
    -- , autoBracketBeginEndWith "vkCmdBeginTransformFeedbackEXT"
    , autoBracketBeginEndWith "vkCmdBeginQueryIndexedEXT"
    ]
  rs <-
    traverseV writePair
    $  [ simpleBracket True  "Instance"                  Nothing
       , simpleBracket False "Device" (Just "VkPhysicalDevice")
       , simpleBracket True  "CommandPool"               (Just "VkDevice")
       , simpleBracket True  "Buffer"                    (Just "VkDevice")
       , simpleBracket True  "BufferView"                (Just "VkDevice")
       , simpleBracket True  "Image"                     (Just "VkDevice")
       , simpleBracket True  "ImageView"                 (Just "VkDevice")
       , simpleBracket True  "ShaderModule"              (Just "VkDevice")
       , simpleBracket True  "PipelineLayout"            (Just "VkDevice")
       , simpleBracket True  "Sampler"                   (Just "VkDevice")
       , simpleBracket True  "DescriptorSetLayout"       (Just "VkDevice")
       , simpleBracket True  "DescriptorPool"            (Just "VkDevice")
       , simpleBracket True  "Fence"                     (Just "VkDevice")
       , simpleBracket True  "Semaphore"                 (Just "VkDevice")
       , simpleBracket True  "Event"                     (Just "VkDevice")
       , simpleBracket True  "QueryPool"                 (Just "VkDevice")
       , simpleBracket True  "Framebuffer"               (Just "VkDevice")
       , simpleBracket True  "RenderPass"                (Just "VkDevice")
       , simpleBracket True  "PipelineCache"             (Just "VkDevice")
       , simpleBracket True  "ObjectTableNVX"            (Just "VkDevice")
       , simpleBracket True  "IndirectCommandsLayoutNVX" (Just "VkDevice")
       , simpleBracket True  "DescriptorUpdateTemplate"  (Just "VkDevice")
       , simpleBracket True  "SamplerYcbcrConversion"    (Just "VkDevice")
       , simpleBracket True  "ValidationCacheEXT"        (Just "VkDevice")
       , simpleBracket True  "AccelerationStructureNV"   (Just "VkDevice")
       , simpleBracket True  "SwapchainKHR"              (Just "VkDevice")
       , simpleBracket True  "DebugReportCallbackEXT"    (Just "VkInstance")
       , simpleBracket True  "DebugUtilsMessengerEXT"    (Just "VkInstance")
       , allocateBracket True False (Just "commandPool") "CommandBuffer" Nothing
       , allocateBracket False True Nothing "DeviceMemory" (Just "Memory")
       , allocateBracket True
                         False
                         (Just "descriptorPool")
                         "DescriptorSet"
                         Nothing
       , createPipeline "Compute"
       , createPipeline "Graphics"
       , mapMemory
       , useCommandBuffer
       , registerObjectsNVX
       ]
    <> autoBrackets
  let ignoredHandles =
        [ "VkPhysicalDevice"
        , "VkQueue"
        , "VkDisplayKHR"
        , "VkDisplayModeKHR"
        , "VkSurfaceKHR"
        , "VkPerformanceConfigurationINTEL"
        ]
      handleNames      = hName <$> handles
      bracketNames     = [ n | (TypeName n, _, _, _) <- rs ]
      unhandledHandles = toList handleNames \\ (bracketNames ++ ignoredHandles)
  unless (null unhandledHandles)
    $ throw ("Unbracketed handles: " <> show unhandledHandles)
  pure $ fromList [ (c, b, w) | (_, c, b, w) <- rs ]

-- | A bracket consuming a "CreateInfo" and "AllocationCallbacks"
simpleBracket
  :: Bool
  -> Text
  -- ^ Type name with no Vk Prefix
  -> Maybe CName
  -> Bracket
simpleBracket passDestructorParent innerType parentMaybe =
  let parentArg =
        [ Provided (Single (TypeName parent))
                   (paramName parent)
        | Just parent <- pure parentMaybe
        ]
  in  Bracket
        (SingleTypeName ("Vk" <> innerType))
        (CName $ "vkWith" <> innerType)
        (CName $ "vkCreate" <> innerType)
        (CName $ "vkDestroy" <> innerType)
        (  parentArg
        ++ [ Provided
             (SingleTypeName
               ("Vk" <> innerType `appendWithVendor` "CreateInfo")
             )
             (T.lowerCaseFirst innerType `appendWithVendor` "CreateInfo")
           , Provided (Optional (TypeName "VkAllocationCallbacks"))
                      "allocationCallbacks"
           ]
        )
        (  [ a | passDestructorParent, a <- parentArg ]
        ++ [ Resource
           , Provided (Optional (TypeName "VkAllocationCallbacks"))
                      "allocationCallbacks"
           ]
        )
        False

allocateBracket :: Bool -> Bool -> Maybe Text -> Text -> Maybe Text -> Bracket
allocateBracket plural useAllocationCallbacks poolMaybe innerTypeName functionNameFragment
  = let suffix    = bool "" "s" plural
        parent    = "VkDevice"
        innerType = fromMaybe innerTypeName functionNameFragment
        allocateInfoTerm =
          (T.lowerCaseFirst innerType `appendWithVendor` "AllocateInfo")
    in  Bracket
          (bool Single
                Multiple
                plural
                (TypeName (CName ("Vk" <> innerTypeName)))
          )
          (CName $ "vkWith" <> innerType <> suffix)
          (CName $ "vkAllocate" <> innerType <> suffix)
          (CName $ "vkFree" <> innerType <> suffix)
          (  [ Provided (Single (TypeName parent)) (paramName parent)
             , Provided
               (SingleTypeName
                 ("Vk" <> innerType `appendWithVendor` "AllocateInfo")
               )
               allocateInfoTerm
             ]
          ++ [ Provided (Optional (TypeName "VkAllocationCallbacks"))
                        "allocationCallbacks"
             | useAllocationCallbacks
             ]
          )
          (  [Provided (Single (TypeName parent)) (paramName parent)]
          ++ maybeToList ((`Member` allocateInfoTerm) <$> poolMaybe)
          ++ [Resource]
          ++ [ Provided (Optional (TypeName "VkAllocationCallbacks"))
                        "allocationCallbacks"
             | useAllocationCallbacks
             ]
          )
          False

createPipeline :: Text -> Bracket
createPipeline createTypePrefix =
  let
    innerType = "Pipeline"
    suffix    = "s"
    parent    = "VkDevice"
    ciType =
      CName
        $                  "Vk"
        <>                 createTypePrefix
        <>                 innerType
        `appendWithVendor` "CreateInfo"
    cacheType = CName $ "Vk" <> innerType `appendWithVendor` "Cache"
  in
    Bracket
      (Multiple (TypeName (CName $ "Vk" <> innerType)))
      (CName $ "vkWith" <> createTypePrefix <> innerType <> suffix)
      (CName $ "vkCreate" <> createTypePrefix <> innerType <> suffix)
      (CName $ "vkDestroy" <> innerType)
      [ Provided (Single (TypeName parent))    (paramName parent)
      , Provided (Single (TypeName cacheType)) (paramName cacheType)
      , Provided (Multiple (TypeName ciType))  (paramName ciType)
      , Provided (Optional (TypeName "VkAllocationCallbacks"))
                 "allocationCallbacks"
      ]
      [ Provided (Single (TypeName parent)) (paramName parent)
      , Resource
      , Provided (Optional (TypeName "VkAllocationCallbacks"))
                 "allocationCallbacks"
      ]
      True

mapMemory :: Bracket
mapMemory =
  let parent = "VkDevice"
      mem    = "VkDeviceMemory"
  in  Bracket
        (Single (Ptr NonConst Void))
        "vkWithMappedMemory"
        "vkMapMemory"
        "vkUnmapMemory"
        [ Provided (Single (TypeName parent))             (paramName parent)
        , Provided (Single (TypeName mem))                (paramName mem)
        , Provided (Single (TypeName "VkDeviceSize"))     "offset'"
        , Provided (Single (TypeName "VkDeviceSize"))     "size'"
        , Provided (Single (TypeName "VkMemoryMapFlags")) "flags'"
        ]
        [ Provided (Single (TypeName parent)) (paramName parent)
        , Provided (Single (TypeName mem))    (paramName mem)
        ]
        False

useCommandBuffer :: Bracket
useCommandBuffer =
  let buf       = "VkCommandBuffer"
      beginInfo = "VkCommandBufferBeginInfo"
  in  Bracket
        (Single Void)
        "vkUseCommandBuffer"
        "vkBeginCommandBuffer"
        "vkEndCommandBuffer"
        [ Provided (Single (TypeName buf))       (paramName buf)
        , Provided (Single (TypeName beginInfo)) (paramName beginInfo)
        ]
        [Provided (Single (TypeName buf)) (paramName buf)]
        False

registerObjectsNVX :: Bracket
registerObjectsNVX =
  let parent     = "VkDevice"
      table      = "VkObjectTableNVX"
      tableEntry = "VkObjectTableEntryNVX"
      entryType  = "VkObjectEntryTypeNVX"
  in  Bracket
        (Single Void)
        "vkWithRegisteredObjectsNVX"
        "vkRegisterObjectsNVX"
        "vkUnregisterObjectsNVX"
        [ Provided (Single (TypeName parent))       (paramName parent)
        , Provided (Single (TypeName table))        (paramName table)
        , Provided (Multiple (TypeName tableEntry)) (paramName tableEntry)
        , Provided (Multiple (TypeName "uint32_t")) "objectIndices"
        ]
        [ Provided (Single (TypeName parent))       (paramName parent)
        , Provided (Single (TypeName table))        (paramName table)
        , Provided (Multiple (TypeName entryType))  (paramName entryType)
        , Provided (Multiple (TypeName "uint32_t")) "objectIndices"
        ]
        False

appendWithVendor :: Text -> Text -> Text
appendWithVendor a b =
  let prefix = T.dropWhileEnd isUpper a
      vendor = T.takeWhileEnd isUpper a
  in  prefix <> b <> vendor

dropVk :: CName -> Text
dropVk (CName t) = if "vk" `T.isPrefixOf` T.toLower t
  then T.dropWhile (== '_') . T.drop 2 $ t
  else t

paramName :: CName -> Text
paramName = unReservedWord . T.lowerCaseFirst . dropVk
