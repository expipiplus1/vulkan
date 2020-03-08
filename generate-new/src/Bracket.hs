module Bracket
  where

import           Relude                  hiding ( Handle
                                                , Type
                                                )
import           Data.List.Extra                ( nubOrd )
import qualified Data.Text.Extra               as T
import           Data.Char                      ( isUpper )
import           Data.Text.Prettyprint.Doc
                                         hiding ( brackets
                                                , plural
                                                )
import           Polysemy
import           Data.Vector                    ( Vector )

import qualified Control.Exception

import           Render.Element
import           Render.Type
import           Render.Utils
import           Spec.Parse
import           Haskell                       as H
import           Error
import           CType

brackets
  :: (HasErr r, HasRenderParams r)
  => Vector Handle
  -> Sem r (Vector (Text, Text, RenderElement))
  -- ^ (Creating command, Bracket command, RenderElem)
brackets handles = do
  rs <- traverseV
    writePair
    [ simpleBracket True  "Instance"                  Nothing
    , simpleBracket False "Device"                    (Just "VkPhysicalDevice")
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
    , allocateBracket True False (Just "descriptorPool") "DescriptorSet" Nothing
    , createPipeline "Compute"
    , createPipeline "Graphics"
    , mapMemory
    -- , useCommandBuffer
    -- , registerObjectsNVX
    ]
  let ignoredHandles =
        [ "PhysicalDevice"
        , "Queue"
        , "DisplayKHR"
        , "DisplayModeKHR"
        , "SurfaceKHR"
        , "PerformanceConfigurationINTEL"
        ]
      handleNames = hName <$> handles
      -- bracketNames     = [ n | (TypeName n, _, _, _) <- rs ]
      -- unhandledHandles = handleNames \\ (bracketNames ++ ignoredHandles)
  -- unless (null unhandledHandles)
    -- $ throwError ("Unbracketed handles: " <> T.tShow unhandledHandles)
  pure $ fromList [ (c, b, w) | (_, c, b, w) <- rs ]

data Bracket = Bracket
  { bInnerType :: ConstructedType
  , bWrapperName :: Text
  , bCreate :: Text
  , bDestroy :: Text
  , bCreateArguments :: [Argument]
  , bDestroyArguments :: [Argument]
  , bDestroyIndividually :: Bool
  }

data ConstructedType
  = Single { unConstructedType :: CType }
  | Optional { unConstructedType :: CType }
  | Multiple { unConstructedType :: CType }
  deriving (Eq, Ord)

renderConstructedType
  :: (HasErr r, HasRenderElem r, HasRenderParams r)
  => ConstructedType
  -> Sem r (Doc ())
renderConstructedType = \case
  Single   t -> renderType =<< cToHsType DoNotPreserve t
  Optional t -> do
    tDoc <- renderTypeHighPrec =<< cToHsType DoNotPreserve t
    pure ("Maybe" <+> tDoc)
  Multiple t -> do
    tellImport ''Vector
    tDoc <- renderTypeHighPrec =<< cToHsType DoNotPreserve t
    pure ("Vector" <+> tDoc)

data Argument
  = Provided ConstructedType Text
  | Resource
  | Member Text Text
  deriving (Eq, Ord)

-- | A bracket consuming a "CreateInfo" and "AllocationCallbacks"
simpleBracket :: Bool -> Text -> Maybe Text -> Bracket
simpleBracket passDestructorParent innerType parentMaybe =
  let parentArg =
        [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
        | Just parent <- [parentMaybe]
        ]
  in
    Bracket
      (Single (TypeName ("Vk" <> innerType)))
      ("vkWith" <> innerType)
      ("vkCreate" <> innerType)
      ("vkDestroy" <> innerType)
      (  parentArg
      ++ [ Provided
           (Single (TypeName ("Vk" <> innerType `appendWithVendor` "CreateInfo")))
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
  = let
      suffix    = bool "" "s" plural
      parent    = "VkDevice"
      innerType = fromMaybe innerTypeName functionNameFragment
      allocateInfoTerm =
        (T.lowerCaseFirst innerType `appendWithVendor` "AllocateInfo")
    in
      Bracket
        (bool Single Multiple plural (TypeName ("Vk" <> innerTypeName)))
        ("vkWith" <> innerType <> suffix)
        ("vkAllocate" <> innerType <> suffix)
        ("vkFree" <> innerType <> suffix)
        (  [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
           , Provided
             (Single (TypeName ("Vk" <> innerType `appendWithVendor` "AllocateInfo")))
             allocateInfoTerm
           ]
        ++ [ Provided (Optional (TypeName "VkAllocationCallbacks"))
                      "allocationCallbacks"
           | useAllocationCallbacks
           ]
        )
        (  [Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)]
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
      ("Vk" <> createTypePrefix <> innerType `appendWithVendor` "CreateInfo")
    cacheType = ("Vk" <> innerType `appendWithVendor` "Cache")
  in
    Bracket
      (Multiple (TypeName ("Vk" <> innerType)))
      ("vkWith" <> createTypePrefix <> innerType <> suffix)
      ("vkCreate" <> createTypePrefix <> innerType <> suffix)
      ("vkDestroy" <> innerType)
      [ Provided (Single (TypeName parent))    (T.lowerCaseFirst parent)
      , Provided (Single (TypeName cacheType)) (T.lowerCaseFirst cacheType)
      , Provided (Multiple (TypeName ciType))  (T.lowerCaseFirst ciType)
      , Provided (Optional (TypeName "VkAllocationCallbacks"))
                 "allocationCallbacks"
      ]
      [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
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
        [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
        , Provided (Single (TypeName mem))                (T.lowerCaseFirst mem)
        , Provided (Single (TypeName "VkDeviceSize"))     "offset'"
        , Provided (Single (TypeName "VkDeviceSize"))     "size'"
        , Provided (Single (TypeName "VkMemoryMapFlags")) "flags'"
        ]
        [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
        , Provided (Single (TypeName mem))    (T.lowerCaseFirst mem)
        ]
        False

useCommandBuffer :: Bracket
useCommandBuffer =
  let buf       = "CommandBuffer"
      beginInfo = "CommandBufferBeginInfo"
  in  Bracket
        (Single Void)
        "vkUseCommandBuffer"
        "vkBeginCommandBuffer"
        "vkEndCommandBuffer"
        [ Provided (Single (TypeName buf))       (T.lowerCaseFirst buf)
        , Provided (Single (TypeName beginInfo)) (T.lowerCaseFirst beginInfo)
        ]
        [Provided (Single (TypeName buf)) (T.lowerCaseFirst buf)]
        False

registerObjectsNVX :: Bracket
registerObjectsNVX =
  let parent     = "Device"
      table      = "ObjectTableNVX"
      tableEntry = "ObjectTableEntryNVX"
      entryType  = "ObjectEntryTypeNVX"
  in  Bracket
        (Single Void)
        "vkWithRegisteredObjectsNVX"
        "vkRegisterObjectsNVX"
        "vkUnregisterObjectsNVX"
        [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
        , Provided (Single (TypeName table))  (T.lowerCaseFirst table)
        , Provided (Multiple (TypeName tableEntry))
                   (T.lowerCaseFirst tableEntry)
        , Provided (Multiple (TypeName "uint32_t")) "objectIndices"
        ]
        [ Provided (Single (TypeName parent))       (T.lowerCaseFirst parent)
        , Provided (Single (TypeName table))        (T.lowerCaseFirst table)
        , Provided (Multiple (TypeName entryType))  (T.lowerCaseFirst entryType)
        , Provided (Multiple (TypeName "uint32_t")) "objectIndices"
        ]
        False

writePair
  :: (HasErr r, HasRenderParams r)
  => Bracket
  -> Sem r (CType, Text, Text, RenderElement)
writePair Bracket {..} =
  let arguments = nubOrd (bCreateArguments ++ bDestroyArguments)
  in
    fmap (unConstructedType bInnerType, bCreate, bWrapperName, )
    . genRe ("bracket " <> bWrapperName)
    $ do
        tellExport (ETerm bWrapperName)
        tellImport (TermName bCreate)
        tellImport (TermName bDestroy)
        argHsTypes <- traverseV renderConstructedType
                                [ t | Provided t _ <- arguments ]
        let argHsVars = [ pretty v | Provided _ v <- arguments ]
        createArgVars <- forV bCreateArguments $ \case
          Provided _ v -> pure (pretty v)
          Resource     -> throw "Resource used in its own construction"
          Member _ _   -> throw "Member used during construction"
        destroyArgVars <- forV bDestroyArguments $ \case
          Provided _ v -> pure $ pretty v
          Resource     -> pure "o"
          Member member argument
            | [t] <- [ t | Provided (Single t) v <- arguments, v == argument ] -> do
              argTyDoc <- renderType =<< cToHsType DoNotPreserve t
              pure
                $ parens
                    (   pretty member
                    <+> parens (pretty argument <+> "::" <+> argTyDoc)
                    )
            | otherwise -> throw "Can't find single argument for member"
        innerHsType <- renderConstructedType bInnerType
        let
          noDestructorResource = Resource `notElem` bDestroyArguments
          noResource = bInnerType == Single Void && noDestructorResource
          bracketDoc = if noResource then "bracket_" else "bracket"
          cont =
            if noResource then "IO a" else "(" <> innerHsType <> " -> IO a)"
          wrapperArguments = punctuate " ->" (argHsTypes ++ [cont, "IO a"])
          resourcePattern  = if noDestructorResource then "_" else "o"
          callDestructor =
            (if noResource then emptyDoc else "\\" <> resourcePattern <+> "->")
              <+> pretty bDestroy
              <+> hsep destroyArgVars
        tellImport 'Control.Exception.bracket
        tellDoc $ vsep
          [ comment
            (T.unlines
              [ "A safe wrapper for @"
              <> bCreate
              <> "@ and @"
              <> bDestroy
              <> "@ using @"
              <> bracketDoc
              <> "@"
              , ""
              , "The allocated value must not be returned from the provided computation"
              ]
            )
          , pretty bWrapperName <+> "::" <+> sep wrapperArguments
          , pretty bWrapperName <+> sep argHsVars <+> "=" <> line <> indent
            2
            (pretty bracketDoc <> line <> indent
              2
              (vsep
                [ parens (pretty bCreate <+> sep createArgVars)
                , bool mempty "(traverse " bDestroyIndividually
                <> parens callDestructor
                <> bool mempty ")" bDestroyIndividually
                ]
              )
            )
          ]

appendWithVendor :: Text -> Text -> Text
appendWithVendor a b =
  let prefix = T.dropWhileEnd isUpper a
      vendor = T.takeWhileEnd isUpper a
  in  prefix <> b <> vendor
