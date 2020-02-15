{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Bracket
  ( brackets
  ) where

import           Control.Monad
import           Data.Bool
import           Control.Monad.Except
import           Data.Foldable
import           Data.List.Extra                          ( (\\)
                                                          , nubOrd
                                                          )
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc         hiding ( brackets , plural)
import           Data.Traversable
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Handle
import           Spec.Savvy.Type

import           Write.Element                     hiding ( TypeName )
import           Write.Monad
import           Write.Marshal.Util

brackets
  :: [Handle] -> Write [((HaskellName, HaskellName), WriteElement)]
  -- ^ ((Creating command, Bracket command), WriteElement)
brackets handles = do
  rs <- traverseV
    writePair
    [ simpleBracket True  "Instance"                  Nothing
    , simpleBracket False "Device"                    (Just "PhysicalDevice")
    , simpleBracket True  "CommandPool"               (Just "Device")
    , simpleBracket True  "Buffer"                    (Just "Device")
    , simpleBracket True  "BufferView"                (Just "Device")
    , simpleBracket True  "Image"                     (Just "Device")
    , simpleBracket True  "ImageView"                 (Just "Device")
    , simpleBracket True  "ShaderModule"              (Just "Device")
    , simpleBracket True  "PipelineLayout"            (Just "Device")
    , simpleBracket True  "Sampler"                   (Just "Device")
    , simpleBracket True  "DescriptorSetLayout"       (Just "Device")
    , simpleBracket True  "DescriptorPool"            (Just "Device")
    , simpleBracket True  "Fence"                     (Just "Device")
    , simpleBracket True  "Semaphore"                 (Just "Device")
    , simpleBracket True  "Event"                     (Just "Device")
    , simpleBracket True  "QueryPool"                 (Just "Device")
    , simpleBracket True  "Framebuffer"               (Just "Device")
    , simpleBracket True  "RenderPass"                (Just "Device")
    , simpleBracket True  "PipelineCache"             (Just "Device")
    , simpleBracket True  "ObjectTableNVX"            (Just "Device")
    , simpleBracket True  "IndirectCommandsLayoutNVX" (Just "Device")
    , simpleBracket True  "DescriptorUpdateTemplate"  (Just "Device")
    , simpleBracket True  "SamplerYcbcrConversion"    (Just "Device")
    , simpleBracket True  "ValidationCacheEXT"        (Just "Device")
    , simpleBracket True  "AccelerationStructureNV"   (Just "Device")
    , simpleBracket True  "SwapchainKHR"              (Just "Device")
    , simpleBracket True  "DebugReportCallbackEXT"    (Just "Instance")
    , simpleBracket True  "DebugUtilsMessengerEXT"    (Just "Instance")
    , allocateBracket True False  (Just "commandPool") "CommandBuffer" Nothing
    , allocateBracket False True Nothing "DeviceMemory" (Just "Memory")
    , allocateBracket True False (Just "descriptorPool") "DescriptorSet" Nothing
    , createPipeline "Compute"
    , createPipeline "Graphics"
    , mapMemory
    , useCommandBuffer
    , registerObjectsNVX
    ]
  let ignoredHandles   = ["PhysicalDevice", "Queue", "DisplayKHR", "DisplayModeKHR", "SurfaceKHR", "PerformanceConfigurationINTEL"]
      handleNames      = dropVkType . hName <$> handles
      bracketNames     = [ n | (TypeName n, _, _, _) <- rs ]
      unhandledHandles = handleNames \\ (bracketNames ++ ignoredHandles)
  unless (null unhandledHandles)
    $ throwError ("Unbracketed handles: " <> T.tShow unhandledHandles)
  pure [ ((c, b), w) | (_, c, b, w) <- rs ]

simpleBracket :: Bool -> Text -> Maybe Text -> Bracket
simpleBracket passDestructorParent innerType parentMaybe =
  let parentArg =
          [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
          | Just parent <- [parentMaybe]
          ]
  in  Bracket
        (Single (TypeName innerType))
        (TermName ("with" <> innerType))
        (TermName ("create" <> innerType))
        (TermName ("destroy" <> innerType))
        (  parentArg
        ++ [ Provided
             (Single (TypeName (innerType `appendWithVendor` "CreateInfo")))
             (T.lowerCaseFirst innerType `appendWithVendor` "CreateInfo")
           , Provided (Optional (TypeName "AllocationCallbacks"))
                      "allocationCallbacks"
           ]
        )
        (  [ a | passDestructorParent, a <- parentArg ]
        ++ [ Resource
           , Provided (Optional (TypeName "AllocationCallbacks"))
                      "allocationCallbacks"
           ]
        )
        False

allocateBracket :: Bool -> Bool -> Maybe Text -> Text -> Maybe Text -> Bracket
allocateBracket plural useAllocationCallbacks poolMaybe innerTypeName functionNameFragment
  = let
      suffix = bool "" "s" plural
      parent = "Device"
      innerType = fromMaybe innerTypeName functionNameFragment
      allocateInfoTerm =
        (T.lowerCaseFirst innerType `appendWithVendor` "AllocateInfo")
    in
      Bracket
        (bool Single Multiple plural (TypeName innerTypeName))
        (TermName ("with" <> innerType <> suffix))
        (TermName ("allocate" <> innerType <> suffix))
        (TermName ("free" <> innerType <> suffix))
        (  [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
           , Provided
             (Single (TypeName (innerType `appendWithVendor` "AllocateInfo")))
             allocateInfoTerm
           ]
        ++ [ Provided (Optional (TypeName "AllocationCallbacks"))
                      "allocationCallbacks"
           | useAllocationCallbacks
           ]
        )
        (  [Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)]
        ++ maybeToList ((`Member` allocateInfoTerm) <$> poolMaybe)
        ++ [Resource]
        ++ [ Provided (Optional (TypeName "AllocationCallbacks"))
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
    parent    = "Device"
    ciType    = (createTypePrefix <> innerType `appendWithVendor` "CreateInfo")
    cacheType = (innerType `appendWithVendor` "Cache")
  in
    Bracket
      (Multiple (TypeName innerType))
      (TermName ("with" <> createTypePrefix <> innerType <> suffix))
      (TermName ("create" <> createTypePrefix <> innerType <> suffix))
      (TermName ("destroy" <> innerType))
      [ Provided (Single (TypeName parent))    (T.lowerCaseFirst parent)
      , Provided (Single (TypeName cacheType)) (T.lowerCaseFirst cacheType)
      , Provided (Multiple (TypeName ciType))  (T.lowerCaseFirst ciType)
      , Provided (Optional (TypeName "AllocationCallbacks"))
                 "allocationCallbacks"
      ]
      [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
      , Resource
      , Provided (Optional (TypeName "AllocationCallbacks"))
                 "allocationCallbacks"
      ]
      True

mapMemory :: Bracket
mapMemory =
  let parent = "Device"
      mem    = "DeviceMemory"
  in  Bracket
        (Single (Ptr NonConst Void))
        (TermName "withMappedMemory")
        (TermName "mapMemory")
        (TermName "unmapMemory")
        [ Provided (Single (TypeName parent)) (T.lowerCaseFirst parent)
        , Provided (Single (TypeName mem))              (T.lowerCaseFirst mem)
        , Provided (Single (TypeName "DeviceSize"))     "offset'"
        , Provided (Single (TypeName "DeviceSize"))     "size'"
        , Provided (Single (TypeName "MemoryMapFlags")) "flags'"
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
        (TermName "useCommandBuffer")
        (TermName "beginCommandBuffer")
        (TermName "endCommandBuffer")
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
        (TermName "withRegisteredObjectsNVX")
        (TermName "registerObjectsNVX")
        (TermName "unregisterObjectsNVX")
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

data Bracket = Bracket
  { bInnerType :: ConstructedType
  , bWrapperName :: HaskellName
  , bCreate :: HaskellName
  , bDestroy :: HaskellName
  , bCreateArguments :: [Argument]
  , bDestroyArguments :: [Argument]
  , bDestroyIndividually :: Bool
  }

data ConstructedType
  = Single { unConstructedType :: Type }
  | Optional { unConstructedType :: Type }
  | Multiple { unConstructedType :: Type }
  deriving (Eq, Ord)

constructedTypeToHsType :: ConstructedType -> WE (Doc ())
constructedTypeToHsType = \case
  Single t -> toHsType t
  Optional t -> ("Maybe" <+>) <$> toHsType t
  Multiple t -> do
    tellImport "Data.Vector" "Vector"
    ("Vector" <+>) <$> toHsType t

data Argument
  = Provided ConstructedType Text
  | Resource
  | Member Text Text
  deriving (Eq, Ord)

writePair
  :: Bracket
  -> Write (Type, HaskellName, HaskellName, WriteElement)
writePair Bracket{..} =
  let arguments = nubOrd (bCreateArguments ++ bDestroyArguments)
  in fmap (unConstructedType bInnerType, bCreate, bWrapperName,) . runWE (unHaskellName bWrapperName) $ do
    tellExport (WithoutConstructors bWrapperName)
    tellDepend (bCreate)
    tellDepend bDestroy
    argHsTypes <- traverse constructedTypeToHsType [ t | Provided t _ <- arguments]
    let argHsVars = [pretty (unReservedWord v) | Provided _ v <- arguments]
    createArgVars <- for bCreateArguments $ \case
      Provided _ v -> pure (pretty (unReservedWord v))
      Resource -> throwError "Resource used in its own construction"
      Member _ _ -> throwError "Member used during construction"
    destroyArgVars <- for bDestroyArguments $ \case
      Provided _ v -> pure $ pretty (unReservedWord v)
      Resource -> pure "o"
      Member member argument
        | [t] <- [t | Provided (Single t) v <- arguments, v == argument]
        -> do
          argumentType <- toHsType t
          pure [qci|({member} ({argument} :: {argumentType})) |]
        | otherwise
        -> throwError "Can't find single argument for member"
    innerHsType <- constructedTypeToHsType bInnerType
    let noDestructorResource = Resource `notElem` bDestroyArguments
        noResource = bInnerType == Single Void && noDestructorResource
        bracket = if noResource
                    then "bracket_"
                    else "bracket"
        cont = if noResource
                 then "IO a"
                 else "(" <> innerHsType <> " -> IO a)"
        wrapperArguments = punctuate " ->" (argHsTypes ++ [cont, "IO a"])
        resourcePattern = if noDestructorResource
                            then "_"
                            else "o"
        callDestructor = (if noResource then emptyDoc else "\\" <>
          resourcePattern <+> "->") <+> pretty (unHaskellName bDestroy) <+> hsep destroyArgVars
    tellImport "Control.Exception" bracket
    pure $ \_ -> [qci|
    -- | A safe wrapper for '{bCreate}' and '{bDestroy}' using '{bracket}'
    --
    -- The allocated value must not be returned from the provided computation
    {unHaskellName bWrapperName}
      :: {hsep wrapperArguments}
    {unHaskellName bWrapperName} {hsep argHsVars} = {bracket}
      ({bCreate} {hsep createArgVars})
      {bool emptyDoc "(traverse " bDestroyIndividually }({callDestructor}){bool emptyDoc ")" bDestroyIndividually}
  |]
