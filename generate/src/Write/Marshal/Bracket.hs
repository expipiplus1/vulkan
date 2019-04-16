{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Marshal.Bracket
  ( Bracket(..)
  -- , extractBrackets
  , bracketCommand
  , insertBracketDependency
  , brackets
  ) where

import           Control.Monad
import           Control.Monad.Except
import           Data.Foldable
import           Data.List                                ( (\\) )
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.Text                                ( Text )
import qualified Data.Text.Extra               as T
import           Data.Text.Prettyprint.Doc         hiding ( brackets )
import           Data.Traversable
import           Prelude                           hiding ( Enum )
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Error
import           Spec.Savvy.Handle
import           Spec.Savvy.Command
import           Spec.Savvy.Type

import           Write.Element                     hiding ( TypeName )
import qualified Write.Element                 as WE
import           Write.Marshal.Monad
import           Write.Marshal.Util

import           Debug.Trace

data Bracket = Bracket
  { bName :: HaskellName
  , bType :: Type
  , bParentType :: Maybe Type
  , bCreateInfoType :: Type
  , bOpen :: Command
  , bClose :: Command
  }
  deriving(Show)

-- extractBrackets :: [Command] -> [Bracket]
-- extractBrackets commands =
--   let
--     isOpener c@Command {..} = guard ("vkCreate" `T.isPrefixOf` cName) $> c
--     isCloser c@Command {..} = guard ("vkDestroy" `T.isPrefixOf` cName) $> c
--     splitExtras :: [Parameter] -> ([Parameter], [Parameter])
--     dropParent :: [Parameter] -> (Maybe Parameter, [Parameter])
--     dropParent = \case
--       x : xs
--         | TypeName p <- pType x
--         , p == "VkDevice" || p == "VkInstance"
--         , Nothing <- pIsOptional x
--         -> (Just x, xs)
--       xs -> (Nothing, xs)
--     isPair co cc
--       | Just h <- T.dropPrefix "vkCreate" (cName co)
--       , Just h' <- T.dropPrefix "vkDestroy" (cName cc)
--       , h == h'
--       = case () of
--         _
--           | (parent, [createInfo, allocationCallbacks, result]) <- dropParent
--             $ cParameters co
--           , (parent', [result', allocationCallbacks']) <- dropParent
--             $ cParameters cc
--           , allocationCallbacks == allocationCallbacks'
--           , Ptr Const (TypeName "VkAllocationCallbacks") <- pType
--             allocationCallbacks
--           , parent == parent'
--           , Ptr Const createInfo <- pType createInfo
--           , Ptr NonConst t <- pType result
--           , pType result' == t
--           , TypeName vn <- t
--           -> let name = TermName ("with" <> dropVkType vn)
--              in  Just (Bracket name t (pType <$> parent) createInfo co cc)
--         _ -> error ("unhandled " ++ show (co))
--       | otherwise
--       = Nothing
--   in
--     pairs isOpener isCloser isPair commands

-- | TODO: Check that all handles are handled
brackets :: [Handle] -> Either [SpecError] [((HaskellName, HaskellName), WriteElement)]
brackets handles = do
  noParent <- traverse noParentConstructor ["Instance"]
  simples  <- traverse
    (uncurry simpleConstructor)
    [ -- ("Instance"      , "PhysicalDevice")
      ("PhysicalDevice", "Device")
    , ("Device"        , "CommandPool")
    , ("Device"        , "Buffer")
    , ("Device"        , "BufferView")
    , ("Device"        , "Image")
    , ("Device"        , "ImageView")
    , ("Device"        , "ShaderModule")
    , ("Device"        , "PipelineLayout")
    , ("Device"        , "Sampler")
    , ("Device"        , "DescriptorSetLayout")
    , ("Device"        , "DescriptorPool")
    , ("Device"        , "Fence")
    , ("Device"        , "Semaphore")
    , ("Device"        , "Event")
    , ("Device"        , "QueryPool")
    , ("Device"        , "Framebuffer")
    , ("Device"        , "RenderPass")
    , ("Device"        , "PipelineCache")
    , ("Device"        , "ObjectTableNVX")
    , ("Device"        , "IndirectCommandsLayoutNVX")
    , ( "Device"
      , "DescriptorUpdateTemplate"
      )
   -- <type category="handle" name="VkDescriptorUpdateTemplateKHR" alias="VkDescriptorUpdateTemplate"/>
    , ( "Device"
      , "SamplerYcbcrConversion"
      )
   -- <type category="handle" name="VkSamplerYcbcrConversionKHR"   alias="VkSamplerYcbcrConversion"/>
    , ("Device", "ValidationCacheEXT")
    , ( "Device"
      , "AccelerationStructureNV"
      )

    , ("SurfaceKHR"                 , "SwapchainKHR")
    , ("Instance"                   , "DebugReportCallbackEXT")
    , ("Instance"                   , "DebugUtilsMessengerEXT")
    ]
  allocated <- traverse (uncurry allocateFreePair) [
                                                   ]
  let rs               = noParent ++ simples
      ignoredHandles   = ["PhysicalDevice", "Queue", "DisplayKHR"]
      handleNames      = dropVkType . hName <$> handles
      bracketNames     = [ unHaskellName n | (n, _, _, _) <- rs ]
      unhandledHandles = handleNames \\ (bracketNames ++ ignoredHandles)
  unless (null unhandledHandles)
    -- $ throwError [Other ("Unbracketed handles: " <> T.tShow unhandledHandles)]
    $ traceShowM [Other ("Unbracketed handles: " <> T.tShow unhandledHandles)]
  pure [ ((c, b), w) | (_, c, b, w) <- rs ]

noParentConstructor :: Text -> Either [SpecError] (HaskellName, HaskellName, HaskellName, WriteElement)
noParentConstructor typename =
  let objectTerm = unReservedWord $ T.lowerCaseFirst typename
      create = "create" <> typename
      destroy = "destroy" <> typename
      withName = "with" <> typename
  in fmap (WE.TypeName typename, TermName create, TermName withName,) . wrapMToWriteElements ("with" <> typename) Nothing $ do
    tellExport (Unguarded (WithoutConstructors (TermName withName)))
    tellImport "Control.Exception" "bracket"
    tellDepend (Unguarded (TermName create))
    tellDepend (Unguarded (TermName destroy))
    tellDepend (Unguarded (WE.TypeName typename))
    tellDepend (Unguarded (WE.TypeName "AllocationCallbacks"))
    pure $ \_ -> [qci|
    -- | Wrapper for '{create}' and '{destroy}' using 'bracket'
    {withName}
      :: {typename}CreateInfo
      -> Maybe AllocationCallbacks
      -> ({typename} -> IO a)
      -> IO a
    {withName} createInfo allocationCallbacks = bracket
      ({create} createInfo allocationCallbacks)
      (\\{objectTerm} -> {destroy} {objectTerm} allocationCallbacks)
  |]

simpleConstructor :: Text -> Text -> Either [SpecError] (HaskellName, HaskellName, HaskellName, WriteElement)
simpleConstructor parent typename =
  let parentTerm = unReservedWord $ T.lowerCaseFirst parent
      objectTerm = unReservedWord $ T.lowerCaseFirst typename
      create = "create" <> typename
      destroy = "destroy" <> typename
      withName = "with" <> typename
  in fmap (WE.TypeName typename, TermName create, TermName withName,) . wrapMToWriteElements ("with" <> typename) Nothing $ do
    tellExport (Unguarded (WithoutConstructors (TermName withName)))
    tellImport "Control.Exception" "bracket"
    tellDepend (Unguarded (TermName create))
    tellDepend (Unguarded (TermName destroy))
    tellDepend (Unguarded (WE.TypeName parent))
    tellDepend (Unguarded (WE.TypeName typename))
    pure $ \_ -> [qci|
    -- | Wrapper for '{create}' and '{destroy}' using 'bracket'
    {withName}
      :: {parent}
      -> {typename}CreateInfo
      -> Maybe AllocationCallbacks
      -> ({typename} -> IO a)
      -> IO a
    {withName} {parentTerm} createInfo allocationCallbacks = bracket
      ({create} {parentTerm} createInfo allocationCallbacks)
      (\\{objectTerm} -> {destroy} {parentTerm} {objectTerm} allocationCallbacks)
  |]

allocateFreePair :: Text -> Text -> Either [SpecError] (HaskellName, HaskellName, HaskellName, WriteElement)
allocateFreePair parent typename =
  let parentTerm = unReservedWord $ T.lowerCaseFirst parent
      objectTerm = unReservedWord $ T.lowerCaseFirst typename
      create = "allocate" <> typename <> "s"
      destroy = "free" <> typename <> "s"
      withName = "with" <> typename
  in fmap (WE.TypeName typename, TermName create, TermName withName,) . wrapMToWriteElements ("with" <> typename) Nothing $ do
    tellExport (Unguarded (WithoutConstructors (TermName withName)))
    tellImport "Control.Exception" "bracket"
    tellDepend (Unguarded (TermName create))
    tellDepend (Unguarded (TermName destroy))
    tellDepend (Unguarded (WE.TypeName parent))
    tellDepend (Unguarded (WE.TypeName typename))
    pure $ \_ -> [qci|
    -- | Wrapper for '{create}' and '{destroy}' using 'bracket'
    {withName}
      :: {parent}
      -> {typename}CreateInfo
      -> Maybe AllocationCallbacks
      -> ({typename} -> IO a)
      -> IO a
    {withName} {parentTerm} createInfo allocationCallbacks = bracket
      ({create} {parentTerm} createInfo allocationCallbacks)
      (\\{objectTerm} -> {destroy} {parentTerm} {objectTerm} allocationCallbacks)
  |]

insertBracketDependency :: [Bracket] -> WriteElement -> WriteElement
insertBracketDependency bs we@WriteElement {..} =
  let weDepends' =
        weDepends
          ++ [ p $> bName
             | Bracket {..} <- bs
             , p            <- weProvides
             , TypeName n   <- [bType]
             , dropVkType n == unHaskellName (unExport (unGuarded p))
             ]
  in  WriteElement {weDepends = weDepends', ..}

-- | Find pairs of commands which should be called using the bracket pattern
bracketCommand :: Bracket -> Either [SpecError] WriteElement
bracketCommand b = do
  let weName        = T.tShow (bName b)
      weBootElement = Nothing
  (weDoc, (weImports, (weProvides, weUndependableProvides), (weDepends, weSourceDepends), weExtensions, _)) <-
    either (throwError . fmap (WithContext weName))
           pure
           (runWrap $ bracketCommand' b)
  pure WriteElement {..}

bracketCommand' :: Bracket -> WrapM (DocMap -> Doc ())
bracketCommand' Bracket{..} = do
  let t = dropVkType . typeName $ bType
      p = dropVkType . typeName <$> bParentType
      ci = dropVkType . typeName $ bCreateInfoType
      co = dropVk $ cName bOpen
      cc = dropVk $ cName bClose
  tellExport (Unguarded (WithoutConstructors bName))
  tellImport "Control.Exception" "bracket"
  tellDepend (Unguarded (TermName co))
  tellDepend (Unguarded (TermName cc))
  traverse_ (tellDepend . Unguarded . WE.TypeName) p
  pure $ \_ -> [qci|
    -- | Wrapper for '{co}' and '{cc}' using 'bracket'
    {unHaskellName bName} :: {maybe "" (<> " -> ") p}{ci} -> Maybe AllocationCallbacks -> ({t} -> IO a) -> IO a
    {unHaskellName bName} {maybe "" (const "parent ") p}createInfo allocationCallbacks =
      bracket
        ({co} {maybe "" (const "parent ") p}createInfo allocationCallbacks)
        (\o -> {cc} {maybe "" (const "parent ") p}o allocationCallbacks)
  |]

typeName :: Type -> Text
typeName = \case
  TypeName t -> t
  _          -> error "typeName on non TypeName"

pairs :: (a -> Maybe l) -> (a -> Maybe r) -> (l -> r -> Maybe p) -> [a] -> [p]
pairs l r p xs =
  [ p' | Just l' <- l <$> xs, Just r' <- r <$> xs, Just p' <- [p l' r'] ]
