module Main
  where

import           Relude                  hiding ( runReader
                                                , uncons
                                                , Type
                                                , Handle
                                                )
import           Relude.Extra.Map
import           Say
import           Data.Char                      ( isLower )
import           System.TimeIt
import           Polysemy
import           Polysemy.Fixpoint
import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import           Polysemy.Reader
import qualified Data.Vector.Storable.Sized    as VSS
import qualified Data.Vector                   as V
import qualified Data.Text                     as T
import           Data.Tuple.Extra               ( curry3 )
import qualified Data.List                     as List
import           Data.Text.Extra                ( lowerCaseFirst
                                                , upperCaseFirst
                                                , (<+>)
                                                )
import           Data.Text.Prettyprint.Doc      ( pretty )
import           Language.Haskell.TH            ( nameBase )

import           Foreign.C.Types
import           Foreign.Ptr

import           CType
import           Error
import           Marshal
import           Marshal.Scheme
import           Render.Element
import           Render.SpecInfo
import           Render.Utils
import           Render.Names
import           Render.Element.Write
import           Render.Aggregate
import           Render.Stmts                   ( useViaName )
import           Render.Stmts.Poke              ( CmdsDoc(..) )
import           Bespoke                        ( BespokeScheme(..)
                                                , bespokeSchemes
                                                , assignBespokeModules
                                                )
import           Render.Spec
import           Spec.Parse
import           Render.Type.Preserve
import           Haskell
import           AssignModules
import           Documentation.All

main :: IO ()
main =
  (runFinal . embedToFinal @IO . fixpointToFinal @IO . runErr $ go) >>= \case
    Left es -> do
      traverse_ sayErr es
      sayErr (show (length es) <+> "errors")
    Right () -> pure ()
 where
  go :: Sem '[Err , Fixpoint , Embed IO , Final IO] ()
  go = do
    specText <- timeItNamed "Reading spec"
      $ readFileBS "./Vulkan-Docs/xml/vk.xml"

    document <- liftIO
      $ loadAllDocumentation [] "./Vulkan-Docs" "./Vulkan-Docs/man"

    (spec@Spec {..}, getSize) <- timeItNamed "Parsing spec" $ parseSpec specText

    runReader (renderParams specHandles)
      . withRenderedNames spec
      . withSpecInfo spec getSize
      . withTypeInfo spec
      $ do
          bespokeSchemes <- bespokeSchemes spec
          let
            aliasMap :: Map.HashMap CName CName
            aliasMap =
              fromList [ (aName, aTarget) | Alias {..} <- toList specAliases ]
            resolveAlias :: CName -> CName
            resolveAlias t = maybe t resolveAlias (Map.lookup t aliasMap) -- TODO: handle cycles!
            bitmaskNames :: HashSet CName
            bitmaskNames = fromList
              [ eName | Enum {..} <- toList specEnums, eType == ABitmask ]
            isBitmask     = (`member` bitmaskNames)
            isBitmaskType = \case
              TypeName n -> isBitmask n || isBitmask (resolveAlias n)
              _          -> False
            nonDispatchableHandleNames :: HashSet CName
            nonDispatchableHandleNames = fromList
              [ hName
              | Handle {..} <- toList specHandles
              , hDispatchable == NonDispatchable
              ]
            isNonDispatchableHandle     = (`member` nonDispatchableHandleNames)
            isNonDispatchableHandleType = \case
              TypeName n -> isNonDispatchableHandle n
                || isNonDispatchableHandle (resolveAlias n)
              _ -> False
            dispatchableHandleNames :: HashSet CName
            dispatchableHandleNames = fromList
              [ hName
              | Handle {..} <- toList specHandles
              , hDispatchable == Dispatchable
              ]
            -- TODO Remove, these will not be defaultable once we bundle the
            -- command pointers
            isDispatchableHandle     = (`member` dispatchableHandleNames)
            isDispatchableHandleType = \case
              TypeName n ->
                isDispatchableHandle n || isDispatchableHandle (resolveAlias n)
              _ -> False
            mps = MarshalParams
              (    isDefaultable'
              <||> isBitmaskType
              <||> isNonDispatchableHandleType
              <||> isDispatchableHandleType
              )
              isPassAsPointerType'
              (\p a ->
                asum . fmap (\(BespokeScheme f) -> f p a) $ bespokeSchemes
              )

          (ss, us, cs) <- runReader mps $ do
            ss <- timeItNamed "Marshaling structs"
              $ traverseV marshalStruct specStructs
            us <- timeItNamed "Marshaling unions"
              $ traverseV marshalStruct specUnions
            cs <- timeItNamed "Marshaling commands"
              $ traverseV marshalCommand specCommands
              -- TODO: Don't use all commands here, just those commands referenced by
              -- features and extensions. Similarly for specs
            pure (ss, us, cs)

          renderElements <-
            timeItNamed "Rendering"
            $   traverse evaluateWHNF
            =<< renderSpec spec ss us cs

          groups <-
            timeItNamed "Segmenting"
            $   assignModules spec
            =<< assignBespokeModules renderElements

          timeItNamed "writing" $ renderSegments "out" (mergeElements groups)

----------------------------------------------------------------
-- Render Params
----------------------------------------------------------------

renderParams :: V.Vector Handle -> RenderParams
renderParams handles = r
 where
  dispatchableHandleNames = Set.fromList
    [ hName | Handle {..} <- toList handles, hDispatchable == Dispatchable ]
  r = RenderParams
    { mkTyName = TyConName . unReservedWord . upperCaseFirst . dropVk
    , mkConName                   = \parent ->
                                      ConName
                                        . unReservedWord
                                        . (case parent of
                                            "VkPerformanceCounterResultKHR" -> ("Counter" <>)
                                            _ -> id
                                          )
                                        . upperCaseFirst
                                        . dropVk
    , mkMemberName                = TermName
                                    . unReservedWord
                                    . lowerCaseFirst
                                    . dropPointer
                                    . unCName
    , mkFunName = TermName . unReservedWord . lowerCaseFirst . dropVk
    , mkParamName = TermName . unReservedWord . dropPointer . unCName
    , mkPatternName               = ConName . unReservedWord . dropVk
    , mkFuncPointerName = TyConName . unReservedWord . T.tail . unCName
    , mkFuncPointerMemberName     = TermName
                                    . unReservedWord
                                    . ("p" <>)
                                    . upperCaseFirst
                                    . unCName
    , mkEmptyDataName             = TyConName . (<> "_T") . dropVk
    , mkDispatchableHandlePtrName = TermName
                                    . (<> "Handle")
                                    . lowerCaseFirst
                                    . dropVk
    , alwaysQualifiedNames        = V.fromList [''VSS.Vector]
    , mkIdiomaticType             =
      (`List.lookup` (  [ wrappedIdiomaticType ''Float  ''CFloat  'CFloat
                        , wrappedIdiomaticType ''Int32  ''CInt    'CInt
                        , wrappedIdiomaticType ''Double ''CDouble 'CDouble
                        , wrappedIdiomaticType ''Word64 ''CSize   'CSize
                        ]
                     <> [ ( ConT ''Ptr
                            :@ ConT (typeName $ mkEmptyDataName r name)
                          , IdiomaticType
                            (ConT (typeName $ mkTyName r name))
                            (do
                              let h = mkDispatchableHandlePtrName r name
                              tellImportWithAll (mkTyName r name)
                              pure (pretty h)
                            )
                            (do
                              let c = mkConName r name name
                              tellImportWith (mkTyName r name) c
                              case name of
                                "VkInstance" -> do
                                  tellImport (TermName "initInstanceCmds")
                                  pure
                                    .   IOFunction
                                    $   "(\\h ->"
                                    <+> pretty c
                                    <+> "h <$> initInstanceCmds h)"
                                "VkDevice" -> do
                                  tellImport (TermName "initDeviceCmds")
                                  CmdsDoc cmds <- useViaName "cmds"
                                  pure
                                    .   IOFunction
                                    $   "(\\h ->"
                                    <+> pretty c
                                    <+> "h <$> initDeviceCmds"
                                    <+> cmds
                                    <+> "h)"
                                _ -> do
                                  CmdsDoc cmds <- useViaName "cmds"
                                  pure
                                    .   PureFunction
                                    $   "(\\h ->"
                                    <+> pretty c
                                    <+> "h"
                                    <+> cmds
                                    <+> ")"
                            )
                          )
                        | name <- toList dispatchableHandleNames
                        ]
                     )
      )
    , mkHsTypeOverride            = \preserve t -> case preserve of
      DoNotPreserve -> Nothing
      _             -> case t of
        TypeName n | Set.member n dispatchableHandleNames ->
          Just $ ConT ''Ptr :@ ConT (typeName (mkEmptyDataName r n))
        _ -> Nothing
    , unionDiscriminators         = V.fromList
      [ UnionDiscriminator
        "VkPipelineExecutableStatisticValueKHR"
        "VkPipelineExecutableStatisticFormatKHR"
        "format"
        [ ("VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_BOOL32_KHR" , "b32")
        , ("VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_INT64_KHR"  , "i64")
        , ("VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_UINT64_KHR" , "u64")
        , ("VK_PIPELINE_EXECUTABLE_STATISTIC_FORMAT_FLOAT64_KHR", "f64")
        ]
      , UnionDiscriminator
        "VkPerformanceValueDataINTEL"
        "VkPerformanceValueTypeINTEL"
        "type"
        [ ("VK_PERFORMANCE_VALUE_TYPE_UINT32_INTEL", "value32")
        , ("VK_PERFORMANCE_VALUE_TYPE_UINT64_INTEL", "value64")
        , ("VK_PERFORMANCE_VALUE_TYPE_FLOAT_INTEL" , "valueFloat")
        , ("VK_PERFORMANCE_VALUE_TYPE_BOOL_INTEL"  , "valueBool")
        , ("VK_PERFORMANCE_VALUE_TYPE_STRING_INTEL", "valueString")
        ]
      ]
    , successCodeType             = TypeName "VkResult"
    , isSuccessCodeReturned       = (/= "VK_SUCCESS")
    , firstSuccessCode            = "VK_SUCCESS"
    , exceptionTypeName           = TyConName "VulkanException"
    , complexMemberLengthFunction = curry3 $ \case
      ("pAllocateInfo", "descriptorSetCount", sibling) -> Just $ do
        tellQualImport 'V.length
        tellImportWithAll (mkTyName r "VkDescriptorSetAllocateInfo")
        pure
          $   "fromIntegral . Data.Vector.length ."
          <+> pretty (mkMemberName r "pSetLayouts")
          <+> "$"
          <+> sibling
      _ -> Nothing
    }

wrappedIdiomaticType
  :: Name
  -- ^ Wrapped type
  -> Name
  -- ^ Wrapping type constructor
  -> Name
  -- ^ Wrapping constructor
  -> (Type, IdiomaticType)
  -- ^ (Wrapping type (CFloat), idiomaticType)
wrappedIdiomaticType t w c =
  ( ConT w
  , IdiomaticType
    (ConT t)
    (do
      tellImportWith w c
      pure (pretty (nameBase c))
    )
    (do
      tellImportWith w c
      pure . Constructor . pretty . nameBase $ c
    )
  )


----------------------------------------------------------------
-- Bespoke Vulkan stuff
----------------------------------------------------------------

dropVk :: CName -> Text
dropVk (CName t) = if "vk" `T.isPrefixOf` T.toLower t
  then T.dropWhile (== '_') . T.drop 2 $ t
  else t

dropPointer :: Text -> Text
dropPointer =
  lowerCaseFirst
    . uncurry (<>)
    . first (\p -> if T.all (== 'p') p then "" else p)
    . T.span isLower

isDefaultable' :: CType -> Bool
isDefaultable' t = isDefaultableForeignType t || isIntegral t

isIntegral :: CType -> Bool
isIntegral =
  (`elem` [ Int
          , Char
          , TypeName "uint8_t"
          , TypeName "uint16_t"
          , TypeName "uint32_t"
          , TypeName "uint64_t"
          , TypeName "int8_t"
          , TypeName "int16_t"
          , TypeName "int32_t"
          , TypeName "int64_t"
          , TypeName "size_t"
          , TypeName "VkDeviceSize"
          , TypeName "VkDeviceAddress"
          ]
  )

isDefaultableForeignType :: CType -> Bool
isDefaultableForeignType =
  (`elem` [ TypeName "HANDLE"
          , TypeName "DWORD"
          , TypeName "LPCWSTR"
          , TypeName "PFN_vkInternalAllocationNotification"
          , TypeName "PFN_vkInternalFreeNotification"
          , TypeName "PFN_vkAllocationFunction"
          , TypeName "PFN_vkReallocationFunction"
          , TypeName "PFN_vkFreeFunction"
          , Ptr CType.Const (TypeName "SECURITY_ATTRIBUTES")
          ]
  )

-- | Is this a type we don't want to marshal
isPassAsPointerType' :: CType -> Bool
isPassAsPointerType' = \case
  TypeName n
    | n
      `elem` [ "MirConnection"
             , "wl_display"
             , "wl_surface"
             , "Display"
             , "xcb_connection_t"
             , "AHardwareBuffer"
             , "ANativeWindow"
             , "CAMetalLayer"
             , "SECURITY_ATTRIBUTES"
             ]
    -> True
  _ -> False

-- TODO: Remove, extra union names and handle
extraStructNames :: [CName]
extraStructNames = ["VkClearColorValue", "VkSemaphore"]

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
