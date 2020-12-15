{-# language QuasiQuotes #-}
module XR.RenderParams
  ( renderParams
  ) where

import qualified Bespoke.RenderParams          as Vk
import           CType
import           Data.Char                      ( isLower )
import           Data.Generics.Uniplate.Data
import qualified Data.HashSet                  as Set
import qualified Data.List                     as List
import qualified Data.Text                     as T
import           Data.Text.Extra                ( lowerCaseFirst
                                                , upperCaseFirst
                                                )
import           Data.Text.Extra               as T
                                                ( (<+>) )
import           Data.Text.Prettyprint.Doc      ( pretty )
import           Data.Vector                    ( Vector )
import           Foreign.Ptr
import           Haskell
import           Language.Haskell.TH
import           Polysemy
import           Polysemy.Input
import           Relude                  hiding ( Handle
                                                , Type
                                                , uncons
                                                )
import           Render.Element
import           Render.Stmts                   ( useViaName )
import           Render.Stmts.Poke              ( CmdsDoc(..) )
import           Render.Type.Preserve
import           Spec.Parse
import           Text.Casing             hiding ( dropPrefix )
import           Text.InterpolatedString.Perl6.Unindented
import           VkModulePrefix

import           Control.Exception
import           Control.Monad.Trans.Cont
import           Data.Vector                    ( generateM )
import           Foreign.Marshal.Alloc          ( callocBytes
                                                , free
                                                )
import           Foreign.Storable               ( Storable )
import           GHC.IO.Exception               ( IOErrorType(..)
                                                , IOException(..)
                                                )

renderParams :: Vector Handle -> RenderParams
renderParams handles = r
 where
  dispatchableHandleNames = Set.fromList
    [ hName | Handle {..} <- toList handles, hDispatchable == Dispatchable ]
  vulkanParams = Vk.renderParams handles
  r            = RenderParams
    { mkTyName = \n -> TyConName . upperCaseFirst . dropXr $ n
    , mkConName = \_ n -> ConName . upperCaseFirst . dropXr $ n
    , mkMemberName                   = \_parent ->
                                         TermName . lowerCaseFirst . dropPointer . unCName
    , mkFunName                      = TermName . lowerCaseFirst . dropXr
    , mkParamName                    = TermName . dropPointer . unCName
    , mkPatternName = \n -> ConName . upperCaseFirst . dropXr $ n
    , mkFuncPointerName              = TyConName . T.tail . unCName
    , mkFuncPointerMemberName = TermName . ("p" <>) . upperCaseFirst . unCName
    , mkEmptyDataName                = TyConName . (<> "_T") . dropXr
    , mkDispatchableHandlePtrName    = TermName
                                       . (<> "Handle")
                                       . lowerCaseFirst
                                       . dropXr
    , camelPrefix                    = "Xr"
    , lowerPrefix                    = "xr"
    , upperPrefix                    = "XR"
    , flagsTypeName                  = "XrFlags64"
    , alwaysQualifiedNames = fromList (vulkanHaskellNames vulkanParams)
    , extraNewtypes = fromList (vulkanHaskellNames vulkanParams)
    , mkIdiomaticType                =
      let
        dropVulkanModule = transformBi
          (\n -> if nameModule n == Just (T.unpack vulkanModulePrefix)
            then mkName (nameBase n)
            else n
          )
        xrIdiomatic =
          (`List.lookup` (  [ ( ConT (typeName $ mkTyName r "XrBool32")
                              , IdiomaticType
                                (ConT ''Bool)
                                (do
                                  tellImport (TermName "boolToBool32")
                                  pure "boolToBool32"
                                )
                                (do
                                  tellImport (TermName "bool32ToBool")
                                  pure $ PureFunction "bool32ToBool"
                                )
                              )
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
                                    "XrInstance" -> do
                                      tellImport (TermName "initInstanceCmds")
                                      pure
                                        .   IOFunction
                                        $   "(\\h ->"
                                        <+> pretty c
                                        <+> "h <$> initInstanceCmds h)"
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
      in
        \t ->
          xrIdiomatic t
            <|> (mkIdiomaticType vulkanParams . dropVulkanModule $ t)
    , mkHsTypeOverride               = \_ structStyle preserve t ->
      case vulkanManifest structStyle vulkanParams t of
        Just t -> Just $ do
          t <- t
          case preserve of
            DoNotPreserve -> case mkIdiomaticType r t of
              Just i  -> pure $ itType i
              Nothing -> pure t
            _ -> pure t
        Nothing -> pure <$> case preserve of
          DoNotPreserve -> Nothing
          _             -> case t of
            TypeName n | Set.member n dispatchableHandleNames ->
              Just $ ConT ''Ptr :@ ConT (typeName (mkEmptyDataName r n))
            _ -> Nothing
    , unionDiscriminators            = mempty
    , successCodeType                = TypeName "XrResult"
    , isSuccessCodeReturned          = (/= "XR_SUCCESS")
    , firstSuccessCode               = "XR_SUCCESS"
    , versionType                    = TypeName "XrVersion"
    , exceptionTypeName              = TyConName "OpenXrException"
    , complexMemberLengthFunction    = \_ _ _ -> Nothing
    , isExternalName                 = const Nothing
    , externalDocHTML                = Just
      "https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html"
    , objectTypePattern              = pure
                                       . mkPatternName r
                                       . CName
                                       . ("XR_OBJECT_TYPE_" <>)
                                       . T.pack
                                       . toScreamingSnake
                                       . fromHumps
                                       . T.unpack
                                       . dropXr
    , extensibleStructTypeMemberName = Just "type"
    , extensibleStructTypeType       = Just "XrStructureType"
    , modulePrefix                   = "OpenXR"
    , commandOverrides               = commandOverrides'
    }

dropXr :: CName -> Text
dropXr (CName t) = fromMaybe t (dropPrefix "xr" t)

dropPrefix
  :: Text
  -- ^ Prefix
  -> Text
  -- ^ body
  -> Maybe Text
  -- ^ Nothing if 'body' didn't begin with 'prefix'
dropPrefix p t = if p `T.isPrefixOf` T.toLower t
  then Just . T.dropWhile (== '_') . T.drop (T.length p) $ t
  else Nothing

dropPointer :: Text -> Text
dropPointer =
  lowerCaseFirst
    . uncurry (<>)
    . first (\p -> if T.all (== 'p') p then "" else p)
    . T.span isLower

vulkanTypesModule :: Text
vulkanTypesModule = "OpenXR.VulkanTypes"

-- TODO: Generate this automatically
vulkanManifest
  :: ExtensibleStructStyle r -> RenderParams -> CType -> Maybe (Sem r Type)
vulkanManifest structStyle RenderParams {..} =
  let vk =
        Just
          . pure
          . ConT
          . mkName
          . T.unpack
          . ((vulkanTypesModule <> ".") <>)
          . unName
          . mkTyName
      someVk t = Just $ do
        let structTyCon =
              ConT
                . mkName
                . T.unpack
                . ((vulkanTypesModule <> ".") <>)
                . unName
                . mkTyName
                $ t
        case structStyle of
          -- Never expose vulkan structs as applied
          _ ->
            pure
              $  ConT
                   (mkName (T.unpack vulkanTypesModule <> "." <> "SomeStruct"))
              :@ structTyCon
  in  \case
        TypeName n | n `elem` vulkanMonoNames -> vk n
                   | n `elem` vulkanPolyNames -> someVk n
        _ -> Nothing

vulkanMonoNames, vulkanPolyNames, vulkanNames :: [CName]
vulkanMonoNames =
  [ "VkInstance"
  , "VkPhysicalDevice"
  , "VkDevice"
  , "VkImage"
  , "VkResult"
  , "VkFormat"
  , "VkAllocationCallbacks"
  , "PFN_vkGetInstanceProcAddr"
  ]
vulkanPolyNames = ["VkInstanceCreateInfo", "VkDeviceCreateInfo"]
vulkanNames = vulkanMonoNames <> vulkanPolyNames

vulkanHaskellNames :: RenderParams -> [Name]
vulkanHaskellNames RenderParams {..} =
  mkName (T.unpack vulkanTypesModule <> "." <> "SomeStruct")
    : (typeNameWithModule (ModName vulkanTypesModule) . mkTyName <$> vulkanNames
      )

----------------------------------------------------------------
-- Bespoke commands
----------------------------------------------------------------

-- xrEnumerateSwapchainImages needs special handling because its return value
-- is polymorphic and in an array.
commandOverrides'
  :: (HasRenderParams r, HasRenderElem r) => CName -> Maybe (Sem r ())
commandOverrides' = \case
  "xrEnumerateSwapchainImages" -> Just $ do
    RenderParams {..} <- input
    tellExport (ETerm (TermName "enumerateSwapchainImages"))
    traverse_
      tellImport
      [ ''FunPtr
      , ''Ptr
      , ''Word32
      , ''MonadIO
      , ''Vector
      , 'liftIO
      , 'evalContT
      , 'lift
      , 'unless
      , 'nullFunPtr
      , 'throwIO
      , 'IOError
      , 'InvalidArgument
      , 'ContT
      , 'bracket
      , 'callocBytes
      , 'free
      , 'when
      , 'generateM
      , 'traverse_
      ]
    traverse_ tellImportWithAll [''ContT, ''Storable]
    traverse_
      (tellImport . TyConName)
      [ "Swapchain_T"
      , "SomeChild"
      , "SwapchainImageBaseHeader"
      , "Inherits"
      , "FromCStruct"
      , "Swapchain"
      , ":::"
      ]
    traverse_ (tellImport . TermName) ["advancePtrBytes", "lowerChildPointer"]
    tellImport
      (mkName (T.unpack modulePrefix <> ".Internal.Utils.traceAroundEvent"))
    traverse_ (tellImportWithAll . TyConName)
              ["ToCStruct", "Result", "OpenXrException", "InstanceCmds"]
    tellDocWithHaddock $ \getDoc -> [qqi|
      foreign import ccall
      #if !defined(SAFE_FOREIGN_CALLS)
        unsafe
      #endif
        "dynamic" mkXrEnumerateSwapchainImages
        :: FunPtr (Ptr Swapchain_T -> Word32 -> Ptr Word32 -> Ptr (SomeChild SwapchainImageBaseHeader) -> IO Result) -> Ptr Swapchain_T -> Word32 -> Ptr Word32 -> Ptr (SomeChild SwapchainImageBaseHeader) -> IO Result

      {getDoc (TopLevel "xrEnumerateSwapchainImages")}
      enumerateSwapchainImages :: forall a io
                                . (Inherits SwapchainImageBaseHeader a, ToCStruct a, FromCStruct a, MonadIO io)
                               => {getDoc (Nested "xrEnumerateSwapchainImages" "swapchain")}
                                  Swapchain
                               -> io (Result, "images" ::: Vector a)
      enumerateSwapchainImages swapchain = liftIO . evalContT $ do
        let xrEnumerateSwapchainImagesPtr = pXrEnumerateSwapchainImages (instanceCmds (swapchain :: Swapchain))
        lift $ unless (xrEnumerateSwapchainImagesPtr /= nullFunPtr) $
          throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateSwapchainImages is null" Nothing Nothing
        let xrEnumerateSwapchainImages' = mkXrEnumerateSwapchainImages xrEnumerateSwapchainImagesPtr
        let swapchain' = swapchainHandle swapchain
        pImageCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
        r <- lift $ traceAroundEvent "xrEnumerateSwapchainImages" (xrEnumerateSwapchainImages' swapchain' 0 pImageCountOutput nullPtr)
        lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
        imageCountOutput <- lift $ peek @Word32 pImageCountOutput
        pImages <- ContT $ bracket (callocBytes @a (fromIntegral imageCountOutput * cStructSize @a)) free
        traverse_ (\\i -> ContT $ pokeZeroCStruct (pImages `advancePtrBytes` (i * cStructSize @a) :: Ptr a) . ($ ())) [0..fromIntegral imageCountOutput - 1]
        r' <- lift $ traceAroundEvent "xrEnumerateSwapchainImages" (xrEnumerateSwapchainImages' swapchain' imageCountOutput pImageCountOutput (lowerChildPointer pImages))
        lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
        imageCountOutput' <- lift $ peek @Word32 pImageCountOutput
        images' <- lift $ generateM (fromIntegral imageCountOutput') (\\i -> peekCStruct @a (pImages `advancePtrBytes` (cStructSize @a * i) :: Ptr a))
        pure (r', images')
    |]
  _ -> Nothing
