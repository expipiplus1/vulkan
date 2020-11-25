{-# LANGUAGE QuasiQuotes #-}

module VK.SPIRVElements
  ( renderSPIRVElements
  ) where

import           CType                          ( CType(TypeName) )
import           Data.Bits
import           Data.Foldable
import           Data.List.Extra                ( nubOrd )
import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Version                   ( Version(versionBranch)
                                                , makeVersion
                                                )
import           Error
import           Haskell                        ( HName(..)
                                                , allTypeNames
                                                , renderType
                                                , (~>)
                                                )
import           Language.Haskell.TH            ( Type(ConT)
                                                , mkName
                                                )
import           Polysemy.Input
import           Relude
import           Render.Element
import           Render.SpecInfo                ( HasSpecInfo
                                                , SpecInfo(..)
                                                )
import           Render.Type                    ( cToHsType )
import           Render.Type.Preserve           ( Preserve(DoNotPreserve) )
import           Spec.Types
import           Text.InterpolatedString.Perl6.Unindented
import           VkModulePrefix
import qualified Prelude

renderSPIRVElements
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Vector SPIRVExtension
  -> Vector SPIRVCapability
  -> Sem r RenderElement
renderSPIRVElements exts caps = genRe "SPIR-V stuff" $ do
  tellExplicitModule (vulkanModule ["SPIRVRequirements"])
  tellCanFormat
  bespokeStuff
  renderExts exts
  renderCaps caps

renderExts
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Vector SPIRVExtension
  -> Sem r ()
renderExts = renderSPIRVThing "spirvExtensionRequirements"
                              spirvExtensionName
                              spirvExtensionReqs

renderCaps
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Vector SPIRVCapability
  -> Sem r ()
renderCaps = renderSPIRVThing "spirvCapabilityRequirements"
                              spirvCapabilityName
                              spirvCapabilityReqs

renderSPIRVThing
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Text
  -> (a -> Text)
  -> (a -> Vector SPIRVRequirement)
  -> Vector a
  -> Sem r ()
renderSPIRVThing funName name reqs xs = do
  let case' x = do
        let mergeReqs = Prelude.head -- TODO: this is probably wrong! discarding the
                                     -- other reqs
        reqs' <- mergeReqs <$> traverse renderReq (V.toList (reqs x))
        pure $ viaShow (name x) <+> "->" <+> list reqs'
  cases <- (<> ["_ -> []"]) <$> traverse case' (V.toList xs)
  tellImport ''ByteString
  tellImport (mkName "Vulkan.Requirements.Requirement")
  tellExport (ETerm (TermName funName))
  tellDoc $ vsep
    [ pretty funName <+> ":: ByteString -> [Requirement]"
    , pretty funName <+> "= \\case" <> line <> indent 2 (vsep cases)
    ]

renderReq
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => SPIRVRequirement
  -> Sem r [Doc ()]
renderReq = \case
  SPIRVReqVersion   v    -> pure <$> versionReq v

  SPIRVReqExtension p    -> minVersionAndExtensionsReqs (V.singleton p)

  SPIRVReqFeature s f rs -> do
    RenderParams {..} <- input
    tellImportWithAll (mkName "Vulkan.Requirements.Requirement")
    tellLanguageExtension (LanguageExtension "OverloadedLists")
    sTy <- cToHsType DoNotPreserve (TypeName s)
    -- TODO: this is pretty lazy, import the accessors properly
    traverse_ tellImportWithAll (allTypeNames sTy)
    checkTDoc <- renderType (sTy ~> ConT ''Bool)
    sTyDoc    <- renderType sTy
    otherReqs <- minVersionAndExtensionsReqs rs
    let featureMemberName = mkMemberName s f
    let xs =
          [ ("featureName" , viaShow f)
          , ("checkFeature", pretty featureMemberName <+> "::" <+> checkTDoc)
          , ( "enableFeature"
            , "\\f ->"
              <+> "f"
              <>  braces (pretty featureMemberName <+> "= True")
              <+> "::"
              <+> sTyDoc
            )
          ]
    pure $ "RequireFeature" <> braceAssignmentList xs : otherReqs

  SPIRVReqProperty p m v rs -> do
    RenderParams {..} <- input
    tellImportWithAll (mkName "Vulkan.Requirements.Requirement")
    tellLanguageExtension (LanguageExtension "OverloadedLists")
    -- TODO: this is pretty lazy, import the accessors properly
    sTy <- cToHsType DoNotPreserve (TypeName p)
    traverse_ tellImportWithAll (allTypeNames sTy)
    sTyDoc <- renderType sTy
    let propertyMemberName = mkMemberName p m
        propertyValueName  = mkPatternName v
    otherReqs <- minVersionAndExtensionsReqs rs
    -- TODO, do this properly
    checker   <- if
      | v == "VK_TRUE"
      -> pure $ "\\p ->" <+> pretty propertyMemberName <+> parens
        ("p ::" <+> sTyDoc)
      | "_BIT" `T.isInfixOf` unCName v
      -> do
        tellImport propertyValueName
        pure
          $   "\\p ->"
          <+> pretty propertyValueName
          <+> ".&&."
          <+> pretty propertyMemberName
          <+> parens ("p ::" <+> sTyDoc)
      | otherwise
      -> do
        tellImport propertyValueName
        pure
          $   "\\p ->"
          <+> pretty propertyValueName
          <+> "=="
          <+> pretty propertyMemberName
          <+> parens ("p ::" <+> sTyDoc)
    let xs = [("propertyName", viaShow p), ("checkProperty", checker)]
    pure $ "RequireProperty" <> braceAssignmentList xs : otherReqs

minVersionAndExtensionsReqs
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => Vector Text
  -> Sem r [Doc ()]
minVersionAndExtensionsReqs rs = do
  SpecInfo {..} <- input
  let dependencies = nubOrd $ toList rs <> concatMap siExtensionDeps rs
  (minVersions, instanceExtensions, deviceExtensions) <-
    foldMap
        (\case
          RequireVersion v             -> ([v], [], [])
          RequireInstanceExtension e v -> ([], [(e, v)], [])
          RequireDeviceExtension   e v -> ([], [], [(e, v)])
        )
      <$> traverse extensionNamePattern dependencies

  v <- case minVersions of
    [] -> pure Nothing
    xs -> Just <$> versionReq (maximum xs)

  pure
    $  maybeToList v
    <> [ "RequireInstanceExtension" <> braceAssignmentList
           [ ("instanceExtensionLayerName" , "Nothing")
           , ("instanceExtensionName"      , e)
           , ("instanceExtensionMinVersion", v)
           ]
       | (e, v) <- instanceExtensions
       ]
    <> [ "RequireDeviceExtension" <> braceAssignmentList
           [ ("deviceExtensionLayerName" , "Nothing")
           , ("deviceExtensionName"      , e)
           , ("deviceExtensionMinVersion", v)
           ]
       | (e, v) <- deviceExtensions
       ]

extensionNamePattern
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Text
  -> Sem r RequireType
extensionNamePattern p = do
  RenderParams {..} <- input
  SpecInfo {..}     <- input
  case parseVersion p of
    Just v  -> pure . RequireVersion $ v
    Nothing -> do
      -- TODO: do this properly lol
      let patternPrefix = if
            | p == "VK_KHR_maintenance2"   -> "VK_KHR_MAINTENANCE2"
            | p == "VK_NV_viewport_array2" -> "VK_NV_VIEWPORT_ARRAY2"
            | "2" `T.isSuffixOf` p         -> T.toUpper (T.init p) <> "_2"
            | otherwise                    -> T.toUpper p
          nameName    = CName $ patternPrefix <> "_EXTENSION_NAME"
          versionName = CName $ patternPrefix <> "_SPEC_VERSION"
      let namePattern    = mkPatternName nameName
          versionPattern = mkPatternName versionName
      tellImport namePattern
      tellImport versionPattern
      case siExtensionType p of
        Just DeviceExtension -> pure $ RequireDeviceExtension
          (pretty namePattern)
          (pretty versionPattern)
        Just InstanceExtension -> pure $ RequireInstanceExtension
          (pretty namePattern)
          (pretty versionPattern)
        Just UnknownExtensionType ->
          throw $ "Dependency on extension of unknown type: " <> show p
        Nothing -> throw $ "Dependency on unknown extension" <> show p

data RequireType
  = RequireVersion Version
  | RequireInstanceExtension (Doc ()) (Doc ())
  | RequireDeviceExtension (Doc ()) (Doc ())
  -- ^ _EXTENSION_NAME and _SPEC_VERSION

versionReq
  :: (HasRenderParams r, HasRenderElem r, HasErr r) => Version -> Sem r (Doc ())
versionReq v = do
  tellImportWithAll (mkName "Vulkan.Requirements.Requirement")
  vDoc <- versionDoc v
  pure $ "RequireVersion $" <+> vDoc

versionDoc
  :: (HasRenderParams r, HasRenderElem r, HasErr r) => Version -> Sem r (Doc ())
versionDoc v = do
  tellImport (ConName "MAKE_VERSION")
  (ma, mi, pa) <- case versionBranch v of
    [ma]         -> pure (ma, 0, 0)
    [ma, mi]     -> pure (ma, mi, 0)
    [ma, mi, pa] -> pure (ma, mi, pa)
    []           -> throw "Version branch has no components"
    _            -> throw "Version branch has more than three components"
  pure $ "MAKE_VERSION" <+> hsep (viaShow <$> [ma, mi, pa])

parseVersion :: Text -> Maybe Version
parseVersion t = do
  let p = "VK_VERSION_"
  v <- if p `T.isPrefixOf` t then pure $ T.drop (T.length p) t else empty
  let cs = T.split (== '_') v
  is <- traverse (readMaybe . T.unpack) cs
  pure $ makeVersion is

bespokeStuff :: (HasRenderParams r, HasRenderElem r) => Sem r ()
bespokeStuff = do
  tellImport ''Bits
  tellImport '(.&.)
  tellImport 'zeroBits
  tellDoc [qqi|
    -- | Check if the intersection of bits is non-zero
    (.&&.) :: Bits a => a -> a -> Bool
    x .&&. y = (x .&. y) /= zeroBits
  |]


----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

braceAssignmentList :: [(Doc ann, Doc ann)] -> Doc ann
braceAssignmentList = encloseSep "{" "}" ", " . fmap (\(l, r) -> l <+> "=" <+> r)
