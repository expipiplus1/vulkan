{-# LANGUAGE QuasiQuotes #-}

module VK.SPIRVElements
  ( renderSPIRVElements
  ) where

import           CType                          ( CType(TypeName) )
import           Data.Bits
import           Data.Foldable
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
import           Render.SpecInfo                ( HasSpecInfo )
import           Render.Type                    ( cToHsType )
import           Render.Type.Preserve           ( Preserve(DoNotPreserve) )
import           Spec.Types
import           Text.InterpolatedString.Perl6.Unindented
import           VkModulePrefix

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
        reqs' <- traverse renderReq (V.toList (reqs x))
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
  -> Sem r (Doc ())
renderReq = \case
  SPIRVReqVersion   v -> versionReq v
  SPIRVReqExtension p -> do
    extensionNamePattern p >>= \case
      Left  v  -> versionReq v
      Right p' -> do
        tellImportWithAll (mkName "Vulkan.Requirements.Requirement")
        pure $ "RequireExtension" <+> p'
  SPIRVReqFeature s f rs -> do
    RenderParams {..} <- input
    tellImportWithAll (mkName "Vulkan.Requirements.Requirement")
    tellLanguageExtension (LanguageExtension "OverloadedLists")
    sTy <- cToHsType DoNotPreserve (TypeName s)
    -- TODO: this is pretty lazy, import the accessors properly
    traverse_ tellImportWithAll (allTypeNames sTy)
    checkTDoc    <- renderType (sTy ~> ConT ''Bool)
    sTyDoc       <- renderType sTy
    otherSetters <- minVersionAndExtensionsSetters rs
    let featureMemberName = mkMemberName f
    let xs =
          [ "featureName =" <+> viaShow f
            , "checkFeature =" <+> pretty featureMemberName <+> "::" <+> checkTDoc
            , "enableFeature ="
            <+> "\\f ->"
            <+> "f"
            <>  braces (pretty featureMemberName <+> "= True")
            <+> "::"
            <+> sTyDoc
            ]
            <> otherSetters
    pure $ "RequireFeature" <> encloseSep "{" "}" ", " xs
  SPIRVReqProperty p m v rs -> do
    RenderParams {..} <- input
    tellImportWithAll (mkName "Vulkan.Requirements.Requirement")
    tellLanguageExtension (LanguageExtension "OverloadedLists")
    -- TODO: this is pretty lazy, import the accessors properly
    sTy <- cToHsType DoNotPreserve (TypeName p)
    traverse_ tellImportWithAll (allTypeNames sTy)
    sTyDoc <- renderType sTy
    let propertyMemberName = mkMemberName m
        propertyValueName  = mkPatternName v
    otherSetters <- minVersionAndExtensionsSetters rs
    -- TODO, do this properly
    checker      <- if
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
    let xs =
          ["propertyName =" <+> viaShow p, "checkProperty =" <+> checker]
            <> otherSetters
    pure $ "RequireProperty" <> encloseSep "{" "}" ", " xs

minVersionAndExtensionsSetters
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => Vector Text
  -> Sem r [Doc ()]
minVersionAndExtensionsSetters rs = do
  (minVersions, extensionNames) <-
    partitionEithers . V.toList <$> traverse extensionNamePattern rs
  minVersionDoc <- case minVersions of
    [] -> pure "Nothing"
    vs -> ("Just $" <+>) <$> versionDoc (maximum vs)
  pure
    [ "requireMinVersion =" <+> minVersionDoc
    , "requireExtensions =" <+> viaShow extensionNames
    ]

extensionNamePattern
  :: (HasRenderElem r, HasRenderParams r)
  => Text
  -> Sem r (Either Version (Doc ()))
extensionNamePattern p = do
  RenderParams {..} <- input
  case parseVersion p of
    Just v  -> pure . Left $ v
    Nothing -> do
      let nameName = case p of
            -- TODO: Handle these properly
            "VK_INTEL_shader_integer_functions2" ->
              CName "VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME"
            _ -> CName $ T.toUpper p <> "_EXTENSION_NAME"
      let p' = mkPatternName nameName
      tellImport p'
      pure . Right $ pretty p'

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
