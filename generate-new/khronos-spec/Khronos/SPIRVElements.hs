{-# LANGUAGE QuasiQuotes #-}

module Khronos.SPIRVElements
  ( renderSPIRVElements
  ) where

import           CType                          ( CType(TypeName) )
import           Data.Bits
import           Data.Foldable
import qualified Data.HashMap.Strict           as HM
import           Data.List.Extra                ( nubOrd )
import qualified Data.Text                     as T
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Version                   ( Version
                                                , makeVersion
                                                )
import           Error
import           Haskell                        ( HName(..)
                                                , allTypeNames
                                                , renderType
                                                )
import           Khronos.Utils
import           Language.Haskell.TH            ( mkName )
import           Marshal.Scheme                 ( isElided )
import           Marshal.Struct                 ( MarshaledStruct
                                                , msMembers
                                                , msmScheme
                                                )
import           Polysemy                       ( MemberWithError )
import           Polysemy.Input
import qualified Prelude
import           Prettyprinter
import           Relude
import           Render.Element
import           Render.SpecInfo                ( HasSpecInfo
                                                , SpecInfo(..)
                                                )
import           Render.Type                    ( cToHsType )
import           Render.Type.Preserve           ( Preserve(DoNotPreserve) )
import           Render.Names
import           Spec.Types
import           Text.InterpolatedString.Perl6.Unindented

renderSPIRVElements
  :: (HasErr r, HasRenderParams r, HasSpecInfo r, HasRenderedNames r)
  => Vector SPIRVExtension
  -> Vector SPIRVCapability
  -> HashMap CName (MarshaledStruct AStruct)
  -> Sem r RenderElement
renderSPIRVElements exts caps structs =
  genRe "SPIR-V stuff" $ runInputConst (`HM.lookup` structs) $ do
    tellExplicitModule =<< mkModuleName ["SPIRVRequirements"]
    tellCanFormat
    bespokeStuff
    renderExts exts
    renderCaps caps

type HasMarshalledStructs r
  = MemberWithError (Input (CName -> Maybe (MarshaledStruct AStruct))) r

renderExts
  :: ( HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , HasMarshalledStructs r
     , HasRenderedNames r
     )
  => Vector SPIRVExtension
  -> Sem r ()
renderExts = renderSPIRVThing "spirvExtensionRequirements"
                              spirvExtensionName
                              spirvExtensionReqs

renderCaps
  :: ( HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , HasMarshalledStructs r
     , HasRenderedNames r
     )
  => Vector SPIRVCapability
  -> Sem r ()
renderCaps = renderSPIRVThing "spirvCapabilityRequirements"
                              spirvCapabilityName
                              spirvCapabilityReqs

renderSPIRVThing
  :: ( HasRenderElem r
     , HasRenderParams r
     , HasErr r
     , HasSpecInfo r
     , HasMarshalledStructs r
     , HasRenderedNames r
     )
  => Text
  -> (a -> Text)
  -> (a -> Vector SPIRVRequirement)
  -> Vector a
  -> Sem r ()
renderSPIRVThing funName name reqs xs = do
  tellLanguageExtension (LanguageExtension "TupleSections")
  let case' x = do
        let mergeReqs = Prelude.head -- TODO: this is probably wrong! discarding the
                                     -- other reqs
        (instReqs', devReqs') <- mergeReqs
          <$> traverse renderReq (V.toList (reqs x))
        pure $ viaShow (name x) <+> "-> (,)" <+> align
          (fillSep [align (list instReqs'), align (list devReqs')])
  cases <- (<> ["_ -> ([],[])"]) <$> traverse case' (V.toList xs)
  tellImport ''ByteString
  tellImport (TyConName "Instance")
  tellImport (TyConName "PhysicalDevice")
  tellImportWithAll (mkName "Vulkan.Requirement.DeviceRequirement")
  tellImportWithAll (mkName "Vulkan.Requirement.InstanceRequirement")
  tellExport (ETerm (TermName funName))
  tellDoc $ vsep
    [ pretty funName
      <+> ":: ByteString -> ([InstanceRequirement], [DeviceRequirement])"
    , pretty funName <+> "= \\case" <> line <> indent 2 (vsep cases)
    ]

renderReq
  :: ( HasRenderParams r
     , HasRenderElem r
     , HasErr r
     , HasSpecInfo r
     , HasMarshalledStructs r
     , HasRenderedNames r
     )
  => SPIRVRequirement
  -> Sem r ([Doc ()], [Doc ()])
renderReq = \case
  SPIRVReqVersion v -> (\(iReq, devReq) -> ([iReq], [devReq])) <$> versionReq v

  SPIRVReqExtension p -> minVersionAndExtensionsReqs (V.singleton p)

  SPIRVReqFeature s f rs -> do
    RenderParams {..} <- input
    tellImportWithAll (mkName "Vulkan.Requirement.DeviceRequirement")
    tellImportWithAll (mkName "Vulkan.Requirement.InstanceRequirement")
    tellLanguageExtension (LanguageExtension "OverloadedLists")
    sTy <- cToHsType DoNotPreserve (TypeName s)
    -- TODO: this is pretty lazy, import the accessors properly
    traverse_ tellImportWithAll (allTypeNames sTy)
    (otherInstReqs, otherDevReqs) <- minVersionAndExtensionsReqs rs
    getStruct                     <- input
    oneMember                     <- case getStruct s of
      Nothing -> throw $ "SPIRV features " <> show s <> " aren't in a struct"
      Just str ->
        pure
          . null
          . drop 1
          $ [ ()
            | m <- toList $ msMembers @AStruct str
            , not $ isElided (msmScheme m)
            ]
    resolveAlias <- getResolveAlias
    let featureMemberName = resolveAlias $ mkMemberName s f
    let con               = resolveAlias $ mkConName s s
    let xs =
          [ ("featureName", viaShow f)
          , ( "checkFeature"
            , "\\"
              <>  pretty con
              <>  "{"
              <>  pretty featureMemberName
              <>  "} ->"
              <+> pretty featureMemberName
            )
          , if oneMember
            then
              ( "enableFeature"
              , "\\_ ->"
              <+> pretty con
              <>  "{"
              <>  pretty featureMemberName
              <+> "= True}"
              )
            else
              ( "enableFeature"
              , "\\"
              <>  pretty con
              <>  "{..} ->"
              <+> pretty con
              <>  "{"
              <>  pretty featureMemberName
              <+> "= True, ..}"
              )
          ]
    pure
      ( otherInstReqs
      , "RequireDeviceFeature" <> braceAssignmentList xs : otherDevReqs
      )

  SPIRVReqProperty p m v rs -> do
    RenderParams {..} <- input
    tellImportWithAll (mkName "Vulkan.Requirement.DeviceRequirement")
    tellImportWithAll (mkName "Vulkan.Requirement.InstanceRequirement")
    tellLanguageExtension (LanguageExtension "OverloadedLists")
    -- TODO: this is pretty lazy, import the accessors properly
    sTy <- cToHsType DoNotPreserve (TypeName p)
    traverse_ tellImportWithAll (allTypeNames sTy)
    sTyDoc <- renderType sTy
    let propertyMemberName = mkMemberName p m
        propertyValueName  = mkPatternName v
    (otherInstReqs, otherDevReqs) <- minVersionAndExtensionsReqs rs
    -- TODO, do this properly
    checker                       <- if
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
    pure
      ( otherInstReqs
      , "RequireDeviceProperty" <> braceAssignmentList xs : otherDevReqs
      )

minVersionAndExtensionsReqs
  :: (HasRenderParams r, HasRenderElem r, HasErr r, HasSpecInfo r)
  => Vector Text
  -> Sem r ([Doc ()], [Doc ()])
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
    ( maybe
      id
      ((:) . fst)
      v
      [ "RequireInstanceExtension" <> braceAssignmentList
          [ ("instanceExtensionLayerName" , "Nothing")
          , ("instanceExtensionName"      , e)
          , ("instanceExtensionMinVersion", v)
          ]
      | (e, v) <- instanceExtensions
      ]
    , maybe
      id
      ((:) . snd)
      v
      [ "RequireDeviceExtension" <> braceAssignmentList
          [ ("deviceExtensionLayerName" , "Nothing")
          , ("deviceExtensionName"      , e)
          , ("deviceExtensionMinVersion", v)
          ]
      | (e, v) <- deviceExtensions
      ]
    )

extensionNamePattern
  :: (HasRenderElem r, HasRenderParams r, HasErr r, HasSpecInfo r)
  => Text
  -> Sem r RequireType
extensionNamePattern p = do
  SpecInfo {..} <- input
  case parseVersion p of
    Just v  -> pure . RequireVersion $ v
    Nothing -> do
      (namePattern, _versionPattern) <- extensionPatterns p
      tellImport namePattern
      case siExtensionType p of
        Just DeviceExtension -> pure $ RequireDeviceExtension
          (pretty namePattern)
          "0"
        Just InstanceExtension -> pure $ RequireInstanceExtension
          (pretty namePattern)
          "0"
        Just UnknownExtensionType ->
          throw $ "Dependency on extension of unknown type: " <> show p
        Nothing -> throw $ "Dependency on unknown extension" <> show p

data RequireType
  = RequireVersion Version
  | RequireInstanceExtension (Doc ()) (Doc ())
  | RequireDeviceExtension (Doc ()) (Doc ())
  -- ^ _EXTENSION_NAME and _SPEC_VERSION

versionReq
  :: (HasRenderParams r, HasRenderElem r, HasErr r)
  => Version
  -> Sem r (Doc (), Doc ())
versionReq v = do
  tellImportWithAll (mkName "Vulkan.Requirement.DeviceRequirement")
  tellImportWithAll (mkName "Vulkan.Requirement.InstanceRequirement")
  vDoc <- versionDoc v
  pure
    ("RequireInstanceVersion $" <+> vDoc, "RequireDeviceVersion   $" <+> vDoc)

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
braceAssignmentList =
  encloseSep "{" "}" ", " . fmap (\(l, r) -> l <+> "=" <+> r)
