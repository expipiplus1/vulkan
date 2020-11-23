{-# LANGUAGE QuasiQuotes #-}

module VK.SPIRVElements
  ( renderSPIRVElements
  ) where

import qualified Data.Text                     as T
import           Data.Text.Prettyprint.Doc
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Version                   ( Version(versionBranch)
                                                , makeVersion
                                                )
import           Error
import           Polysemy.Input
import           Relude
import           Render.Element
import           Spec.Types
import           Text.InterpolatedString.Perl6.Unindented
import           VkModulePrefix

renderSPIRVElements
  :: (HasErr r, HasRenderParams r)
  => Vector SPIRVExtension
  -> Vector SPIRVCapability
  -> Sem r RenderElement
renderSPIRVElements exts caps = do
  genRe "SPIR-V stuff" $ do
    tellExplicitModule (vulkanModule ["SPIRVInfo"])
    tellCanFormat
    bespokeStuff
    renderExts exts
    renderCaps caps

renderExts
  :: (HasRenderElem r, HasRenderParams r) => Vector SPIRVExtension -> Sem r ()
renderExts = renderSPIRVThing "spirvExtensionRequirements"
                              spirvExtensionName
                              spirvExtensionReqs

renderCaps
  :: (HasRenderElem r, HasRenderParams r) => Vector SPIRVCapability -> Sem r ()
renderCaps = renderSPIRVThing "spirvCapabilityRequirements"
                              spirvCapabilityName
                              spirvCapabilityReqs

renderSPIRVThing
  :: (HasRenderElem r, HasRenderParams r)
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
  tellDoc $ vsep
    [ (pretty funName <+> ":: ByteString -> [SPIRVRequirement]")
    , (pretty funName <+> "= \\case" <> line <> indent 2 (vsep cases))
    ]

renderReq
  :: (HasRenderParams r, HasRenderElem r) => SPIRVRequirement -> Sem r (Doc ())
renderReq = \case
  SPIRVReqVersion v -> do
    tellImport 'makeVersion
    pure $ "Version $ makeVersion" <+> viaShow (versionBranch v)
  SPIRVReqExtension p -> do
    p' <- extensionNamePattern p
    pure $ "Extension" <+> p'
  SPIRVReqFeature s f rs -> do
    rs' <- traverse extensionNamePattern (V.toList rs)
    tellLanguageExtension (LanguageExtension "OverloadedLists")
    pure $ "Feature" <+> viaShow s <+> viaShow f <+> list rs'
  SPIRVReqProperty p m v rs -> do
    rs' <- traverse extensionNamePattern (V.toList rs)
    tellLanguageExtension (LanguageExtension "OverloadedLists")
    pure $ "Property" <+> viaShow p <+> viaShow m <+> viaShow v <+> list rs'

extensionNamePattern
  :: (HasRenderElem r, HasRenderParams r)
  => Text
  -> Sem r (Doc ())
extensionNamePattern p = do
  RenderParams {..} <- input
  if "VK_VERSION_" `T.isPrefixOf` p
    then pure $ viaShow p
    else do
      let nameName = case p of
            -- TODO: Handle these properly
            "VK_INTEL_shader_integer_functions2" ->
              CName "VK_INTEL_SHADER_INTEGER_FUNCTIONS_2_EXTENSION_NAME"
            _ -> CName $ T.toUpper p <> "_EXTENSION_NAME"
      let p' = mkPatternName nameName
      tellImport p'
      pure $ pretty p'

bespokeStuff :: (HasRenderParams r, HasRenderElem r) => Sem r ()
bespokeStuff = do
  tellImport ''ByteString
  tellImport ''Vector
  tellImport ''Version
  tellDoc [qqi|
    data SPIRVRequirement
      = Version Version
      | Extension ByteString
      | Feature ByteString ByteString (Vector ByteString)
      -- ^ Struct, feature, requires
      | Property ByteString ByteString ByteString (Vector ByteString)
      -- ^ Property, member, value, requires
      deriving(Show)
  |]
