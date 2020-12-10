module Main where

import           Bespoke                        ( assignBespokeModules )
import           Bespoke.MarshalParams
import           Data.Text.Extra                ( (<+>) )
import           Data.Version
import           Documentation.All
import           Error
import           Khronos.AssignModules
import           Khronos.Render
import           Marshal
import           Polysemy
import           Polysemy.Fixpoint
import           Polysemy.Input
import           Polysemy.State
import           Relude                  hiding ( Handle
                                                , State
                                                , Type
                                                , evalState
                                                , uncons
                                                )
import           Render.Aggregate
import           Render.Element.Write
import           Render.Names
import           Render.SpecInfo
import           Render.State                   ( initialRenderState )
import           Say
import           Spec.Parse
import           System.TimeIt
import           Text.Show.Pretty
import           XR.RenderParams

main :: IO ()
main =
  (runFinal . embedToFinal @IO . fixpointToFinal @IO . runErr $ go) >>= \case
    Left es -> do
      traverse_ sayErr es
      sayErr (show (length es) <+> "errors")
      exitFailure
    Right () -> pure ()
 where
  go :: Sem '[Err , Fixpoint , Embed IO , Final IO] ()
  go = do
    specText <- timeItNamed "Reading spec"
      $ readFileBS "./OpenXR-Docs/specification/registry/xr.xml"

    (spec@Spec {..}, getSize) <- timeItNamed "Parsing spec"
      $ parseSpec @SpecXr specText

    let allExtensionNames =
          toList (exName <$> specExtensions)
            <> [ "XR_VERSION_" <> show major <> "_" <> show minor
               | Feature {..}      <- toList specFeatures
               , major : minor : _ <- pure $ versionBranch fVersion
               ]
        doLoadDocs = False
    getDocumentation <- if doLoadDocs
      then liftIO $ loadAllDocumentation allExtensionNames
                                         "./OpenXR-Docs/specification"
                                         "./OpenXR-Docs/specification/man"
      else pure (const Nothing)

    runInputConst (renderParams specHandles)
      . withRenderedNames spec
      . withSpecInfo spec getSize
      . withTypeInfo spec
      $ do

          mps          <- marshalParams spec

          (ss, us, cs) <- runInputConst mps $ do
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
            timeItNamed "Rendering" $ traverse evaluateWHNF =<< evalStateIO
              initialRenderState
              (renderSpec spec getDocumentation brackets ss us cs)

          groups <-
            timeItNamed "Segmenting"
            $   assignModules spec
            =<< assignBespokeModules renderElements

          timeItNamed "writing"
            $   renderSegments getDocumentation "out-xr"
            =<< mergeElements groups

------------------------------------------------------------------
----
------------------------------------------------------------------

evalStateIO :: Member (Embed IO) r => s -> Sem (State s ': r) a -> Sem r a
evalStateIO i = fmap snd . stateToIO i


----------------------------------------------------------------
-- Todo
----------------------------------------------------------------

brackets _ _ = pure mempty
