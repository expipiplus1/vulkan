module Main where

import           Data.Text.Extra                ( (<+>) )
import           Data.Version
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
import           Say
import           System.TimeIt
import           Text.Show.Pretty

import           Bespoke                        ( assignBespokeModules )
import           Bespoke.MarshalParams
import           Bespoke.RenderParams
import           Documentation.All
import           Error
import           Marshal
import           Render.Aggregate
import           Render.Element.Write
import           Render.Names
import           Render.SpecInfo
import           Spec.Parse

import           Render.State                   ( initialRenderState )

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

    liftIO $ pPrint spec

    error "TODO"
