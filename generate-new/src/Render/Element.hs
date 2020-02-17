module Render.Element
  where

import           Relude hiding (runState, State, modify')
import           Data.Vector as V
import           Data.Text as T
import           Data.Text.IO as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Polysemy
import           Polysemy.State

import           Render.Utils

data RenderElement = RenderElement
  { reName    :: Text
  , reDoc     :: Doc ()
  , reExports :: Vector Export
  }

data Export = Export
  { exportName      :: Text
  , exportWith      :: Bool
  , exportIsPattern :: Bool
  }
  deriving (Show, Eq, Ord)

pattern ETerm :: Text -> Export
pattern ETerm n = Export n False False

pattern EPat :: Text -> Export
pattern EPat n = Export n False True

pattern EData :: Text -> Export
pattern EData n = Export n True False

exportDoc :: Export -> Doc ()
exportDoc Export {..} =
  (if exportIsPattern then ("pattern" <+>) else id)
    . (if exportWith then (<> "(..)") else id)
    $ pretty exportName

----------------------------------------------------------------
-- Generating RenderElements
----------------------------------------------------------------

genRe :: Text -> Sem (State RenderElement : r) () -> Sem r RenderElement
genRe n m = do
  (o, _) <- runState
    RenderElement { reName = n, reDoc = mempty, reExports = mempty }
    m
  pure o

tellExport :: MemberWithError (State RenderElement) r => Export -> Sem r ()
tellExport e = modify' (\r -> r { reExports = reExports r <> V.singleton e })

tellDoc :: MemberWithError (State RenderElement) r => Doc () -> Sem r ()
tellDoc d = modify' (\r -> r { reDoc = reDoc r <> hardline <> d })

----------------------------------------------------------------
-- Rendering
----------------------------------------------------------------

renderModule :: FilePath -> Vector Text -> Vector RenderElement -> IO ()
renderModule p modulePath es =
  let f = p <> "/" <> T.unpack
        (T.intercalate "/" (V.toList modulePath) <> ".hs")
  in  withFile f WriteMode $ \h -> T.hPutStr h $ renderStrict
        (layoutPretty defaultLayoutOptions { layoutPageWidth = Unbounded }
                      contents
        )
 where
  modName = T.intercalate "." (V.toList modulePath)
  contents =
    vsep
      $   "module"
      <+> pretty modName
      <>  line
      <>  indent 2 (parenList (exportDoc <$> V.concatMap reExports es))
      <+> "where"
      :   V.toList (reDoc <$> es)

