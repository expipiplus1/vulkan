module Render.Element
  where

import           Relude                  hiding ( runState
                                                , State
                                                , modify'
                                                )
import           Data.Vector.Extra             as V
import           Data.Text                     as T
import           Data.Text.IO                  as T
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
  , exportWithAll   :: Bool
  , exportIsPattern :: Bool
  , exportWith      :: Vector Export
  }
  deriving (Show, Eq, Ord)

pattern ETerm :: Text -> Export
pattern ETerm n = Export n False False Empty

pattern EPat :: Text -> Export
pattern EPat n = Export n False True Empty

pattern EData :: Text -> Export
pattern EData n = Export n True False Empty

exportDoc :: Export -> Doc ()
exportDoc Export {..} =
  let subExports = if V.null exportWith && not exportWithAll
        then ""
        else parenList
          (  (exportDoc . (\e -> e { exportIsPattern = False }) <$> exportWith)
              -- no need to specify "pattern" for sub exports
          <> (if exportWithAll then V.singleton ".." else V.empty)
          )
  in  (if exportIsPattern then ("pattern" <+>) else id)
        . (<> subExports)
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

