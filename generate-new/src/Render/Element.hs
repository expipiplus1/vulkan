module Render.Element
  where

import           Relude                  hiding ( runState
                                                , State
                                                , modify'
                                                )
import           Data.Vector.Extra             as V
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Data.Set                       ( insert, unions )
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import Data.Char (isAlpha)
import           Polysemy
import           Polysemy.State
import           Language.Haskell.TH            ( Name
                                                , nameBase
                                                , nameModule
                                                )

import           Render.Utils

data RenderElement = RenderElement
  { reName    :: Text
  , reDoc     :: Doc ()
  , reExports :: Vector Export
  , reImports :: Set Import
  }

data Export = Export
  { exportName      :: Text
  , exportWithAll   :: Bool
  , exportIsPattern :: Bool
  , exportWith      :: Vector Export
  }
  deriving (Show, Eq, Ord)

data Import = Import
  { importName :: Name
  , importQualified :: Bool
  }
  deriving (Show, Eq, Ord)

pattern ETerm :: Text -> Export
pattern ETerm n = Export n False False Empty

pattern EPat :: Text -> Export
pattern EPat n = Export n False True Empty

pattern EData :: Text -> Export
pattern EData n = Export n True False Empty

pattern EType :: Text -> Export
pattern EType n = Export n False False Empty

exportDoc :: Export -> Doc ()
exportDoc Export {..} =
  let subExports = if V.null exportWith && not exportWithAll
        then ""
        else parenList
          (  (exportDoc . (\e -> e { exportIsPattern = False }) <$> exportWith)
              -- no need to specify "pattern" for sub exports
          <> (if exportWithAll then V.singleton ".." else V.empty)
          )
      isSymbol = not . (\x -> isAlpha x || (x == '_')) . T.head $ exportName
  in  (if exportIsPattern then ("pattern" <+>) else id)
        . (<> subExports)
        . (if isSymbol then parens else id)
        $ pretty exportName

----------------------------------------------------------------
-- Generating RenderElements
----------------------------------------------------------------

genRe :: Text -> Sem (State RenderElement : r) () -> Sem r RenderElement
genRe n m = do
  (o, _) <- runState
    RenderElement { reName    = n
                  , reDoc     = mempty
                  , reExports = mempty
                  , reImports = mempty
                  }
    m
  pure o

tellExport :: MemberWithError (State RenderElement) r => Export -> Sem r ()
tellExport e = modify' (\r -> r { reExports = reExports r <> V.singleton e })

tellDoc :: MemberWithError (State RenderElement) r => Doc () -> Sem r ()
tellDoc d = modify' (\r -> r { reDoc = reDoc r <> hardline <> d })

tellImport :: MemberWithError (State RenderElement) r => Import -> Sem r ()
tellImport e = modify' (\r -> r { reImports = insert e (reImports r) })

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
  imports =
    vsep (renderImport <$> Relude.toList (unions (reImports <$> V.toList es)))
  contents =
    vsep
      $   "{-# language PatternSynonyms #-}"
      :   "module"
      <+> pretty modName
      <>  indent 2 (parenList (exportDoc <$> V.concatMap reExports es))
      <+> "where"
      :   imports
      :   V.toList (reDoc <$> es)

renderImport :: Import -> Doc ()
renderImport (Import n qual) =
  let qualDoc = bool "" " qualified" qual
      mod'    = fromMaybe "UNKNOWN" (nameModule n)
      base    = nameBase n
  in  "import" <> qualDoc <+> pretty mod' <+> parenList
        (V.fromList [pretty base])

