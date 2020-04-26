-- Build Your Own Bindings
module BYOB
  ( CommandManifest(..)
  , Located(..)
  , byob
  ) where

import           Relude                  hiding ( Type )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Text.Prettyprint.Doc
import           Polysemy

import           Control.Monad.Trans.Resource

import           Error
import           Haskell
import           Render.Element
import           Render.Element.Write
import           Render.Names
import           Render.SpecInfo
import           Write.Segment
import           Render.CommandInfo
import           Render.ImportLocation

data Located a = Located ModName a

data CommandManifest = CommandManifest
  { cmCommands :: Vector (Located RenderedCommand)
  , cmBrackets :: Vector (Located RenderedBracket)
  }

instance Semigroup CommandManifest where
  c1 <> c2 = CommandManifest (((<>) `on` cmCommands) c1 c2)
                             (((<>) `on` cmBrackets) c1 c2)

instance Monoid CommandManifest where
  mempty = CommandManifest mempty mempty

byob
  :: ( HasErr r
     , Member (Embed IO) r
     , HasTypeInfo r
     , HasRenderedNames r
     , HasRenderParams r
     , HasSpecInfo r
     )
  => FilePath
  -> ImportLocation
  -> CommandManifest
  -> Sem r ()
byob out il CommandManifest {..} = do
  es <- forV cmBrackets $ \l@(Located (ModName m) _) ->
    Segment (ModName (m <> ".MTL")) . V.singleton <$> renderAltBracket l
  let getDocumentation = const Nothing
  renderSegments getDocumentation out il (toList es)

renderAltBracket
  :: (HasErr r, HasRenderParams r, HasSpecInfo r)
  => Located RenderedBracket
  -> Sem r RenderElement
renderAltBracket (Located (ModName m) RenderedBracket {..}) =
  genRe (unName rbName) $ do
    tellImport 'allocate
    tellQualImport (bcName rbCreate)
    tellQualImport (bcName rbDestroy)
    tellExport (ETerm (bcName rbCreate))
    tellReexportModHiding (ModName m) [bcName rbCreate, bcName rbDestroy]
    create <- bcCall
      rbCreate
      (pretty $ TermName (m <> "." <> unName (bcName rbCreate)))
    destroy <- bcCall
      rbDestroy
      (pretty $ TermName (m <> "." <> unName (bcName rbDestroy)))
    tellDoc
      $   pretty (bcName rbCreate)
      <+> sep (pretty . fst <$> rbArguments)
      <+> "= allocate"
      <>  line
      <>  indent 2 (vsep [parens create, parens destroy])
