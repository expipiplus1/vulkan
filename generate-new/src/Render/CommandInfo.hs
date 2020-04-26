module Render.CommandInfo
  ( RenderedCommandInfo(..)
  , RenderedCommand(..)
  , RenderedBracket(..)
  , BracketCall(..)
  ) where

import           Relude                  hiding ( Type )
import           Data.Vector                    ( Vector )
import           Data.Text.Prettyprint.Doc
import           Language.Haskell.TH            ( Type )
import           Polysemy

import           Error
import           Haskell.Name
import           Render.Element
import           Render.SpecInfo

data RenderedCommandInfo
  = ACommand RenderedCommand
  | ABracket RenderedBracket

data RenderedCommand = RenderedCommand
  { rcName       :: HName
  , rcReturnType :: Type
  , rcArguments  :: Vector Type
  }

data RenderedBracket = RenderedBracket
  { rbName      :: HName
  , rbCreate    :: BracketCall
  , rbDestroy   :: BracketCall
  , rbArguments :: [(Text, Type)]
  }

data BracketCall = BracketCall
  { bcName :: HName
  , bcCall
      :: forall r
       . (HasErr r, HasSpecInfo r, HasRenderParams r, HasRenderElem r)
      => Doc ()
      -> Sem r (Doc ())
  }
