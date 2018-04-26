module Write.Marshal.Wrap
  ( Wrapper
  , foldWrappers
  ) where

import           Data.Foldable
import           Data.Monoid               (Endo (..))
import           Data.Text.Prettyprint.Doc
import           Prelude                   hiding (Enum)

type Wrapper = (Doc () -> Doc ()) -> Doc () -> Doc ()

foldWrappers :: [Wrapper] -> Wrapper
foldWrappers = appEndo . fold . fmap Endo
