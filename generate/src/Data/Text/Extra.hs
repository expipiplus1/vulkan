module Data.Text.Extra
  ( readMaybe
  , module Data.Text
  ) where

import           Data.Text
import qualified Text.Read as R

readMaybe :: Read a => Text -> Maybe a
readMaybe = R.readMaybe . unpack
