module Spec.ExtensionTag
  ( ExtensionTag(..)
  , stringToExtensionTag
  ) where

import           Data.Char      (isAsciiUpper)
import           Data.Text      (Text, pack)

-- | A string containing only upper case ASCII characters
newtype ExtensionTag = ExtensionTag{ unExtensionTag :: Text }
  deriving (Eq, Show)

stringToExtensionTag :: String -> Maybe ExtensionTag
stringToExtensionTag s = if all isAsciiUpper s && not (null s)
                           then Just $ ExtensionTag (pack s)
                           else Nothing
