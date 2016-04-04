module Spec.ExtensionTag
  ( ExtensionTag
  , unExtensionTag
  , stringToExtensionTag
  , appendTag
  ) where

import           Data.Char (isAsciiUpper)

-- | A string containing only upper case ASCII characters
newtype ExtensionTag = ExtensionTag{ unExtensionTag :: String }
  deriving (Eq, Show)

stringToExtensionTag :: String -> Maybe ExtensionTag
stringToExtensionTag s = if all isAsciiUpper s && not (null s)
                           then Just $ ExtensionTag s
                           else Nothing

appendTag :: String -> ExtensionTag -> String
appendTag s t = s ++ unExtensionTag t
