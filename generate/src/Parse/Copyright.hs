{-# LANGUAGE OverloadedStrings #-}

module Parse.Copyright
  ( parseCopyright
  ) where

import           Data.List         (isPrefixOf)
import           Parse.Utils
import           Text.XML.HXT.Core

-- The Copyright is the only comment beginning with "Copyright"
parseCopyright :: IOStateArrow s XmlTree String
parseCopyright =
  isA ("Copyright" `isPrefixOf`)
    <<< strip
    ^<< getAllText
    <<< hasName "comment"
