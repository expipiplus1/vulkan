{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.HeaderVersion
  ( writeHeaderVersion
  ) where

import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Write.Element

writeHeaderVersion :: Word -> WriteElement
writeHeaderVersion version =
  let weDoc = [qci|
          pattern VK_HEADER_VERSION :: Integral a => a
          pattern VK_HEADER_VERSION = {version}
        |]
      weImports = []
      weExtensions = ["PatternSynonyms"]
      weName = "Header Version"
      weProvides = [Pattern "VK_HEADER_VERSION"]
      weDepends = []
  in WriteElement{..}
