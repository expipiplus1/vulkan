{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Bitmask
  ( writeBitmask
  ) where

import           Data.Maybe
import           Data.Text.Prettyprint.Doc
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Bitmask
import           Spec.Type                                (BitmaskType (..))
import           Write.Element
import           Write.Util

writeBitmask :: BitmaskType -> Bitmask -> WriteElement
writeBitmask bmt bm =
  let weDoc = bitmaskDoc bmt bm
      weExtensions = []
      weImports = []
  in WriteElement {..}

bitmaskDoc :: BitmaskType -> Bitmask -> Doc ()
bitmaskDoc BitmaskType{..} Bitmask{..} = [qci|
  bmtName: {bmtName}
  bmtType: {bmtType}
  bmName: {bmName}
|]
