{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parse.Constant
  ( parseConstants
  ) where

import           Parse.Utils
import           Spec.Constant
import           Text.XML.HXT.Core

parseConstants :: IOStateArrow s XmlTree [Constant]
parseConstants = extractFields
  "API Constants"
  (hasAttrValue "name" (== "API Constants"))
  (allChildren constantFailDiag [constantAlias, constant])

constantAlias :: IOStateArrow s XmlTree Constant
constantAlias = proc c -> do
  cName <- requiredAttrValueT "name" -< c
  cValue <- Left . ConstantAlias ^<< getAttrValue0T "alias" -< c
  let cComment = Nothing
  returnA -< Constant{..}

constant :: IOStateArrow s XmlTree Constant
constant = proc c -> do
  cName <- requiredAttrValueT "name" -< c
  cValue <- Right ^<< requiredAttrValueT "value" -< c
  cComment <- optionalAttrValueT "comment" -< c
  returnA -< Constant{..}

constantFailDiag :: IOStateArrow s XmlTree String
constantFailDiag = proc t -> do
  name <- optional (getAttrOrChildText "name") -< t
  returnA -< "Failed to parse constant"
          ++ maybe "" (" named " ++) name
