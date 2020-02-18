module Render.Params
  where

import           Relude
import           Data.Vector
import           Language.Haskell.TH            ( Name )

data RenderParams = RenderParams
  { mkTyName          :: Text -> Text
  , mkConName         :: Text
                      -- ^ Parent union or enum name
                      -> Text -> Text
  , mkMemberName      :: Text -> Text
  , mkFunName         :: Text -> Text
  , mkParamName       :: Text -> Text
  , mkPatternName     :: Text -> Text
  , mkHandleName      :: Text -> Text
  , mkFuncPointerName :: Text -> Text
    -- ^ Should be distinct from mkTyName
  , alwaysQualifiedNames :: Vector Name
  }

