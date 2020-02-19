module Render.Params
  where

import           Relude hiding (Reader)
import           Data.Vector
import           Polysemy
import           Polysemy.Reader
import           Language.Haskell.TH            ( Name )

type HasRenderParams r = MemberWithError (Reader RenderParams) r

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
  , mkFuncPointerMemberName :: Text -> Text
    -- ^ The name of function pointer members in the dynamic collection
  , alwaysQualifiedNames :: Vector Name
  }

