module Render.SpecInfo
  where

import           Relude                  hiding ( Reader
                                                , asks
                                                , runReader
                                                )
import qualified Data.Map                      as Map
import           Polysemy.Reader
import           Polysemy

import           Spec.Types

type HasSpecInfo r = MemberWithError (Reader SpecInfo) r

data SpecInfo = SpecInfo
  { siIsUnion :: Text -> Maybe Union
  , siIsStruct :: Text -> Maybe Struct
  }

withSpecInfo :: Spec -> Sem (Reader SpecInfo ': r) a -> Sem r a
withSpecInfo Spec {..} r = do
  let mkLookup n f =
        let m = Map.fromList [ (n s, s) | s <- toList f ] in (`Map.lookup` m)
      siIsUnion  = mkLookup sName specUnions
      siIsStruct = mkLookup sName specStructs
  runReader SpecInfo { .. } r

getStruct :: HasSpecInfo r => Text -> Sem r (Maybe Struct)
getStruct t = ($ t) <$> asks siIsStruct

getUnion :: HasSpecInfo r => Text -> Sem r (Maybe Union)
getUnion t = ($ t) <$> asks siIsUnion
