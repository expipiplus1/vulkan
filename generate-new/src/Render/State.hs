module Render.State where

import qualified Data.Set                      as Set
import           Error
import           Haskell.Name                   ( HName )
import           Polysemy
import           Polysemy.State
import           Relude                  hiding ( State
                                                , gets
                                                , modify'
                                                )

type HasRenderState r = Member (State RenderState) r

newtype RenderState = RenderState
  { storableStructOrUnionSet :: Set.Set HName
  }

initialRenderState :: RenderState
initialRenderState = RenderState mempty

declareStorable :: HasRenderState r => HName -> Sem r ()
declareStorable n = modify'
  (\r ->
    r { storableStructOrUnionSet = Set.insert n (storableStructOrUnionSet r) }
  )

isStorableStructOrUnion :: (HasErr r, HasRenderState r) => HName -> Sem r Bool
isStorableStructOrUnion n = do
  m <- gets storableStructOrUnionSet
  pure (Set.member n m)
