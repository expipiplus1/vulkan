{-# LANGUAGE AllowAmbiguousTypes #-}

module Render.Stmts
  ( HasStmts
  , Stmt
  , StmtE
  ) where

import           Polysemy
import           Polysemy.State
import           Polysemy.Fixpoint
import           Error

type HasStmts r = (Member Fixpoint r, HasErr r)
type Stmt s (r :: [Effect]) = Sem (StmtE s r ': r)
type StmtE s (r :: [Effect]) = State (ActionsState s r) :: Effect
data ActionsState s (r :: [Effect])
