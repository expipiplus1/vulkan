{-# LANGUAGE AllowAmbiguousTypes #-}

module Render.Stmts
  ( HasStmts
  , Stmt
  , StmtE
  ) where

import           Error
import           Polysemy
import           Polysemy.Fixpoint
import           Polysemy.State

type HasStmts r = (Member Fixpoint r, HasErr r)
type Stmt s (r :: [Effect]) = Sem (StmtE s r ': r)
type StmtE s (r :: [Effect]) = State (ActionsState s r) :: Effect
data ActionsState s (r :: [Effect])
