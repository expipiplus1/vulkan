{-# language AllowAmbiguousTypes #-}
module Render.Peek
  where

import           Data.Vector                    ( Vector )
import           Relude                  hiding ( Const
                                                , State
                                                , Type
                                                , init
                                                , last
                                                )
import           Data.Text.Prettyprint.Doc

import           Error
import           Marshal.Marshalable
import           Render.Element
import           Render.SpecInfo
import           Render.Stmts
import           Render.Stmts.Poke

type Lengths = Vector ParameterLength

getLenRef
  :: forall a r s
   . ( HasErr r
     , HasRenderElem r
     , HasRenderParams r
     , HasSpecInfo r
     , HasSiblingInfo a r
     , Show a
     )
  => Lengths
  -> Stmt s r (Ref s ValueDoc)
getLenRef lengths = do
  -- The ValueDoc line fails, the Doc() line doesn't error
  stmt @ValueDoc Nothing Nothing undefined
  -- stmt @(Doc ()) Nothing Nothing undefined
