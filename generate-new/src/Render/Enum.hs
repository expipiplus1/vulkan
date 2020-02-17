{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
module Render.Enum
  where

import           Text.Printf
import           Prelude                        ( Show(..) )
import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Text.InterpolatedString.Perl6.Unindented
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import qualified Data.Vector                   as V

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Type

renderEnum
  :: (HasErr r, Member (Reader RenderParams) r) => Enum' -> Sem r RenderElement
renderEnum Enum {..} = do
  RenderParams {..} <- ask
  genRe ("enum " <> eName) $ do
    let n       = mkTyName eName
        conName = mkConName eName
        innerTy = case eType of
          AnEnum   -> ConT ''Int32
          ABitmask -> ConT ''Word32
    (patterns, patternExports) <- V.unzip <$> traverseV (renderEnumValue conName eType) eValues
    tellExport (Export n True False patternExports)
    tellDoc [qqi|
        newtype {n} = {conName} {renderType innerTy}
        {vsep (toList patterns)}
        |]

renderEnumValue
  :: (HasErr r, Member (Reader RenderParams) r)
  => Text
  -- ^ Constructor name
  -> EnumType
  -> EnumValue
  -> Sem r (Doc (), Export)
renderEnumValue conName enumType EnumValue {..} = do
  RenderParams {..} <- ask
  let n = mkPatternName evName
      v = case enumType of
            AnEnum -> showsPrec 9 evValue ""
            ABitmask -> printf "0x%08x" evValue
  pure
    ( "pattern"
    <+> pretty n
    <+> "="
    <+> pretty conName
    <+> pretty v
    , EPat n
    )
