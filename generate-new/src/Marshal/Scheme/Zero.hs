module Marshal.Scheme.Zero where

import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.NonDet
import           Relude

import           Error
import           Haskell.Name
import           Marshal.Scheme
import           Render.Element

zeroScheme
  :: (HasRenderElem r, HasRenderParams r, HasErr r)
  => MarshalScheme a
  -> Sem r (Maybe (Doc ()))
zeroScheme = runNonDetMaybe . go
 where
  go = \case
    Unit              -> pure "()"
    Preserve _        -> pure "zero"
    Normal   _        -> pure "zero"
    Length{}          -> pure "zero"
    ElidedLength{}    -> empty
    ElidedUnivalued _ -> empty
    ElidedVoid        -> empty
    VoidPtr           -> pure "zero"
    ByteString        -> pure "mempty"
    Maybe _           -> pure "Nothing"
    Vector _ _        -> pure "mempty"
    EitherWord32 _    -> pure $ parens "Left 0"
    Tupled n s        -> do
      z <- go s
      pure $ tupled (replicate (fromIntegral n) z)
    Returned      _ -> empty
    InOutCount    _ -> empty
    WrappedStruct _ -> do
      tellImportWithAll (TyConName "SomeStruct")
      pure $ parens "SomeStruct zero"
    WrappedChildStruct _ -> throw "Unable to get a zero inheriting struct"
    Custom CustomScheme {..} -> maybe empty pure csZero
    ElidedCustom _ -> empty

