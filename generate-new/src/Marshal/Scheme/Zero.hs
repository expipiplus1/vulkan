module Marshal.Scheme.Zero
  where

import           Relude
import           Polysemy
import           Polysemy.NonDet
import           Data.Text.Prettyprint.Doc

import           Marshal.Scheme
import           Render.Element
import           Haskell.Name

zeroScheme
  :: (HasRenderElem r, HasRenderParams r)
  => MarshalScheme a
  -> Sem r (Maybe (Doc ()))
zeroScheme = runNonDetMaybe . go
 where
  go = \case
    Unit              -> pure "()"
    Preserve _        -> pure "zero"
    Normal   _        -> pure "zero"
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
    Custom       CustomScheme {..} -> maybe empty pure csZero
    ElidedCustom _                 -> empty

