{-# LANGUAGE TemplateHaskell #-}

module InstrumentDecs
  ( withSpan_
  , instrumentDecs
  ) where

import           Control.Monad                  ( replicateM )
import           Data.ByteString                ( ByteString )
import           Language.Haskell.TH
import           OpenTelemetry.Eventlog         ( beginSpan
                                                , endSpan
                                                )
import           UnliftIO                       ( MonadUnliftIO )
import           UnliftIO.Exception             ( bracket )

-- Profiling span
withSpan_ :: MonadUnliftIO m => ByteString -> m c -> m c
withSpan_ n x = bracket (beginSpan n) endSpan (const x)

instrumentDecs :: (Name -> Maybe String) -> [Dec] -> Q [Dec]
instrumentDecs p ds = do
  concat <$> sequenceA
    [ case d of
        FunD n [Clause ps (NormalB o) _] | Just s <- p n -> do
          d' <- instrumentFun s n ps o
          pure [d']
        _ -> pure [d]
    | d <- ds
    ]

instrumentFun :: String -> Name -> [Pat] -> Exp -> Q Dec
instrumentFun s n ps o = do
  let n'     = n
      eArity = \case
        LamE ls e -> length ls + eArity e
        _         -> 0
      arity = length ps + eArity o
  vs <- replicateM arity (newName "x")
  e  <- [|withSpan_ $(litE (StringL s)) $(foldl appE (pure o) (varE <$> vs))|]
  pure $ FunD n' [Clause (VarP <$> vs) (NormalB e) []]
