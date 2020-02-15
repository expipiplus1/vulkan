{-# language Strict #-}
{-# language RecursiveDo #-}

module Store3 where

-- import Control.Monad.Fix
-- import Data.Function(fix)
import Control.Monad.Identity

-- -- bar :: Identity ((), ())
-- -- bar = mfix go

-- -- go ~(_, _) = do
-- --   pure ((), ())

-- bar :: Identity Int
-- bar = mdo { ~i <- pure i; pure 1 } :: Identity Int

foo :: Identity Int
foo = do
  rec ~ ~i <- pure i
  pure 1

bar' :: Identity Int
bar' = do
  rec ~ ~_ <- pure ()
  pure 1

-- bar'' :: Identity Int
-- bar'' = fmap snd . mfix $ \(~ ~(~i, ~_)) -> do
--   ~i <- pure i
--   pure (i, 1)

-- bar''' :: Identity Int
-- bar''' = do
--   mfix pure
--   pure 1

-- foo :: ()
-- foo = (\(~ ~(_, _)) -> ()) (error "hello")
