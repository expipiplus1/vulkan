{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Write.WriteMonad where

import Control.Monad.Writer
import Data.HashSet as S
import Data.Hashable
import GHC.Generics(Generic)
import Write.Utils

data RequiredName = ExternalName ModuleName String
                  | InternalName String
  deriving(Eq, Generic)

instance Hashable RequiredName

type Write = Writer (HashSet RequiredName)

runWrite :: Write a -> (a, HashSet RequiredName)
runWrite = runWriter

tellSingle :: (Eq a, Hashable a, Monad m) => a -> WriterT (HashSet a) m ()
tellSingle = tell . S.singleton

