{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Write.WriteMonad
  ( Write
  , RequiredName(..)
  , WildCard(..)
  , FileType(..)
  , runWrite
  , askTypeEnv
  , isBoot
  , tellRequiredName
  , tellExtension
  , requireStorable
  , doesDeriveStorable
  ) where

import Control.Monad.Writer
import Control.Monad.Reader
import Data.HashSet as S
import Data.Hashable
import GHC.Generics(Generic)
import Write.Utils
import Spec.TypeEnv

data RequiredName = ExternalName ModuleName String
                  | InternalName WildCard String
  deriving(Eq, Generic)

data WildCard = WildCard
              | NoWildCard
  deriving(Eq, Generic)

instance Hashable WildCard

instance Hashable RequiredName

data FileType = Normal
              | Boot
  deriving(Eq)

type ExtensionName = String

type Write = ReaderT (TypeEnv, FileType) (Writer ( HashSet RequiredName
                                         , HashSet ExtensionName
                                         ))

runWrite :: TypeEnv -> FileType -> Write a 
         -> (a, (HashSet RequiredName, HashSet ExtensionName))
runWrite env boot m = runWriter (runReaderT m (env, boot))

askTypeEnv :: Write TypeEnv
askTypeEnv = asks fst

isBoot :: Write Bool
isBoot = asks ((== Boot) . snd)

doesDeriveStorable :: Write ()
doesDeriveStorable = 
  requireStorable >> tellExtension "GeneralizedNewtypeDeriving"

requireStorable :: Write ()
requireStorable = tellRequiredName (ExternalName (ModuleName "Foreign.Storable") "Storable(..)")

tellRequiredName :: RequiredName -> Write ()
tellRequiredName rn = tell (S.singleton rn, mempty)

tellExtension :: ExtensionName -> Write ()
tellExtension en = tell (mempty, S.singleton en)

