{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Write.WriteMonad
  ( Write
  , WriteOutput
  , ReadInput
  , RequiredName(..)
  , WildCard(..)
  , FileType(..)
  , runWrite
  , askTypeEnv
  , isBoot
  , tellRequiredName
  , tellRequiredNames
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

type WriteOutput = (HashSet RequiredName, HashSet ExtensionName)

type ReadInput = (TypeEnv, FileType)

type Write = ReaderT ReadInput (Writer WriteOutput)

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

tellRequiredName :: MonadWriter WriteOutput m  => RequiredName -> m ()
tellRequiredName rn = tell (S.singleton rn, mempty)

tellRequiredNames :: [RequiredName] -> Write ()
tellRequiredNames rns = tell (S.fromList rns, mempty)

tellExtension :: ExtensionName -> Write ()
tellExtension en = tell (mempty, S.singleton en)

