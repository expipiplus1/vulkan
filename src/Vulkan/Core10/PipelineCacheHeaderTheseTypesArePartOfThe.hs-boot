{-# language CPP #-}
-- No documentation found for Chapter "PipelineCacheHeaderTheseTypesArePartOfThe"
module Vulkan.Core10.PipelineCacheHeaderTheseTypesArePartOfThe  ( PipelineCacheHeaderVersionSafetyCriticalOne
                                                                , PipelineCacheSafetyCriticalIndexEntry
                                                                , PipelineCacheStageValidationIndexEntry
                                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PipelineCacheHeaderVersionSafetyCriticalOne

instance ToCStruct PipelineCacheHeaderVersionSafetyCriticalOne
instance Show PipelineCacheHeaderVersionSafetyCriticalOne

instance FromCStruct PipelineCacheHeaderVersionSafetyCriticalOne


data PipelineCacheSafetyCriticalIndexEntry

instance ToCStruct PipelineCacheSafetyCriticalIndexEntry
instance Show PipelineCacheSafetyCriticalIndexEntry

instance FromCStruct PipelineCacheSafetyCriticalIndexEntry


data PipelineCacheStageValidationIndexEntry

instance ToCStruct PipelineCacheStageValidationIndexEntry
instance Show PipelineCacheStageValidationIndexEntry

instance FromCStruct PipelineCacheStageValidationIndexEntry

