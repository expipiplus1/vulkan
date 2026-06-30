{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-| A 64-bit buffer device address — a GPU pointer to a @buffer_reference@ /
@PhysicalStorageBuffer@ block.

A @buffer_reference@ member of a shader block is stored as an 8-byte address, not
the pointee inline, so reflection-driven codegen maps it to a 'DeviceAddress'
field. The phantom @a@ records the pointee's generated record type (e.g.
@DeviceAddress Node@), purely for documentation and type-safety at the call site;
the runtime representation is just the 'Word64' address.

The 'Graphics.Gl.Block.Block' instance lays it out as a single 8-byte scalar
(alignment 8) under both std140 and std430 — matching @VkDeviceAddress@ — so a
generated record carrying one gets the right offsets via @deriving Storable via
(Std430 …)@.
-}
module Vulkan.Utils.SpirV.DeviceAddress
  ( DeviceAddress (..)
  ) where

import Data.Word (Word64)
import Foreign.Ptr.Diff (peekDiffOff, pokeDiffOff)
import Foreign.Storable (Storable)
import Graphics.Gl.Block (Block (..))

-- | A device address pointing at a @buffer_reference@ block of type @a@.
newtype DeviceAddress a = DeviceAddress Word64
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

{- | One 8-byte scalar (alignment 8) under both layouts; mirrors the @Double@
scalar instance gl-block ships, since @a@ is phantom.
-}
instance Block (DeviceAddress a) where
  type PackedSize (DeviceAddress a) = 8
  alignment140 _ = 8
  sizeOf140 = sizeOfPacked
  alignment430 = alignment140
  sizeOf430 = sizeOf140
  isStruct _ = False
  read140 = peekDiffOff
  write140 = pokeDiffOff
  read430 = read140
  write430 = write140
  readPacked = read140
  writePacked = write140
