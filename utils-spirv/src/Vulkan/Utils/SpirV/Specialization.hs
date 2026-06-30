{-| Build Vulkan specialization-constant values from a reflected 'Module'.

Reflection yields each constant's @constant_id@ and name but not its type or
width, and the value side (the 'Specialization' class) is a stack of 'Word32's —
so only 32-bit scalar constants (@int@\/@uint@\/@float@\/@bool@, the
@GL_KHR_vulkan_glsl@ baseline) are supported, one 4-byte slot each.
'specializationMapEntries' lays them out in ascending @constant_id@ order (the
ids need not be contiguous). Wider constants are out of scope (see
@spirv-cleanup.md@); a 64-bit value supplied as two 'Word32's trips 'packed'\'s
arity check rather than being silently truncated.

Supply the values via the 'Specialization' class from
"Vulkan.Utils.Pipeline.Specialization", in ascending @constant_id@ order. (That
module shares the 32-bit limit and additionally assumes @constantID = offset \/ 4@.)
-}
module Vulkan.Utils.SpirV.Specialization
  ( specializationConstants
  , specializationMapEntries
  , withSpecializationInfo
  , allocateSpecializationInfo
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import Control.Monad.Trans.Resource (MonadResource, allocate)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word32)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr)
import Vulkan.Core10 qualified as Vk
import Vulkan.Utils.Pipeline.Specialization (Specialization (..))

import Data.SpirV.Reflect.Module (Module)
import Data.SpirV.Reflect.Module qualified as Module
import Data.SpirV.Reflect.SpecializationConstant qualified as SC

-- | Reflected specialization constants as @(constant_id, name)@, ascending by id.
specializationConstants :: Module -> [(Word32, Maybe Text)]
specializationConstants m =
  sortOn fst [(scId c, scName c) | c <- V.toList (moduleSpecConstants m)]

{- | One tightly-packed 32-bit map entry per reflected spec constant, ascending
by @constant_id@ (offset @= index * 4@, size 4).
-}
specializationMapEntries :: Module -> Vector Vk.SpecializationMapEntry
specializationMapEntries m =
  V.fromList
    [ Vk.SpecializationMapEntry
        { Vk.constantID = cid
        , Vk.offset = fromIntegral (ix * 4)
        , Vk.size = 4
        }
    | (ix, (cid, _)) <- zip [0 :: Int ..] (specializationConstants m)
    ]

{- | Pack a 'Specialization' against the reflected map entries, yielding a
'Vk.SpecializationInfo' valid for the callback's duration (pipeline creation
copies the values out — build and create the pipeline inside the callback).
'Nothing' when the shader declares no spec constants.

The supplied values must be in ascending @constant_id@ order and number the
shader's declared constants; a mismatch is a programmer error and 'error's.
-}
withSpecializationInfo
  :: (Specialization spec, MonadUnliftIO m)
  => Module
  -> spec
  -> (Maybe Vk.SpecializationInfo -> m a)
  -> m a
withSpecializationInfo m spec action =
  case packed m spec of
    Nothing -> action Nothing
    Just (entries, ws) ->
      withRunInIO $ \run ->
        VS.unsafeWith (VS.fromList ws) $ \p ->
          run . action $ Just (specInfo entries (length ws) (castPtr p))

{- | As 'withSpecializationInfo', but the backing buffer lives until the
surrounding 'Control.Monad.Trans.Resource.ResourceT' scope ends rather than a
callback — so it survives a later pipeline creation. 'Nothing' when the shader
declares no spec constants.
-}
allocateSpecializationInfo
  :: (Specialization spec, MonadResource m)
  => Module
  -> spec
  -> m (Maybe Vk.SpecializationInfo)
allocateSpecializationInfo m spec =
  case packed m spec of
    Nothing -> pure Nothing
    Just (entries, ws) -> do
      let n = length ws
      -- A C-malloc'd buffer is pointer-stable and freed at scope end; the
      -- pointer must stay valid until pipeline creation copies the values.
      (_key, ptr) <- allocate (mallocArray n) free
      liftIO $ pokeArray ptr ws
      pure $ Just (specInfo entries n (castPtr ptr))

{- | Reflected map entries paired with the caller's packed values, or 'Nothing'
when there are no spec constants. 'error's on an arity mismatch.
-}
packed
  :: (Specialization spec)
  => Module
  -> spec
  -> Maybe (Vector Vk.SpecializationMapEntry, [Word32])
packed m spec
  | V.null entries = Nothing
  | length ws /= n =
      error $
        "Vulkan.Utils.SpirV.Specialization: shader declares "
          <> show n
          <> " specialization constant(s) but "
          <> show (length ws)
          <> " value(s) were supplied"
  | otherwise = Just (entries, ws)
  where
    entries = specializationMapEntries m
    n = V.length entries
    ws = specializationData spec

specInfo :: Vector Vk.SpecializationMapEntry -> Int -> Ptr () -> Vk.SpecializationInfo
specInfo entries n p =
  Vk.SpecializationInfo
    { Vk.mapEntries = entries
    , Vk.dataSize = fromIntegral (n * 4)
    , Vk.data' = p
    }

-- Field accessors via record patterns (the constructor disambiguates the
-- DuplicateRecordFields names).

moduleSpecConstants :: Module -> Vector SC.SpecializationConstant
moduleSpecConstants Module.Module{Module.spec_constants = cs} = cs

scId :: SC.SpecializationConstant -> Word32
scId SC.SpecializationConstant{SC.constant_id = i} = i

scName :: SC.SpecializationConstant -> Maybe Text
scName SC.SpecializationConstant{SC.name = n} = n
