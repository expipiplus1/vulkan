module Vulkan.Utils.Misc
  ( -- * Sorting things
    partitionOptReq
  , partitionOptReqIO
    -- * Bit Utils
  , showBits
  , (.&&.)
  ) where

import           Control.Monad.IO.Class
import           Data.Bits
import           Data.Foldable
import           Data.List                      ( intercalate
                                                , partition
                                                )
import           Vulkan.Utils.Internal

-- | From a list of things, take all the required things and as many optional
-- things as possible.
partitionOptReq
  :: Eq a
  => [a]
  -- ^ What do we have available
  -> [a]
  -- ^ Optional desired elements
  -> [a]
  -- ^ Required desired elements
  -> ([a], Either [a] [a])
  -- ^ (Missing optional elements, Either (missing required elements) or (all
  -- required elements and as many optional elements as possible)
partitionOptReq available optional required =
  let (optHave, optMissing) = partition (`elem` available) optional
      (reqHave, reqMissing) = partition (`elem` available) required
  in  ( optMissing
      , case reqMissing of
        [] -> Right (reqHave <> optHave)
        xs -> Left xs
      )

-- | Like 'partitionOptReq'.
--
-- Will throw an 'IOError in the case of missing things. Details on missing
-- things will be reported in stderr.
--
-- This is useful in dealing with layers and extensions.
partitionOptReqIO
  :: (Show a, Eq a, MonadIO m)
  => String
  -- ^ What are we sorting (Used for a debug message)
  -> [a]
  -- ^ What do we have available
  -> [a]
  -- ^ Optional desired elements
  -> [a]
  -- ^ Required desired elements
  -> m ([a],[a])
  -- ^ All the required elements and as many optional elements as possible,
  --   as well as the missing optional elements.
partitionOptReqIO type' available optional required = liftIO $ do
  let (optMissing, exts) = partitionOptReq available optional required
  for_ optMissing
    $ \o -> sayErr $ "Missing optional " <> type' <> ": " <> show o
  case exts of
    Left reqMissing -> do
      for_ reqMissing
        $ \r -> sayErr $ "Missing required " <> type' <> ": " <> show r
      noSuchThing $ "Don't have all required " <> type' <> "s"
    Right xs -> pure (xs, optMissing)

----------------------------------------------------------------
-- * Bit utils
----------------------------------------------------------------

-- | Show valies as a union of their individual bits
--
-- >>> showBits @Int 5
-- "1 .|. 4"
--
-- >>> showBits @Int 0
-- "zeroBits"
--
-- >>> import Vulkan.Core10.Enums.QueueFlagBits
-- >>> showBits (QUEUE_COMPUTE_BIT .|. QUEUE_GRAPHICS_BIT)
-- "QUEUE_GRAPHICS_BIT .|. QUEUE_COMPUTE_BIT"
showBits :: forall a . (Show a, FiniteBits a) => a -> String
showBits a = if a == zeroBits
  then "zeroBits"
  else intercalate " .|. " $ fmap show (setBits a)

-- | The list of bits which are set
setBits :: FiniteBits a => a -> [a]
setBits a =
  [ b
  | -- lol, is this really necessary
    p <- [countTrailingZeros a .. finiteBitSize a - countLeadingZeros a - 1]
  , let b = bit p
  , a .&&. b
  ]

-- | Check if the intersection of bits is non-zero
(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (x .&. y) /= zeroBits
