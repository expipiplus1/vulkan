{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}

module Fix where

import           Control.Monad.Fix
import           Data.Either.Validation
import           Data.Foldable
import           Data.List              (find)
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Text              (Text)
import           Data.Traversable


-- fixStringLookupM :: (MonadFix m, Ord k) => (a -> k) -> ((k -> Maybe a) -> m [a]) -> m [a]
-- fixStringLookupM getName make = mdo
--   let m = Map.fromList [(getName x, x) | x <- made]
--   made <- make (`Map.lookup` m)
--   pure made



data Baz = Baz !Text Int
  deriving Show

name (Baz n _) = n
number (Baz _ n) = n

go = fixLookupM ["a", "b0", "b1", "b2", "b3", "b4", "b5"] name make
  where
    make :: (Text -> Maybe Baz) -> IO [Baz]
    make f =
      let get x = case f x of
            Just b  -> pure (number b)
            Nothing -> do
              putStrLn "failed lookup"
              pure 999
      in  sequenceA
            [ pure $ Baz "a" 1
            , Baz "b0" <$> get "c"
            , Baz "b1" <$> get "b0"
            , print ((),()) >> Baz "b2" <$> get "b1"
            , Baz "b3" <$> get "b2"
            , Baz "b4" <$> get "b3"
            , Baz "b5" <$> get "b4"
            ]

go'' = fixLookupM ["a", "b0", "b1", "b2", "b3", "b4", "b5"] name make
  where
    make :: (Text -> Maybe Baz) -> Either Text [Baz]
    make f =
      let get x = case f x of
            Just b  -> pure (number b)
            Nothing -> Left ("Couldn't find " <> x)
      in  sequenceA
            [ pure $ Baz "a" 1
            , Baz "b0" <$> get "c"
            , Baz "b1" <$> get "b0"
            , Baz "b2" <$> get "b1"
            , Baz "b3" <$> get "b2"
            , Baz "b4" <$> get "b3"
            , Baz "b5" <$> get "b4"
            ]


go' = fixStringLookup
  name
  (\f ->
    [ Baz "a" 1
    , Baz
      "b"
      (case f "a" of
        Just b  -> number b
        Nothing -> 12
      )
    ]
  )

data Bar = Bar
  { bType :: Either Text Word
  , bName :: Text
  }

data Foo = Foo
  { sz    :: Word
  , al    :: Word
  , off   :: Word
  , fName :: Text
  , type' :: Either Text Word
  }
  deriving(Show)

bs = [Bar (Left "c") "a", Bar (Left "a") "b", Bar (Right 10) "c"]

foo' :: [Bar] -> Either Text [Foo]
foo' bars = fixLookupM [] fName make
  where
    make :: (Text -> Maybe Foo) -> Either Text [Foo]
    make f = do
       rec foos <- do
             let offsets = memberOffsets' foos
             let lookupType n =
                   case f n of
                     Nothing -> 0 -- Left "unknown type" -- pure $ sz f
                     Just f  -> sz f
             for (zipSameLength bars offsets) $ \(b, off) -> do
               let sz = case bType b of
                      Right w -> w
                      Left n  -> lookupType n
               let al = 4
                   fName = bName b
                   type' = bType b
               pure Foo {..}
       pure foos

foo :: [Bar] -> Either Text [Foo]
foo bars = do
  rec foos <- do
        let offsets = memberOffsets' foos
        let lookupType n =
              case find' ((n ==) . fName) foos of
                Nothing -> Left "unknown type" -- pure $ sz f
                Just f  -> pure $ sz f
        for (zipSameLength bars offsets) $ \(b, off) -> do
          sz <- case bType b of
                 Right w -> pure w
                 Left n  -> lookupType n
          let al = 4
              fName = bName b
              type' = bType b
          pure Foo {..}
  pure foos

find' p xs =
  let p' Nothing  = True
      p' (Just x) = p x
  in find'' p' ((Just <$> xs) ++ [Nothing])

find'' p ~(x:xs)
  | p x
  = x
  | otherwise
  = find'' p xs

-- | Find the next multiple of an alignment
nextAlignment
  :: Word
  -- ^ The alignment
  -> Word
  -- ^ The value to align
  -> Word
  -- ^ The next multiple of alignment
nextAlignment alignment value =
  alignment * ((value + (alignment - 1)) `quot` alignment)

-- | zipSameLength is lazy in its second argument
zipSameLength :: [a] -> [b] -> [(a,b)]
zipSameLength []     _bs     = []
zipSameLength (a:as) ~(b:bs) = (a,b) : zipSameLength as bs

memberOffsets' :: [Foo] -> [Word]
memberOffsets' = tail . fmap fst . scanl go (0, 0)
  where
    go
      :: (Word, Word)
      -- ^ The beginning and end of the previous struct
      -> Foo
      -- ^ The struct to determine the offset for
      -> (Word, Word)
    go (_, e) m =
      let b = nextAlignment (al m) e
      in (b, b + sz m)
