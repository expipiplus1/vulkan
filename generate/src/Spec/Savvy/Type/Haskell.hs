{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

module Spec.Savvy.Type.Haskell
  ( toHsType
  , toHsTypePrec
  , protoToHsTypeNonIO
  ) where

import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.Strict (WriterT, runWriterT)
import           Control.Monad.Writer.Class
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Text.Prettyprint.Doc
import           Spec.Savvy.Error
import           Spec.Savvy.Type
import           Write.Element                     hiding (pattern TypeName)

toHsType
  :: Type
  -- ^ The type to convert to a Haskell string
  -> Either [SpecError] (Doc (), ([Import], [Text]))
  -- ^ (The type string, (Any imports it requires, Any language extensions it requires))
toHsType = toHsTypePrec (-1)

toHsTypePrec
  :: Int
  -- ^ The precendence of the sorrounding context
  -> Type
  -- ^ The type to convert to a Haskell string
  -> Either [SpecError] (Doc (), ([Import], [Text]))
  -- ^ (The type string, (Any imports it requires, Any language extensions it requires))
toHsTypePrec prec = runWriterT . runPop allLowercaseWords . toHsType' prec Positive

protoToHsTypeNonIO
  :: Type
  -> [(Maybe Text, Type)]
  -> Either [SpecError] (Doc (), ([Import], [Text]))
protoToHsTypeNonIO ret ps = runWriterT . runPop allLowercaseWords $ do
  ret' <- toHsType' 10 Positive ret
  -- Although '->' has a precedence of 0, it looks weird to leave the
  -- arguments naked.
  ps'  <- traverse (paramToHsType 10 (Negative)) ps
  pure . parens' False $ foldr (\p r -> p <+> "->" <+> r) ret' ps'


data Pos = Negative | Positive

neg :: Pos -> Pos
neg = \case
  Negative -> Positive
  Positive -> Negative

toHsType' :: Int -> Pos -> Type -> TypeM (Doc ())
toHsType' prec pos = \case
  Float -> use "Foreign.C.Types" "CFloat"
  Void  -> case pos of
    Negative ->
      pure "()"
      -- n <- pop
      -- pure $ pretty n
    Positive -> pure "()"
  Char  -> use "Foreign.C.Types" "CChar"
  Int   -> use "Foreign.C.Types" "CInt"
  Ptr t -> do
    tellImport "Foreign.Ptr" "Ptr"
    t' <- toHsType' 10 pos t
    pure . parens' (prec >= 10) $ "Ptr" <+> t'
  Array size t -> do
    tellImport "Data.Vector.Storable.Sized" "Vector"
    size' <- sizeToType size
    t'    <- toHsType' 10 pos t
    pure . parens' (prec >= 10) $ "Vector" <+> size' <+> t'
  TypeName t   -> cIdToHsType t
  Proto ret ps -> do
    ret' <- toHsType' 10 pos ret
    let ioRet = "IO" <+> ret'
    -- Although '->' has a precedence of 0, it looks weird to leave the
    -- arguments naked.
    ps' <- traverse (paramToHsType 10 (neg pos)) ps
    pure . parens' (prec >= 0) $ foldr (\p r -> p <+> "->" <+> r) ioRet ps'

cIdToHsType
  :: Text
  -> TypeM (Doc ())
cIdToHsType = \case
  "void"     -> pure "()"
  "char"     -> use "Foreign.C.Types" "CChar"
  "float"    -> use "Foreign.C.Types" "CFloat"
  "uint8_t"  -> use "Data.Word" "Word8"
  "uint32_t" -> use "Data.Word" "Word32"
  "uint64_t" -> use "Data.Word" "Word64"
  "int32_t"  -> use "Data.Int" "Int32"
  "size_t"   -> use "Foreign.C.Types" "CSize"
  t          -> pure $ pretty t

parens' :: Bool -> Doc () -> Doc ()
parens' = \case
  True -> parens
  False -> id

paramToHsType :: Int -> Pos -> (Maybe Text, Type) -> TypeM (Doc ())
paramToHsType prec pos = \case
  (Just n, t) -> do
    t' <- toHsType' 9 pos t
    tellImport "Graphics.Vulkan.NamedType" "(:::)"
    tellExtension "DataKinds"
    pure . parens' (prec >= 9) $ "\"" <> pretty n <> "\"" <+> ":::" <+> t'
  (Nothing, t) -> toHsType' prec pos t

sizeToType
  :: ArraySize
  -> TypeM (Doc ())
sizeToType = \case
  NumericArraySize n -> do
    tellExtension "DataKinds"
    pure (pretty (show n))
  SymbolicArraySize s -> pure $ pretty s


use
  :: Text
  -- ^ Modue name
  -> Text
  -- ^ Type name
  -> TypeM (Doc ())
use m name = do
  tellImport m name
  pure (pretty name)

tellImport :: Text -> Text
  -> TypeM ()
tellImport m t = tell ([Import m [t]],[])

tellExtension :: Text
  -> TypeM ()
tellExtension e = tell ([], [e])

type TypeM  = PopT Text (WriterT ([Import], [Text]) (Either [SpecError]))

----------------------------------------------------------------
-- Getting names
----------------------------------------------------------------

newtype PopT p m a = Pop { unPop :: StateT [p] m a }
  deriving (Functor, Applicative, Monad, MonadWriter w)

runPop :: Monad m => [p] -> PopT p m a -> m a
runPop ps (Pop s) = evalStateT s (cycle ps)

pop :: Monad m => PopT p m p
pop = Pop $ do
  p <- gets head
  modify' tail
  pure p

--------------------------------------------------------------------------------
-- The alphabet
--------------------------------------------------------------------------------

allLowercaseWords :: [Text]
allLowercaseWords =
  T.pack . fmap unLowercase <$> iterate lexicographicIncrement [minBound]

newtype LowercaseLatin = LowercaseLatin {unLowercase :: Char}
  deriving (Eq, Enum)

instance Bounded LowercaseLatin where
  minBound = LowercaseLatin 'a'
  maxBound = LowercaseLatin 'z'

lexicographicIncrement :: (Bounded a, Enum a, Eq a) => [a] -> [a]
lexicographicIncrement = \case
  [] -> [minBound]
  (x : xs) | x == maxBound -> minBound : lexicographicIncrement xs
           | otherwise     -> succ x : xs
