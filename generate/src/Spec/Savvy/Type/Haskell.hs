{-# LANGUAGE DataKinds                  #-}


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

import           Control.Monad.Trans.Writer.Strict (WriterT, runWriterT)
import           Control.Monad.Writer.Class
import           Data.Text                         (Text)
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
toHsTypePrec prec = runWriterT . toHsType' prec Positive

protoToHsTypeNonIO
  :: Type
  -> [(Maybe Text, Type)]
  -> Either [SpecError] (Doc (), ([Import], [Text]))
protoToHsTypeNonIO ret ps = runWriterT $ do
  ret' <- toHsType' 10 Positive ret
  -- Although '->' has a precedence of 0, it looks weird to leave the
  -- arguments naked.
  ps'  <- traverse (paramToHsType 10 Negative) ps
  pure . parens' False $ foldr (\p r -> p <+> "->" <+> r) ret' ps'


data Pos = Negative | Positive

neg :: Pos -> Pos
neg = \case
  Negative -> Positive
  Positive -> Negative

toHsType' :: Int -> Pos -> Type -> TypeM (Doc ())
toHsType' prec pos = \case
  Float -> useWithConstructors "Foreign.C.Types" "CFloat"
  Void  -> pure "()"
  Char  -> useWithConstructors "Foreign.C.Types" "CChar"
  Int   -> useWithConstructors "Foreign.C.Types" "CInt"
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

cIdToHsType :: Text -> TypeM (Doc ())
cIdToHsType = \case
  "void"             -> pure "()"
  "int"              -> useWithConstructors "Foreign.C.Types" "CInt"
  "char"             -> useWithConstructors "Foreign.C.Types" "CChar"
  "float"            -> useWithConstructors "Foreign.C.Types" "CFloat"
  "uint8_t"          -> use "Data.Word" "Word8"
  "uint32_t"         -> use "Data.Word" "Word32"
  "uint64_t"         -> use "Data.Word" "Word64"
  "int32_t"          -> use "Data.Int" "Int32"
  "size_t"           -> useWithConstructors "Foreign.C.Types" "CSize"
  "xcb_connection_t" -> pure "Xcb_connection_t"
  "xcb_visualid_t"   -> pure "Xcb_visualid_t"
  "xcb_window_t"     -> pure "Xcb_window_t"
  "wl_display"       -> pure "Wl_display"
  "wl_surface"       -> pure "Wl_surface"
  t                  -> pure $ pretty t

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
    tellExtension "TypeOperators"
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

useWithConstructors
  :: Text
  -- ^ Modue name
  -> Text
  -- ^ Type name
  -> TypeM (Doc ())
useWithConstructors m name = do
  tellImport m (name <> "(..)")
  pure (pretty name)

tellImport :: Text -> Text
  -> TypeM ()
tellImport m t = tell ([Import m [t]],[])

tellExtension :: Text
  -> TypeM ()
tellExtension e = tell ([], [e])

type TypeM  = WriterT ([Import], [Text]) (Either [SpecError])

