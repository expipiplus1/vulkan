module Bracket
  where

import           Relude                  hiding ( Handle
                                                , Type
                                                )
import           Data.List.Extra                ( nubOrd )
import qualified Data.Text.Extra               as T
import           Language.Haskell.TH            ( mkName )
import           Data.Text.Prettyprint.Doc
                                         hiding ( brackets
                                                , plural
                                                )
import           Polysemy
import           Polysemy.Input
import           Data.Vector                    ( Vector )

import qualified Control.Exception

import           Render.Element
import           Render.Type
import           Render.Utils
import           Render.SpecInfo
import           Render.Command
import           Render.Names
import           Spec.Parse
import           Haskell                       as H
import           Error
import           CType

data Bracket = Bracket
  { bInnerType           :: ConstructedType
  , bWrapperName         :: CName
  , bCreate              :: CName
  , bDestroy             :: CName
  , bCreateArguments     :: [Argument]
  , bDestroyArguments    :: [Argument]
  , bDestroyIndividually :: Bool
  }

data ConstructedType
  = Single { unConstructedType :: CType }
  | Optional { unConstructedType :: CType }
  | Multiple { unConstructedType :: CType }
  deriving (Eq, Ord)

pattern SingleTypeName :: Text -> ConstructedType
pattern SingleTypeName t = Single (TypeName (CName t))

getConstructedType
  :: (HasErr r, HasRenderElem r, HasRenderParams r, HasSpecInfo r)
  => ConstructedType
  -> Sem r Type
getConstructedType = \case
  Single   Void -> pure (ConT ''())
  Single   t    -> cToHsType DoNotPreserve t
  Optional t    -> do
    t <- cToHsType DoNotPreserve t
    pure (ConT ''Maybe :@ t)
  Multiple t -> do
    t <- cToHsType DoNotPreserve t
    pure (ConT ''Vector :@ t)

data Argument
  = Provided ConstructedType Text
  | Resource
  | Member Text Text
  deriving (Eq, Ord)

autoBracket
  :: forall r
   . (HasErr r, HasSpecInfo r)
  => (CName -> Text)
  -- ^ Get parameter name
  -> CName
  -- ^ With
  -> CName
  -- ^ Create
  -> CName
  -- ^ Destroy
  -> Sem r Bracket
autoBracket paramName withName beginName endName = do
  let get :: CName -> Sem r Command
      get n = note ("Unable to find command " <> show n) =<< getCommand n
  begin <- get beginName
  end   <- get endName
  let toArg :: Parameter -> Argument
      toArg Parameter {..} = Provided (Single pType) (paramName pName)
      bInnerType           = Single (cReturnType begin)
      bWrapperName         = withName
      bCreate              = beginName
      bDestroy             = endName
      bCreateArguments     = toArg <$> toList (cParameters begin)
      bDestroyArguments    = toArg <$> toList (cParameters end)
      bDestroyIndividually = False
  pure Bracket { .. }

autoBracketBeginEndWith
  :: (HasErr r, HasSpecInfo r)
  => (CName -> Text)
  -- ^ get param name
  -> CName
  -- ^ begin
  -> Sem r Bracket
autoBracketBeginEndWith paramName begin =
  let end  = CName (T.replace "Begin" "End" (unCName begin))
      with = CName (T.replace "Begin" "With" (unCName begin))
  in  autoBracket paramName with begin end

writePair
  :: (HasErr r, HasRenderParams r, HasRenderedNames r, HasSpecInfo r)
  => Bracket
  -> Sem r (CType, CName, CName, RenderElement)
  -- ^ (Inner type, create, with, render element)
writePair b@Bracket {..} =
  let arguments = nubOrd (bCreateArguments ++ bDestroyArguments)
  in
    fmap (unConstructedType bInnerType, bCreate, bWrapperName, )
    . genRe ("bracket " <> unCName bWrapperName)
    $ do
        RenderParams {..} <- input
        let create      = mkFunName bCreate
            destroy     = mkFunName bDestroy
            wrapperName = mkFunName bWrapperName
        tellExport (ETerm wrapperName)
        argHsTypes <- traverseV getConstructedType
                                [ t | Provided t _ <- arguments ]
        let argHsVars = [ pretty v | Provided _ v <- arguments ]
        innerHsType <- getConstructedType bInnerType
        let
          noDestructorResource = Resource `notElem` bDestroyArguments
          noResource = bInnerType == Single Void && noDestructorResource
          cont = if noResource
            then ConT ''IO :@ VarT (mkName "r")
            else innerHsType ~> ConT ''IO :@ VarT (mkName "r")
          wrapperType =
            foldr (~>) (ConT ''IO :@ VarT (mkName "r")) (argHsTypes ++ [cont])
        constrainedType <- constrainStructVariables wrapperType
        wrapperTDoc     <- renderType constrainedType
        bracketDoc      <- if noResource
          then do
            tellImport 'Control.Exception.bracket_
            pure "bracket_"
          else do
            tellImport 'Control.Exception.bracket
            pure "bracket"
        createCall <- renderCreate b
        destroyCall <- renderDestroy b
        tellDoc $ vsep
          [ comment
            (T.unlines
              (  [ "A safe wrapper for '"
                   <> unName create
                   <> "' and '"
                   <> unName destroy
                   <> "' using '"
                   <> bracketDoc
                   <> "'"
                 ]
              <> bool
                   [ ""
                   , "The allocated value must not be returned from the provided computation"
                   ]
                   []
                   noResource
              )
            )
          , pretty wrapperName <+> "::" <+> wrapperTDoc
          , pretty wrapperName <+> sep argHsVars <+> "=" <> line <> indent
            2
            (pretty bracketDoc <> line <> indent
              2
              (vsep
                [ parens createCall
                , parens destroyCall
                ]
              )
            )
          ]

renderCreate
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasRenderedNames r
     , HasRenderElem r
     )
  => Bracket
  -> Sem r (Doc ())
renderCreate Bracket {..} = do
  RenderParams {..} <- input
  let create = mkFunName bCreate
  createArgVars <- forV bCreateArguments $ \case
    Provided _ v -> pure (pretty v)
    Resource     -> throw "Resource used in its own construction"
    Member _ _   -> throw "Member used during construction"
  tellImport create
  pure $ pretty create <+> sep createArgVars

renderDestroy
  :: ( HasErr r
     , HasRenderParams r
     , HasSpecInfo r
     , HasRenderedNames r
     , HasRenderElem r
     )
  => Bracket
  -> Sem r (Doc ())
renderDestroy Bracket {..} = do
  RenderParams {..} <- input
  let arguments            = nubOrd (bCreateArguments ++ bDestroyArguments)
      destroy              = mkFunName bDestroy
      noDestructorResource = Resource `notElem` bDestroyArguments
      noResource           = bInnerType == Single Void && noDestructorResource
      resourcePattern      = if noDestructorResource then "_" else "o"
  destroyArgVars <- forV bDestroyArguments $ \case
    Provided _ v -> pure $ pretty v
    Resource     -> pure "o"
    Member member argument
      | [t] <- [ t | Provided (Single t) v <- arguments, v == argument ] -> do
        argTyDoc <- renderType =<< cToHsTypeWithHoles DoNotPreserve t
        pure $ parens
          (pretty member <+> parens (pretty argument <+> "::" <+> argTyDoc))
      | otherwise -> throw "Can't find single argument for member"
  tellImport destroy
  let callDestructor =
        (if noResource then emptyDoc else "\\" <> resourcePattern <+> "-> ")
          <>  pretty destroy
          <+> sep destroyArgVars
      traverseDestroy = "traverse" <+> parens callDestructor
  pure $ bool callDestructor traverseDestroy bDestroyIndividually
