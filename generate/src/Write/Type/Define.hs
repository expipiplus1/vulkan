{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Write.Type.Define
  ( writeDefine
  ) where

import           Control.Applicative           ((<|>))
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashSet                  as S
import           Data.String
import qualified Language.C.Types              as C
import           Language.Haskell.Exts.Pretty  (prettyPrint)
import           Language.Haskell.Exts.Syntax  hiding (Assoc (..), ModuleName)
import           Prelude                       hiding (exp)
import           Spec.Type
import           Spec.TypeEnv
import           Text.InterpolatedString.Perl6
import           Text.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style       (emptyOps)
import           Text.PrettyPrint.Leijen.Text  hiding (char, parens, (<$>))
import           Text.Trifecta
import           Write.TypeConverter
import           Write.Utils
import           Write.WriteMonad

writeDefine :: Define -> Write Doc
writeDefine d =
  let (header, value) = head . dSymTab $ d
      boundNames = boundVariablesFromDefine header
      hsName = dHsName d
      -- TODO: This assumes the defines are of type uint32_t
      Right uint32_t = C.cIdentifierFromString "uint32_t"
  in do (expression, requiredNames) <-
          parseCExpressionToHsExpression value
        tellRequiredNames (S.toList requiredNames)
        hsElemType <- cTypeToHsType (C.TypeSpecifier (C.Specifiers [] [] [])
                                    (C.TypeName uint32_t))
        let hsType = foldr TyFun hsElemType (hsElemType <$ boundNames)
        pure [qc|{hsName} :: {prettyPrint hsType}
{hsName} {hsep (fromString <$> boundNames)} = {prettyPrint expression}
|]

boundVariablesFromDefine :: String -> [String]
boundVariablesFromDefine d =
  case parseString (p <* eof) mempty d of
    Failure e -> error ("Failed to parse define header\n" ++ show e)
    Success bs -> bs
  where p = name *> (parens (commaSep name) <|> pure [])

name :: (TokenParsing m, Monad m) => m String
name = ident identifierStyle

identifierStyle :: TokenParsing m => IdentifierStyle m
identifierStyle = IdentifierStyle{ _styleName = "ident"
                                 , _styleStart = letter <|> char '_'
                                 , _styleLetter = alphaNum <|> char '_'
                                 , _styleReserved = mempty
                                 , _styleHighlight = Identifier
                                 , _styleReservedHighlight = ReservedIdentifier
                                 }

type ExpressionParser = ReaderT TypeEnv (WriterT (S.HashSet RequiredName) Parser)

parseCExpressionToHsExpression :: String -> Write (Exp, S.HashSet RequiredName)
parseCExpressionToHsExpression s = do
  te <- askTypeEnv
  return $ case parseString (runWriterT $ runReaderT (exp <* eof) te) mempty s of
    Failure e -> error ("Failed to parse C expression\n" ++ show e)
    Success e -> e

exp :: ExpressionParser Exp
exp = buildExpressionParser opTable term <?> "expression"

opTable :: [[Operator ExpressionParser Exp]]
opTable = [ [ binary ">>" (ModuleName "Data.Bits") "shiftR" AssocLeft
            , binary "<<" (ModuleName "Data.Bits") "shiftL" AssocLeft
            ]
          , [binary "&" (ModuleName "Data.Bits") "(.&.)" AssocLeft]
          , [binary "|" (ModuleName "Data.Bits") "(.|.)" AssocLeft]
          ]

binary :: String -> ModuleName -> String -> Assoc
       -> Operator ExpressionParser Exp
binary cName moduleName hsName = Infix p
  where hsOp = Var (UnQual (Ident hsName))
        p = reservedOp cName *>
            tell (S.singleton $ ExternalName moduleName hsName) *>
            pure (\x y -> hsOp `App` x `App` y)

reservedOp :: String -> ExpressionParser ()
reservedOp = reserve emptyOps

term :: ExpressionParser Exp
term = (litExp <?> "literal")
   <|> (try functionCall <?> "Function call")
   <|> (try nameExp <?> "ident")
   <|> (try castExp <?> "C Style cast")
   <|> parens exp

functionCall :: ExpressionParser Exp
functionCall =
  do functionName <- nameExp
     arguments <- parens (commaSep exp)
     pure $ foldl App functionName arguments

nameExp :: ExpressionParser Exp
nameExp = do
  typeMap <- teNameLocations <$> ask
  cName <- name
  hsName <- case M.lookup cName typeMap of
    Just (m, n) -> do
      tell (S.singleton $ ExternalName m n)
      pure n
    Nothing ->
      pure cName
  return $ Var $ UnQual $ Ident hsName

litExp :: ExpressionParser Exp
litExp = Lit . Int <$> natural

castExp :: ExpressionParser Exp
castExp = parens name *> parens exp
