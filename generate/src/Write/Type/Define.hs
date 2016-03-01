{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Write.Type.Define
  ( writeDefine
  ) where

import Control.Applicative((<|>))
import Control.Monad.Writer
import Data.HashSet as S
import Data.String
import Language.Haskell.Exts.Syntax hiding (Assoc(..), ModuleName)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Prelude hiding(exp)
import Spec.Type
import Text.InterpolatedString.Perl6
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Token.Style(emptyOps)
import Text.Parser.Expression
import Text.Parser.Token.Highlight
import Text.PrettyPrint.Leijen.Text hiding ((<$>), parens, char)
import Text.Trifecta
import Write.Utils
import Write.WriteMonad

writeDefine :: Define -> Write Doc
writeDefine d =
  let (header, value) = head . dSymTab $ d
      boundNames = boundVariablesFromDefine header
      (expression, requiredNames) = 
        parseCExpressionToHsExpression value
  in do tellRequiredNames (S.toList requiredNames)
        pure [qc|{camelCase_ $ dName d} {hsep (fromString <$> boundNames)} = {prettyPrint expression}|]
       
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

type ExpressionParser = WriterT (HashSet RequiredName) Parser

parseCExpressionToHsExpression :: String -> (Exp, HashSet RequiredName)
parseCExpressionToHsExpression s = 
  case parseString (runWriterT (exp <* eof)) mempty s of
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
nameExp = Var . UnQual . Ident . camelCase_ <$> name

litExp :: ExpressionParser Exp
litExp = Lit . Int <$> natural

castExp :: ExpressionParser Exp
castExp = parens name *> parens exp



