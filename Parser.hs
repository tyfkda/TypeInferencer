module Parser (parseString) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

import Node (Expr(..))

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { reservedOpNames = ["*", "/", "+", "-"] })

natural     = P.natural lexer
parens      = P.parens lexer
reservedOp  = P.reservedOp lexer
identifier  = P.identifier lexer
lexeme      = P.lexeme lexer

expr :: Parser Expr
expr = buildExpressionParser table term <|> fun <?> "expression"
  where
    table = [[unary "-" "negate"],
             [binop "*" AssocLeft, binop "/" AssocLeft],
             [binop "+" AssocLeft, binop "-" AssocLeft]]
    binop op assoc = Infix (do{ reservedOp op; return $ \x y -> App (App (Var op) x) y } <?> "operator") assoc
    unary s op = Prefix (do{ reservedOp s; return $ App (Var op) })

term :: Parser Expr
term = try(app) <|> try(factor)

factor :: Parser Expr
factor =
  do {
    parens expr;
  } <|> do {
    n <- natural;
    return $ Natural n
  } <|> do {
    var <- identifier;
    return $ Var var
  } <?>
    "factor"

fun :: Parser Expr
fun = do
  lexeme $ char '\\'
  param <- identifier
  lexeme $ string "->"
  e <- expr
  return $ Fun param e

app :: Parser Expr
app = do
  f <- factor
  arg <- factor
  return $ App f arg

stmt :: Parser Expr
stmt = do
  e <- expr
  eof
  return e

parseString text = parse stmt "" text
