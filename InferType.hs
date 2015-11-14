import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as P

data Expr = Natural Integer
          | BinOp String Expr Expr
  deriving (Show)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser (haskellDef { reservedOpNames = ["*", "/", "+", "-"] })

natural     = P.natural lexer
parens      = P.parens lexer
reservedOp  = P.reservedOp lexer

expr :: Parser Expr
expr = buildExpressionParser table term <?> "expression"
  where
    table = [[unary "-" (BinOp "-" (Natural 0))],
             [binop "*" AssocLeft, binop "/" AssocLeft],
             [binop "+" AssocLeft, binop "-" AssocLeft]]
    binop op assoc = Infix (do{ reservedOp op; return (BinOp op) } <?> "operator") assoc
    unary s op = Prefix (do{ reservedOp s; return op })

term :: Parser Expr
term =
  do {
    parens expr;
  } <|> do {
    n <- natural;
    return $ Natural n
  } <?>
    "term"

stmt :: Parser Expr
stmt = do
  e <- expr
  eof
  return e

main = do
  print $ parse stmt "" "-1 + 2 * 3"
  print $ parse stmt "" "(-1 + 2) * 3"
