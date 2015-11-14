import Parser (parseString)
import TypeInferencer (infer)
import Type (Type(..))

globalEnv :: [(String, Type)]
globalEnv = [
  ("+", TFun TInt (TFun TInt TInt)),
  ("-", TFun TInt (TFun TInt TInt)),
  ("*", TFun TInt (TFun TInt TInt)),
  ("/", TFun TInt (TFun TInt TInt)),
  ("negate", TFun TInt TInt)]

parseInfer code = do
  let result = parseString code
  case result of
    Right expr -> putStrLn (show expr ++ " :: " ++ show (infer globalEnv expr))
    Left err   -> print err

main = do
  parseInfer "\\f -> \\x -> f x + 1"
