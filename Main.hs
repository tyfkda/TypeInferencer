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

parseInfer :: String -> IO ()
parseInfer code = do
  let result = parseString code
  case result of
    Right expr -> putStrLn (show expr ++ " :: " ++ show (infer globalEnv expr))
    Left err   -> print err

main :: IO ()
main = do
  parseInfer "\\x -> x + 1"
  parseInfer "\\f -> \\g -> \\x -> f (g x)"
