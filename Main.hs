import Parser (parseString)

main = do
  print $ parseString "(\\x -> x * x) 111"
  print $ parseString "(\\square -> square 111) (\\x -> x * x)"
  print $ parseString "\\f -> \\x -> f x + 1"
