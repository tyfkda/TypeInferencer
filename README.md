Type Inference in Haskell
=========================

Simple parser and type inference in Haskell.

```haskell
$ ghci
GHCi, version 7.10.2: http://www.haskell.org/ghc/  :? for help
Prelude> :l Main
[1 of 5] Compiling Node             ( Node.hs, interpreted )
[2 of 5] Compiling Type             ( Type.hs, interpreted )
[3 of 5] Compiling TypeInferencer   ( TypeInferencer.hs, interpreted )
[4 of 5] Compiling Parser           ( Parser.hs, interpreted )
[5 of 5] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Parser, Type, TypeInferencer, Main, Node.
*Main> parseInfer "\\f -> \\g -> \\x -> f (g x)"
Fun "f" (Fun "g" (Fun "x" (App (Var "f") (App (Var "g") (Var "x"))))) :: (t3 -> t4) -> (t2 -> t3) -> t2 -> t4
```


### Documentation (JP)

* [Parsecで構文解析して構文木を作る - Kludge Factory](https://tyfkda.github.io/blog/2015/11/15/parsec-ast.html)
* [型推論を実装してみる - Kludge Factory](https://tyfkda.github.io/blog/2015/11/15/type-inference.html)
