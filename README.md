Type Inference in Haskell
=========================

Simple parser and type inference in Haskell.

```haskell
$ stack ghci
Using main module: 1. Package `TypeInferencer' component TypeInferencer:exe:infer_type_test with main-is file: ./src/main/Main.hs
TypeInferencer> configure (lib + exe)
Configuring TypeInferencer-0.1.0.0...
TypeInferencer> initial-build-steps (lib + exe)
Configuring GHCi with the following packages: TypeInferencer
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
[1 of 5] Compiling Type             ( ./src/infer/Type.hs, interpreted )
[2 of 5] Compiling Node             ( ./src/infer/Node.hs, interpreted )
[3 of 5] Compiling Parser           ( ./src/infer/Parser.hs, interpreted )
[4 of 5] Compiling TypeInferencer   ( ./src/infer/TypeInferencer.hs, interpreted )
[5 of 5] Compiling Main             ( ./src/main/Main.hs, interpreted )
Ok, modules loaded: TypeInferencer, Node, Parser, Type, Main.
*Main Node Parser Type TypeInferencer> parseInfer "\\f -> \\g -> \\x -> f (g x)"
Fun "f" (Fun "g" (Fun "x" (App (Var "f") (App (Var "g") (Var "x"))))) :: (t3 -> t4) -> (t2 -> t3) -> t2 -> t4
```


### Documentation (JP)

* [Parsecで構文解析して構文木を作る - Kludge Factory](https://tyfkda.github.io/blog/2015/11/15/parsec-ast.html)
* [型推論を実装してみる - Kludge Factory](https://tyfkda.github.io/blog/2015/11/15/type-inference.html)
