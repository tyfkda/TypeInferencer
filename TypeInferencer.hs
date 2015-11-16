module TypeInferencer (infer) where

import Prelude hiding (lookup)
import Control.Monad.ST (ST, runST)
import Data.Map (empty, fromList, insert, lookup, Map)
import Data.STRef (newSTRef, readSTRef, writeSTRef, STRef)

import Node (Expr(..))
import Type (Type(..))

-- Environment
type Env = Map String Type

-- (next var index, {index=>type})
type VarInfo = (Int, Map Int Type)

infer :: [(String, Type)] -> Expr -> Type
infer env expr = runST $ do
  varInfoRef <- newSTRef (0, empty)
  t <- doInfer (fromList env) varInfoRef expr
  (_, varDict) <- readSTRef varInfoRef
  return $ refer t varDict

doInfer :: Env -> STRef s VarInfo -> Expr -> ST s Type
doInfer env varInfoRef expr =
  case expr of
    Natural i  -> return TInt
    Var x -> do
      case lookup x env of
        Just t  -> return t
        Nothing -> error ("not found: " ++ x)
    Fun parm e -> do
      tparm <- createVar varInfoRef
      te <- doInfer (insert parm tparm env) varInfoRef e
      return $ TFun tparm te
    App f arg -> do
      funType <- doInfer env varInfoRef f
      argType <- doInfer env varInfoRef arg
      retType <- createVar varInfoRef
      unify funType (TFun argType retType) varInfoRef
      return retType

unify :: Type -> Type -> STRef s VarInfo -> ST s ()
unify (TFun p1 e1) (TFun p2 e2) varInfoRef = do
  unify p1 p2 varInfoRef
  unify e1 e2 varInfoRef
unify t1@(TVar i1) t2@(TVar i2) varInfoRef
  | i1 == i2  = return ()
unify (TVar i1) t2 varInfoRef = unifyVar i1 t2 varInfoRef
unify t1 (TVar i2) varInfoRef = unifyVar i2 t1 varInfoRef
unify t1 t2 varInfoRef
  | t1 == t2  = return ()
  | otherwise = cannotUnify t1 t2 varInfoRef

unifyVar :: Int -> Type -> STRef s VarInfo -> ST s ()
unifyVar index type2 varInfoRef = do
  isOccur <- occur type2 index varInfoRef
  if isOccur
    then  error "occurs error"
    else do
      (nextIdx, varMap) <- readSTRef varInfoRef
      case lookup index varMap of
        Just vt  -> unify vt type2 varInfoRef
        Nothing  -> writeSTRef varInfoRef (nextIdx, insert index type2 varMap)

occur :: Type -> Int -> STRef s VarInfo -> ST s Bool
occur (TFun p e) n varInfoRef = (||) <$> occur p n varInfoRef <*> occur e n varInfoRef
occur (TVar i) n varInfoRef
  | i == n    = return True
  | otherwise = do
      (_, varMap) <- readSTRef varInfoRef
      case lookup i varMap of
        Just vt  -> occur vt n varInfoRef
        Nothing  -> return False
occur _ _ _   = return False

createVar :: STRef s VarInfo -> ST s Type
createVar varInfoRef = do
  (nextIdx, varMap) <- readSTRef varInfoRef
  writeSTRef varInfoRef (nextIdx + 1, varMap)
  return $ TVar nextIdx

refer :: Type -> Map Int Type -> Type
refer (TFun p e) varMap = TFun (refer p varMap) (refer e varMap)
refer t@(TVar v) varMap = case lookup v varMap of
  Just vt  -> refer vt varMap
  Nothing  -> t
refer t _               = t

cannotUnify t1 t2 varInfoRef = do
  (_, varMap) <- readSTRef varInfoRef
  error ("cannot unify: " ++ show (refer t1 varMap) ++ " <=> " ++ show (refer t2 varMap))
