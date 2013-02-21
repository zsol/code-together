import Data.Map
import Control.Monad.Reader
import Data.Either
import Prelude hiding (lookup)

-- TODO: GADTs
type VarName = String
data Expr = I Int
          | Var VarName
          | Plus Expr Expr
          | Mul Expr Expr
          | Let VarName Expr Expr
            deriving (Show)

type Context = Map String Int

sample :: Expr
sample = Let "myVar" (Plus (I 1) (I 2)) (Mul (Var "myVa")
                                              (Var "myVar"))

eval' :: Expr -> ReaderT Context (Either String) Int
eval' (I int) = return int

eval' (Plus e1 e2) = do
  v1 <- eval' e1
  v2 <- eval' e2
  return $ v1 + v2
  
eval' (Mul e1 e2) = do
  v1 <- eval' e1
  v2 <- eval' e2
  return $ v1 * v2
  
eval' (Var name) = do
  ctx <- ask
  case lookup name ctx of
    Just val -> return val
    Nothing -> lift $ Left $ "Undefined variable " ++ name

eval' (Let name e1 e2) = case eval e1 of
  Right v1 -> local (insert name v1) (eval' e2)
  left     -> lift left

eval :: Expr -> Either String Int
eval e = runReaderT (eval' e) empty
