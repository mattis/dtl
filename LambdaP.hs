{-# LANGUAGE GADTs #-}

module LambdaP where

data ContextTerm where
  Kind :: (String, Kind) -> ContextTerm 
  Type :: (String, Type) -> ContextTerm
  
type Context = [ContextTerm]
  
data Kind where
  Star :: Kind
  Product :: (String, Type) -> Kind -> Kind
  deriving (Show, Eq)

data Type where
  TUnit :: Type
  TVar :: String -> Type
  Forall :: (String, Type) -> Type -> Type
  TApp :: Type -> Expr -> Type
  deriving (Show, Eq)
  
data Expr where
  Unit :: Expr
  Var :: String -> Expr
  Apply :: Expr -> Expr -> Expr
  Abs :: (String, Type) -> Expr -> Expr
  deriving (Show, Eq)

substKind :: String -> Kind ->  Expr -> Kind
substKind n Star _ = Star
substKind n (Product (n1, t) k) e | n == n1   = Product (n1, t) k 
                                  | otherwise = Product (n1, substType n t e) (substKind n k e) 
  
substType :: String -> Type -> Expr -> Type
substType _ TUnit _ = TUnit
substType _ (TVar n) _ = TVar n
substType n t@(Forall (n', t1) t2) e
  | n == n' = t
  | otherwise = Forall (n', substType n t1 e) (substType n t2 e)
substType n (TApp t e) e' = TApp (substType n t e') e

subst :: String -> Expr -> Expr -> Expr
subst _ Unit _ = Unit
subst n (Var n') e
  | n == n' = e
  | otherwise = Var n'
subst n (Apply e1 e2) e' = Apply (subst n e1 e') (subst n e2 e')
subst n (Abs (n', t) e) e'
  | n == n'   = Abs (n', evalType t) e
  | otherwise = Abs (n', evalType t) (subst n e e')

eval :: Expr -> Expr
eval Unit        = Unit
eval (Var n)     = Var n
eval (Apply e1 e2) = let (Abs (n, t) e) = eval e1
                         e2' = eval e2
                   in eval $ subst n e e2'
eval e@(Abs _ _) = e

evalType :: Type -> Type
evalType TUnit = TUnit
evalType (TVar n) = TVar n
evalType (Forall (n, t) t') = Forall (n, evalType t) (evalType t')
evalType (TApp t e) = TApp (evalType t) (eval e)

evalKind :: Kind -> Kind
evalKind Star = Star
evalKind (Product (n, t) k) = Product (n, evalType t) (evalKind k)


dom :: Context -> [String]
dom = map (fst . convert)

lookupK :: Context -> String -> Kind
lookupK c s = case lookup' s c of
  Just (Left k) -> k
  _             -> error $ "variable " ++ s ++ " not bound"
    
lookupT :: Context -> String -> Type
lookupT c s = case lookup' s c of
  Just (Right t) -> t
  _              -> error $ "variable " ++ s ++ " not bound"

lookup' :: String -> Context -> Maybe (Either Kind Type)
lookup' s = lookup s . map convert
  
convert :: ContextTerm -> (String, Either Kind Type)
convert (Kind (n, k)) = (n, Left k)
convert (Type (n, t)) = (n, Right t)
    
    
kindIsWellFormed :: Context -> Kind -> Bool
kindIsWellFormed _ Star = True
kindIsWellFormed c (Product (n, t) k) = kindIsWellFormed (Type (n, t) : c) k

inferKind :: Context -> Type -> Kind
inferKind c TUnit = Star
inferKind c (TVar n) = lookupK c n
inferKind c (Forall (n, t1) t2) = if inferKind (Type (n, t1) : c) t2 == Star then Star else error "no star :("
inferKind c (TApp t e) = let Product (x, t2) k = inferKind c t 
                             t1 = inferType c e
                         in if (t1 == t2) then substKind x k e else error "no type match :'("
                                                                   
inferType :: Context -> Expr -> Type
inferType c Unit = TUnit
inferType c (Var n)        = lookupT c n
inferType c (Apply e1 e2)  = let Forall (n, t1) t2 = inferType c e1
                                 t3 = inferType c e2
                             in if t1 == t3 then substType n t2 e2 else error "type mismatch ;'("
inferType c (Abs (n, t) e) = let t1 = inferType (Type (n,t):c) e
                             in Forall (n,t) t1
                                
                                
(|-->) :: Type -> Type -> Type
t1 |--> t2 = Forall ("_", t1) t2

(|==>) :: Type -> Kind -> Kind 
t1 |==> k2 = Product ("_", t1) k2