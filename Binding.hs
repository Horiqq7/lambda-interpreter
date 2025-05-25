module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx context evalStep lambdaExpr = do
  fullyExpanded <- substituteMacros context lambdaExpr
  return (simplify evalStep fullyExpanded)

-- | Înlocuiește recursiv toți constructorii Macro cu expresiile corespunzătoare
--   definite în context. Dacă un macro nu are definiție, întoarce o eroare.
substituteMacros :: Context -> Lambda -> Either String Lambda
substituteMacros _   v@(Var _)        = Right v
substituteMacros ctx (Macro macroId) =
  case lookup macroId ctx of
    Just expansion -> substituteMacros ctx expansion
    Nothing        -> Left ("Undefined macro: " ++ macroId)
substituteMacros ctx (App left right) =
  App <$> substituteMacros ctx left
      <*> substituteMacros ctx right
substituteMacros ctx (Abs param body) =
  Abs param <$> substituteMacros ctx body


normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
