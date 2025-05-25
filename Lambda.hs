module Lambda where

import Data.List (nub, (\\))
import Control.Monad (replicateM)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars = nub . collectVars
  where
    -- collectVars nu face nub, doar adună toate aparițiile
    collectVars :: Lambda -> [String]
    collectVars (Var x)       = [x]            -- variabilă liberă sau bound
    collectVars (App e1 e2)   = collectVars e1 ++ collectVars e2
    collectVars (Abs x e)     = x : collectVars e  -- binder-ul x e valuează ca apariție
    collectVars (Macro m)     = [m]            -- macro-ul contează ca nume

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e) = freeVars e \\ [x]
freeVars (Macro _) = []

-- 1.3.
newVar :: [String] -> String
newVar usedVars = head (filter (`notElem` usedVars) allNames)
  where
    letters = ['a'..'z']
    allNames = concatMap(\n -> replicateM n letters) [1..]

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _)       = True
isNormalForm (Macro _)     = True
isNormalForm (Abs _ body)  = isNormalForm body
isNormalForm (App e1 e2)   =
    case e1 of
      Abs _ _ -> False            -- redex găsit: (λx. …) e2
      _       -> isNormalForm e1  -- continuăm în stânga
                 && isNormalForm e2  -- și în dreapta

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = substitute x e2 (alphaConvert e1)
  where
    -- variabilele libere din e2
    fvs   = freeVars e2
    -- toate variabilele (bound + free + macro) din e1 și e2
    used0 = vars e1 ++ vars e2

    -- α-conversie: pentru fiecare Abs y bazat pe collisions with fvs,
    -- renumim binder-ul și corpul
    alphaConvert :: Lambda -> Lambda
    alphaConvert v@(Var _)     = v
    alphaConvert m@(Macro _)   = m
    alphaConvert (App l r)     = App (alphaConvert l) (alphaConvert r)
    alphaConvert (Abs y body)
      | y `elem` fvs =
          let y'    = newVar (used0 ++ vars body)
              body' = rename y y' body
          in Abs y' (alphaConvert body')
      | otherwise   = Abs y (alphaConvert body)

    -- renumește toate aparițiile lui old în new
    rename :: String -> String -> Lambda -> Lambda
    rename old new (Var v)
      | v == old  = Var new
      | otherwise = Var v
    rename _ _ m@(Macro _)      = m
    rename old new (App l r)    = App (rename old new l) (rename old new r)
    rename old new (Abs y b)
      | y == old  = Abs new (rename old new b)
      | otherwise = Abs y (rename old new b)

    -- substituție textuală: x ↦ e
    substitute :: String -> Lambda -> Lambda -> Lambda
    substitute x' e (Var v)
      | v == x'   = e
      | otherwise = Var v
    substitute _ _ m@(Macro _)  = m
    substitute x' e (App l r)   = App (substitute x' e l) (substitute x' e r)
    substitute x' e ab@(Abs y b)
      | y == x'   = ab           -- binder-ul îl oprește
      | otherwise = Abs y (substitute x' e b)

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2

-- încercăm să evaluăm mai întâi partea stângă
normalStep (App e1 e2)
  | not (isNormalForm e1) = App (normalStep e1) e2
  | otherwise             = App e1 (normalStep e2)

-- în abstracție: evaluăm corpul
normalStep (Abs x body)    = Abs x (normalStep body)

-- variabilă sau macro: nu putem reduce
normalStep other           = other

-- 1.7.
applicativeStep :: Lambda -> Lambda
-- redex: dacă atât funcția cât și argumentul sunt în formă normală → β-reducere
applicativeStep (App (Abs x e1) e2)
  | isNormalForm e1 && isNormalForm e2 = reduce x e1 e2

-- întâi evaluăm funcția (partea stângă)
applicativeStep (App e1 e2)
  | not (isNormalForm e1) = App (applicativeStep e1) e2
  -- apoi evaluăm argumentul (partea dreaptă)
  | not (isNormalForm e2) = App e1 (applicativeStep e2)
  -- altfel, nu se poate aplica reducerea
  | otherwise             = App e1 e2

-- dacă e o abstracție, evaluăm corpul
applicativeStep (Abs x body) = Abs x (applicativeStep body)

-- variabilă sau macro: nu se reduce
applicativeStep other        = other

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step expr = expr : unfold expr
  where
    -- recursiv aplicăm pasul de evaluare până când ajungem la formă normală
    unfold e
      | isNormalForm e = []              -- dacă e în formă normală, oprim
      | otherwise      = let e' = step e -- altfel, aplicăm un pas
                         in e' : unfold e'

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
