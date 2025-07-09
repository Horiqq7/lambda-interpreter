module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
bTrue  = Abs "x" $ Abs "y" $ vx
bFalse = Abs "x" $ Abs "y" $ vy

-- AND = λp q. p q p
bAnd = Abs "p" $ Abs "q" $
           App (App (Var "p") (Var "q")) (Var "p")

-- OR  = λp q. p p q
bOr  = Abs "p" $ Abs "q" $
           App (App (Var "p") (Var "p")) (Var "q")

-- NOT = λp. p FALSE TRUE
bNot = Abs "p" $
           App (App (Var "p") bFalse) bTrue

-- XOR = λp q. p (NOT q) q
bXor = Abs "p" $ Abs "q" $
            App (App (Var "p")
                     (App bNot (Var "q")))
                (Var "q")

-- 4.2. Pair encodings
-- PAIR = λx y f. f x y
pair = Abs "x" $ Abs "y" $
           Abs "f" $ App (App (Var "f") (Var "x")) (Var "y")

-- FIRST  = λp. p (λx y. x)
first = Abs "p" $
            App (Var "p")
                (Abs "x" $ Abs "y" $ Var "x")

-- SECOND = λp. p (λx y. y)
second = Abs "p" $
             App (Var "p")
                 (Abs "x" $ Abs "y" $ Var "y")

-- 4.3. Natural number (Church) encodings
-- N0  = λf x. x
n0 = Abs "f" $ Abs "x" $ vx

-- N1  = λf x. f x
n1 = Abs "f" $ Abs "x" $ App vf vx

-- N2  = λf x. f (f x)
n2 = Abs "f" $ Abs "x" $ App vf (App vf vx)

-- SUCC = λn f x. f (n f x)
nSucc = Abs "n" $ Abs "f" $ Abs "x" $
            App vf (App (App (Var "n") vf) vx)

-- PRED = λn f x. n (λg h. h (g f)) (λu. x) (λu. u)
nPred = Abs "n" $ Abs "f" $ Abs "x" $
            App
              (App
                (App (Var "n")
                     (Abs "g" $ Abs "h" $
                         App (Var "h") (App (Var "g") vf)))
                (Abs "u" $ vx))
              (Abs "u" $ Var "u")

-- ADD  = λm n f x. m f (n f x)
nAdd = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" $
           App
             (App vm vf)
             (App (App vn vf) vx)

-- SUB  = λm n. n PRED m
nSub = Abs "m" $ Abs "n" $
           App (App vn nPred) vm

-- MULT = λm n f. m (n f)
nMult = Abs "m" $ Abs "n" $ Abs "f" $
            App vm (App vn vf)

-- Default Context
defaultContext :: Context
defaultContext =
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    , ("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]