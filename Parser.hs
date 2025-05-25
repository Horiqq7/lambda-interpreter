module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char
import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- Functor: permite aplicarea unei funcții pe rezultatul parsării
instance Functor Parser where
  fmap f p = Parser $ \s -> do
    (x, s') <- parse p s
    return (f x, s')

-- Combinatori de parsare
instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  pf <*> px = Parser $ \s -> do
    (f, s1) <- parse pf s
    (x, s2) <- parse px s1
    return (f x, s2)

-- Permite alternative (|) între parsere
instance Alternative Parser where
  empty = Parser $ const Nothing
  p <|> q = Parser $ \s -> parse p s <|> parse q s

-- Monad: permite parsarea în lanț cu dependențe între pași
instance Monad Parser where
  p >>= f = Parser $ \s -> do
    (x, s') <- parse p s
    parse (f x) s'

-- Parser care extrage un caracter
item :: Parser Char
item = Parser $ \s -> case s of
  []     -> Nothing
  (c:cs) -> Just (c, cs)

-- Parser pentru un caracter care respectă o condiție
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  c <- item
  guard (p c)
  return c

-- Parser pentru un caracter fix
char :: Char -> Parser Char
char c = satisfy (== c)

-- Ignoră spațiile
spaces :: Parser ()
spaces = void $ many (satisfy isSpace)

-- Parser pentru un șir de caractere
symbol :: String -> Parser String
symbol str = spaces *> traverse char str <* spaces

-- Se asigură că am ajuns la finalul șirului
eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> Just ((), "")
  _  -> Nothing

-- Parsare variabilă: un șir de litere mici
pVar :: Parser String
pVar = spaces *> liftA2 (:) (satisfy isLower) (many (satisfy isLower)) <* spaces

-- Parsare macro: un șir de litere mari și cifre
pMacro :: Parser Lambda
pMacro = do
  spaces
  name <- liftA2 (:) (satisfy isUpperOrDigit) (many (satisfy isUpperOrDigit))
  spaces
  return (Macro name)
  where
    isUpperOrDigit c = isUpper c || isDigit c

-- Parsare expresie între paranteze
pParens :: Parser Lambda
pParens = do
  symbol "("
  l <- pLambda
  symbol ")"
  return l

-- Parsare abstractizare: \x.e sau λx.e
pAbs :: Parser Lambda
pAbs = do
  spaces
  _ <- char '\\' <|> char 'λ'
  v <- pVar
  _ <- char '.'
  b <- pLambda
  return (Abs v b)

-- Parsare aplicație: (e1 e2)
pApp :: Parser Lambda
pApp = do
  symbol "("
  f <- pLambda
  a <- pLambda
  symbol ")"
  return (App f a)

-- Parsor general pentru expresii lambda
pLambda :: Parser Lambda
pLambda =
      pAbs
  <|> pApp
  <|> pMacro
  <|> (Var <$> pVar)
  <|> pParens

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda s = case parse (pLambda <* spaces <* eof) s of
  Just (lam,_) -> lam
  Nothing      -> error ("parse error in parseLambda: " ++ show s)

pLineBinding :: Parser Line
pLineBinding = do
  name <- spaces *> liftA2 (:) (satisfy (\c->isUpper c||isDigit c)) (many (satisfy (\c->isUpper c||isDigit c))) <* spaces
  _    <- symbol "="
  lam  <- pLambda
  _    <- spaces *> eof
  return (Binding name lam)

pLineEval :: Parser Line
pLineEval = do
  lam <- pLambda
  _   <- spaces *> eof
  return (Eval lam)

-- 3.3.
parseLine :: String -> Either String Line
parseLine s = case parse pLineBinding s of
  Just (ln,_) -> Right ln
  Nothing     -> case parse pLineEval s of
    Just (ln,_) -> Right ln
    Nothing     -> Left ("parse error in parseLine: " ++ show s)
