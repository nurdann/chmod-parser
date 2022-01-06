{-# LANGUAGE OverloadedStrings #-}
module ChmodParser where

import Data.Text (Text)
import Data.Void (Void)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char (space1, alphaNumChar, char, string)
import Data.Text (pack)
import qualified Text.Megaparsec.Char.Lexer as L

{- Grammar for Unix style permissions
           mode         ::= clause [, clause ...]
           clause       ::= [who ...] [action ...] action
           action       ::= op [perm ...]
           who          ::= a | u | g | o
           op           ::= + | - | =
           perm         ::= r | s | t | w | x | X | u | g | o
-}

data Letter a = A | U | G | O | R | S | T | W | X | X' deriving Show
data Who
data Perm

a :: Letter Who
u :: Letter Who
g :: Letter Who
o :: Letter Who

a = A
u = U
g = G
o = O

u' :: Letter Perm
g' :: Letter Perm
o' :: Letter Perm
r :: Letter Perm
s :: Letter Perm
t :: Letter Perm
w :: Letter Perm
x :: Letter Perm
x' :: Letter Perm

u' = U
g' = G
o' = O
r = R
s = S
t = T
w = W
x = X
x' = X'

data Action = Allow [Letter Perm] | Deny [Letter Perm] | Exact [Letter Perm] deriving Show
data Mod = Clause [Letter Who] [Action] deriving Show


type Parser = Parsec Void Text

sc :: Parser ()
sc = space1 -- no comments

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parseWho :: Parser (Letter Who)
parseWho = do
  who <-  char 'u' <|> char 'g' <|> char 'o' <?> "who"
  pure $ case who of
    'u' -> u
    'g' -> g
    'o' -> o

parsePerm :: Parser (Letter Perm)
parsePerm = do
  perm <- satisfy (\c -> c `elem` ("rwxXst" :: String)) <?> "perm"
  pure $ case perm of
    'r' -> r
    'w' -> w
    'x' -> x
    'X' -> x'
    's' -> s
    't' -> t

parseAction :: Parser Action
parseAction = do
  op <- (char '+' <|> char '-' <|> char '=' <?> "op")
  perms <- many parsePerm
  pure $ case op of
    '+' -> Allow perms
    '-' -> Deny perms
    '=' -> Exact perms

parseMod :: Parser Mod
parseMod = do
  whos <- many parseWho
  actions <- many parseAction
  pure (Clause whos actions)

parseMods :: Parser [Mod]
parseMods = do
  mod <- parseMod
  mods <- many $ do
    void (char ',')
    parseMod
  pure (mod:mods)

parseChmod = (optional sc >> parseMods) <* (optional sc >> hidden eof)

ex = parseTest parseWho (pack "g")
ex1 = parseTest parsePerm (pack "r")
ex2 = parseTest parseAction "+rw"
ex3 = parseTest parseAction "+"
ex4 = parseTest parseMod "g=rw"
ex5 = parseTest parseMod "=Xs"
ex6 = parseTest parseMod "o= "
ex7 = parseTest parseMod " g=w "
ex8 = parseTest parseChmod " g=w  "
ex9 = parseTest parseChmod "u=rx"
