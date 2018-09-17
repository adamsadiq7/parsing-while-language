{-# LANGUAGE StandaloneDeriving #-}
import TinyBasic
import Yoda
import Data.List (intercalate)
import Data.Char

deriving instance Show Arithmetic
deriving instance Show Boolean
deriving instance Show Statement
deriving instance Show Expr'
deriving instance Show Conj
deriving instance Show Disj
deriving instance Show Term'





type N = String
type X = String
type A = String
type Y = String


data Arithmetic = Arithmetic Int
               | N N
               | X X
               | Y Y
               | ADD Arithmetic Arithmetic
               | MULT Arithmetic Arithmetic
               | SUB Arithmetic Arithmetic


data Boolean = True'
             | False'
             | EQ' Arithmetic Arithmetic
             | LEQ' Arithmetic Arithmetic
             | NOT' Boolean
             | AND' Boolean Boolean


data Statement = ASSIGNMENT String Arithmetic
               | SKIP
               | SEQ Statement Statement
               | IF' Boolean Statement Statement
               | WHILE' Boolean Statement


data Expr' = Expr' Conj [Conj]
data Conj = Conj Disj [Disj]
data Disj = Disj Term' [Term']
data Term' = T | F | Not Expr' | Bracket Expr'

arithmetic :: Parser Arithmetic
arithmetic =  (Arithmetic <$> number)
          <|> (N <$> string "n")
          <|> (X <$> string "x")
          <|> (Y <$> string "y")
          <|> (ADD <$> ((Arithmetic <$> number) <|> (N <$> string "n") <|> (X <$> string "x")) <* string "+" <*> arithmetic)
          <|> (MULT <$> ((Arithmetic <$> number) <|> (N <$> string "n") <|> (X <$> string "x")) <* string "*" <*> arithmetic)
          <|> (SUB <$> ((Arithmetic <$> number) <|> (N <$> string "n") <|> (X <$> string "x")) <* string "-" <*> arithmetic)

boolean :: Parser Boolean
boolean =  (True' <$ tok' "True")
       <|> (False' <$ tok' "False")
       <|> (EQ' <$> arithmetic <* string "==" <*> arithmetic)
       <|> (LEQ' <$> arithmetic <* string "≤" <*> arithmetic)
       <|> (NOT' <$ string "~" <*> boolean)
       <|> (AND' <$> ((True' <$ string "True") <|> (False' <$ string "False") <|> (EQ' <$> arithmetic <* string "==" <*> arithmetic) <|> (LEQ' <$> arithmetic <* string "≤" <*> arithmetic)) <* string "^" <*> boolean)

statement :: Parser Statement
statement =  (ASSIGNMENT <$> string "x" <* string ":=" <*> arithmetic)
         <|> (SKIP <$ string "SKIP")
         <|> (SEQ <$> ((ASSIGNMENT <$> string "x" <* string ":=" <*> arithmetic) <|> (ASSIGNMENT <$> string "y" <* string ":=" <*> arithmetic) <|> (SKIP <$ string "SKIP")) <* string ";" <*> statement)
         <|> (IF' <$ tok' "IF" <* whitespace <*> boolean <* whitespace <* string "THEN" <* whitespace <*> statement <* whitespace <* string "ELSE" <* whitespace <*> statement)
         <|> (WHILE' <$ tok' "WHILE" <* whitespace <*> boolean <* string "DO" <* whitespace <*> statement)

tok' :: String -> Parser String
tok' t = whitespace *> string t <* whitespace

findAdam :: Parser String
findAdam = (tok' "Adam") <|> (tok' "Sadiq") <* eof

expr' :: Parser Expr'
expr' = (Expr' <$> conj <*> many ((tok "=>" *>) conj))

conj :: Parser Conj
conj = (Conj <$> disj  <*> many((tok "&&") *> disj))

disj :: Parser Disj
disj = (Disj <$> term' <*> many((tok' "||" ) *>term'))

term' :: Parser Term'
term' =  (T <$ tok' "T")
    <|> (F <$ tok' "F")
    <|> (Not <$ tok' "!" <*> expr')
    <|> (Bracket <$ tok' "(" <*> expr' <* string ")")

--alwaysTrue :: Parser Bool
--alwaysTrue = pure True
