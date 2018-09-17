{-# LANGUAGE StandaloneDeriving #-}

import Yoda
import Data.Char

--Question 5-1-1-a--

parseHelloWorld :: Parser String
parseHelloWorld = string "Hello World"

--Question 5-1-1-b--

parseMany :: Parser [String]
parseMany = many (string "7")

--Question 5-1-c--

parseSome :: Parser [String]
parseSome = some (string "7")

--Question 5-1-1-d--

parseOneOf :: Parser [Char]
parseOneOf = many (oneOf (['7','5']))

--Question 5-1-1-e--

-- parseNoneOf :: Parser Char
-- parseNoneOf = not . oneOf (['7','5'])
--
-- --Question 5-1-e--
--
weird :: Parser [Char]
weird = try parseOneOf

--Question 5-1-2--

whitespace' :: Parser ()
whitespace' = many (oneOf([' ','\t'])) *> pure ()

--Question 5-1-3--

token :: String -> Parser String
token x = whitespace' *> string x <* whitespace'


number :: Parser Int
number = many (oneOf(['0'..'9'])) >>= return . read

--Question 5-2--

deriving instance Show Robot


data Robot  = Forwards Int Robot
            | Rights Robot
            | Lefts Robot
            | Stops


robot :: Parser Robot
robot = (Forwards <$ token "Forwards" <*> number <*> robot)
     <|> (Rights <$ token "Rights" <* whitespace' <*> robot)
     <|> (Lefts <$  token "Lefts" <* whitespace' <*> robot)
     <|> (Stops <$ token "Stops" <* eof)


data DataB = B DataB Int
           | A Int
           | D


parseBad :: Parser DataB
parseBad =  B <$> parseBad <*> number
        <|> A ⟨$ tok "a" <*> number
        <|> D ⟨$ tok ""






-- parseRobot :: Parser Robot
-- parseRobot = Forwards <$ tok "Forward" <*> number <* whitespace' <*> parseRobot <|> Rights <$ tok "Right" <*> parseRobot <|> Lefts <$ tok "Left" <*> parseRobot <|> Stops <$ tok "Stop"
