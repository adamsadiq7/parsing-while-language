
{-# LANGUAGE StandaloneDeriving #-}
import TinyBasic
import Yoda
import Data.List (intercalate)
import Data.Char

deriving instance Show Adam


data Adam = FirstName String
          | LastName String
          | FullName String String

adam :: Parser Adam
adam =  (FirstName <$> string "Adam" <* whitespace)
    <|> (LastName <$> string "Sadiq" <* whitespace)
    <|> (FullName <$> string "Adam" <* whitespace <*> string "Sadiq")


parse :: Parser a -> String -> [(String,a)]
parse = (Parser px) ts = px ts
