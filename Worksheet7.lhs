> {-# LANGUAGE StandaloneDeriving #-}
> import TinyBasic
> import Yoda
> import Data.List (intercalate)
> import Data.Char
> import Data.Monoid
> import Data.Bool


> data Chess = Turn Move Move Chess
>            | EndGame
>  deriving Show

> data Move = Move MoveP Quant
>           -- Cslt Bool
>           -- End Winner
>  deriving Show

> data Quant = Prom Piece Quant
>            | Chck Quant
>            | Null
>  deriving Show

> data MoveP = Alg Piece Cell
>            -- Smh Cell Cell
>            | AlgDis Piece Cell Cell
>            | Tke Piece Cell
>  deriving Show

> data Winner = White
>             | Black
>             | Draw
>             | AO
>  deriving Show

> data Cell = Cell Char Int
>  deriving (Show,Eq)

> data Piece = King
>            | Queen
>            | Rook
>            | Knight
>            | Bishop
>            | Pawn
>  deriving (Show,Eq)

> validChar :: Parser Char
> validChar = item >>= \t -> if t `elem` ['a'..'h'] then pure t else empty

> validInt :: Parser Int
> validInt = read <$> some (oneOf ['0' .. '8'])

> bool' :: Parser Bool
> bool' = undefined

> chess :: Parser Chess
> chess =  (Turn <$> move <* whitespace <*> move <*> chess) --left recursion
>      <|> (EndGame <$ string "" <* eof)

> move :: Parser Move
> move =  (Move <$> moveP <*> quant)
>     -- <|> (Cslt <$> bool')
>     -- <|> (End <$> winner)

> quant :: Parser Quant
> quant =  (Prom <$> piece <*> quant)
>      <|> (Chck <$ string "+" *> quant)
>      <|> (Null <$ string "")

> moveP :: Parser MoveP
> moveP =  (Alg <$> piece <*> cell)
>      -- <|> (Smh <$> cell <* whitespace <*> cell <* eof)
>      <|> (AlgDis <$> piece <* whitespace <*> cell <* whitespace <*> cell)
>      <|> (Tke <$> piece <*> cell)

> winner :: Parser Winner
> winner =  (White <$ string "White" <* eof)
>       <|> (Black <$ string "Black" <* eof)
>       <|> (Draw <$ string "Draw" <* eof)
>       <|> (AO <$ string "AO" <* eof)

> cell :: Parser Cell
> cell = (Cell <$> validChar <*> validInt)

> piece :: Parser Piece
> piece =  (King <$ string "K")
>      <|> (Queen <$ string "Q")
>      <|> (Rook <$ string "R")
>      <|> (Knight <$ string "N")
>      <|> (Bishop <$ string "B")
>      <|> (Pawn <$ string "P")
