{-# OPTIONS_GHC -Wall #-}
module Parser where
import Text.ParserCombinators.Parsec

{-
digit :: Parser Char
string :: String -> Parser String
    parse (string "ab") "" "abcde" = Right "ab"
    
many1 :: Parser a -> Parser [a]
    parse (many1 digit) "" "12rt" = Right "12"
-}

num :: Parser Int
num = read <$> (many1 digit)

--num = do xs <- many1 digit
--         return $ read xs

infOp :: String -> (a->a->a) -> Parser (a->a->a)
--infOp x f = do _ <- string x
--               return f
infOp x f = string x >> return f

paren :: Parser a -> Parser a
--paren p = do _ <- string "("
--             res <- p
--             _ <- string ")"
--             return res
paren p = string "(" *> p <* string ")"
          -- take result of this <*> apply to this
  
addop, mulopp :: Parser (Int -> Int -> Int)
addop = infOp "+" (+) <|> infOp "-" (-)


mulopp = infOp "*" (*)

factor :: Parser Int
factor = num <|> paren expr --num


term,expr :: Parser Int
term = chainl1 factor mulopp

expr = chainl1 term addop

full :: Parser Int
--full = do v <- expr
--          eof
--          return v

full = expr <* eof
-- <*    =      do x <- _
--                 return x
