{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc where

import Text.Parsec hiding (Parser)
import Text.Parsec.Text
import Data.Text as T


-- $setup
-- >>> :set -XOverloadedStrings

-- | Integer Calculator
-- >>> calc "1"
-- Just 1
-- >>> calc "2"
-- Just 2
-- >>> calc "10"
-- Just 10
-- >>> calc "10 g"
-- Nothing
-- >>> calc "10"
-- Just 10
-- >>> calc "1+1"
-- Just 2
-- >>> calc "1+1"
-- Just 2
-- >>> calc "1-1"
-- Just 0
-- >>> calc "1*2"
-- Just 2
-- >>> calc "3*2+2"
-- Just 8
-- >>> calc "2+3*2"
-- Just 8
-- >>> calc "3/2"
-- Just 1
-- >>> calc " ( 2 + 3 ) * 2  "
-- Just 10
-- >>> calc " 1 1 "
-- Nothing
-- >>> calc " ( 1 "
-- Nothing
-- >>> calc " ( ( 1 + 2 ) * 2 ) "
-- Just 6
calc :: Text -> Maybe Integer
calc i =
  case p i of
    Left _ -> Nothing
    Right result -> Just result

p i = parse calcParser "input" i

calcParser = do
  r <- expr
  eof
  return r

whitespace = between spaces spaces
parens = between (char '(') (char ')')

expr = chainl1 (chainl1 term mulop) addop

term = whitespace (parens expr <|> number)

addop = add <|> sub
mulop = mul <|> divide

add = char '+' *> return (+)

sub = char '-' *> return (-)

mul = char '*' *> return (*)

divide = char '/' *> return div

number :: Parser Integer
number = read <$> many1 digit
