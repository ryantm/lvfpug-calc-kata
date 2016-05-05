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
-- >>> calc " 10"
-- Just 10
-- >>> calc " 10 2"
-- Nothing
-- >>> calc " 10 + 2"
-- Just 12
-- >>> calc " 10 + 5 "
-- Just 15
-- >>> calc " 10 - 5 "
-- Just 5
-- >>> calc " 5 - 10 "
-- Just (-5)
-- >>> calc " 5 + 5 - 5 "
-- Just 5
-- >>> calc " 5 * 5 - 5 "
-- Just 20
-- >>> calc " 5 + 5 * 5 "
-- Just 30
-- >>> calc " 5 + 5 / 5 "
-- Just 6
-- >>> calc "(5 + 5)/ 5 "
-- Just 2
-- >>> calc " ( 5 + 5)/ 5 "
-- Just 2
-- >>> calc " ( 5 + 5)/ 5 "
-- Just 2
-- >>> calc " 6 / 3 * 2 "
-- Just 1
calc :: Text -> Maybe Integer
calc i =
  case p i of
    Left _ -> Nothing
    Right a -> Just a

p s = parse pmain "calculator input" s

pmain = do
  n <- expr
  eof
  return n

expr   = chainl1 (chainl1 (chainl1 (chainl1 factor mult) divide) plus) minus
factor = whitespace (parens expr <|> number)

whitespace = between spaces spaces
parens = between (char '(') (char ')')

number :: Parser Integer
number = fmap read (many1 digit)

plus = do
  char '+'
  return (+)

minus = do
  char '-'
  return (-)

mult = do
  char '*'
  return (*)

divide = do
  char '/'
  return (div)
