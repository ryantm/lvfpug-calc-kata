{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Calc where

import Text.Parsec hiding (Parser)
import Text.Parsec.Text
import Data.Text as T


-- $setup
-- >>> :set -XOverloadedStrings

-- | Integer calculator
-- >>> calc "1"
-- Just 1
-- >>> calc "2"
-- Just 2
-- >>> calc "10"
-- Just 10
-- >>> calc " 10"
-- Just 10
-- >>> calc "10 "
-- Just 10
-- >>> calc " 10 "
-- Just 10
-- >>> calc " 1 + 1"
-- Just 2
-- >>> calc " 2 g 2 + 1"
-- Nothing
-- >>> calc " 1 + 2 * 2"
-- Just 5
-- >>> calc " 1 + 2 - 2"
-- Just 1
-- >>> calc " 1 + 2 / 2"
-- Just 2
-- >>> calc " (3 + 3) / 2"
-- Just 3
-- >>> calc " 3 + (3 / 2 )"
-- Just 4
calc :: Text -> Maybe Integer
calc a =
  case p a of
    Left _ -> Nothing
    Right r -> Just r


p i = parse expression "input" i

whitespace = between spaces spaces
parens = between (char '(') (char ')')

expression = do
  r <- term
  eof
  return r

term = chainl1 (chainl1 (chainl1 (chainl1 factor mul) divide) plus) sub
factor = whitespace (parens term <|> number)

plus = do
  char '+'
  return (+)

mul = do
  char '*'
  return (*)

sub = do
  char '-'
  return (-)

divide = do
  char '/'
  return (div)

number = fmap read (many1 digit)
