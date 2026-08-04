module Lexer (Token(..), lexer) where

import Data.Char (isSpace, isDigit, isAlpha, isAlphaNum)

-- each token is always exactly one of the following:
data Token
    = -- Identifiers and constants both hold data, i.e. they need to keep track of their actual content
      IdentifierToken String 
    | ConstantToken Int      
    | IntKeywordToken
    | VoidKeywordToken
    | RetKeywordToken
    | OpenParToken
    | CloseParToken
    | OpenBraceToken
    | CloseBraceToken
    | SemicolonToken
    | TildeToken
    | NegativeToken
    | DecrementToken
    -- Show allows us to print these new data types, eq allows us to check them for equality
    -- deriving tells the compiler to come up with these functions for us
    deriving (Show, Eq)

-- take a string input and return either an error message or a list of tokens
lexer :: String -> Either String [Token]
-- an empty input returns an empty list of tokens:
lexer [] = Right []

-- check for comments:
lexer ('/':'/':x) = skipLine x
lexer ('/':'*':x) = skipMultiLine x

lexer ('-':'-':x) = addToken (DecrementToken, x)

-- split input into first element c and all remaining elements x
lexer (c:x)
  | isSpace c   = lexer x
  | c == '('    = addToken (OpenParToken, x)
  | c == ')'    = addToken (CloseParToken, x)
  | c == '{'    = addToken (OpenBraceToken, x)
  | c == '}'    = addToken (CloseBraceToken, x)
  | c == ';'    = addToken (SemicolonToken, x)
  | c == '-'    = addToken (NegativeToken, x)
  | c == '~'    = addToken (TildeToken, x)
  | isDigit c   = lexNum (c:x)
  | isAlpha c   = lexWord (c:x)
  | otherwise   = Left (" Error - Invalid character: " ++ [c])

-- accept a tuple containing the new token and the remaining string,
-- return either an error message or a list of tokens:
addToken :: (Token, String) -> Either String [Token]
-- lexer rem is a recursive function call that sends the remaining string back to lexer:
addToken (t, rem) = case lexer rem of
    -- if that results in an error, return the error
    Left error   -> Left error
    -- otherwise add the newly lexed token to the list of tokens:
    -- ( (t: tokens) PREPENDS not appends, but that works because recursion)
    Right tokens -> Right (t : tokens)

-- skip single line comments by looking for new line char:
skipLine :: String -> Either String [Token]
skipLine []       = lexer []
skipLine ('\n':x) = lexer x
skipLine (_:x)    = skipLine x

-- skip multiline comments by looking for matching '*/' pattern:
skipMultiLine :: String -> Either String [Token]
skipMultiLine []          = lexer []
skipMultiLine ('*':'/':x) = lexer x
skipMultiLine (_:x)       = skipMultiLine x


-- accept a string and return either an error message or a list of tokens:
lexNum :: String -> Either String [Token]
lexNum str =
    -- isDigit returns a tuple containing the leading string of digit characters and anything else that follows.
    -- num is the string of digits, rem is the remainder:
    let (num, rem) = span isDigit str

    -- check what comes after the digits:
    in case rem of
        -- if the remainder is non-empty, call the first character of that remainder r. if r is alphanum,
        -- the token can't be a valid one. e.g. 123abc, 123!, 123_, etc are all invalid tokens:
        (r:_) | isAlphaNum r -> Left ("Error - Invalid token: " ++ num ++ [r])
        -- otherwise if no extra character immediately follows, this is a valid token.
        -- read num converts num from a string of digits to an actual int type, which gets passsed back to
        -- addToken along with the remaining string:
        _ -> addToken ((ConstantToken (read num)), rem)

-- accept a char and return a boolean
isAllowedChar :: Char -> Bool
-- allowable characters in words are alphanumeric or underscores ONLY:
isAllowedChar c = isAlphaNum c || c == '_'

-- accept a string and return either an error message or a list of tokens:
lexWord :: String -> Either String [Token]
lexWord str =
    let (word, rem) = span isAllowedChar str in addToken (keywordORnot word , rem)

-- accept a word and return the correct token type, depending on whether the word
-- is a valid keyword or just a regular identifier:
keywordORnot :: String -> Token
-- only int, return, and void are valid keywords so far:
keywordORnot "int"  = IntKeywordToken
keywordORnot "return"  = RetKeywordToken
keywordORnot "void"  = VoidKeywordToken
-- anything else not matching the above gets returned as a regular identifier:
keywordORnot str  = IdentifierToken str