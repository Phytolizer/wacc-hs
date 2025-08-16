module WaCC.Syntax.Token (
  TokenType (..),
  Keyword (..),
  Token (..),
) where

import Data.Text qualified as T

import WaCC.Document (Span)

data TokenType
  = TError
  | TEOF
  | TWhitespace
  | TIdentifier
  | TIntLiteral
  | TKeyword Keyword
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TSemicolon
  deriving (Eq)

data Keyword
  = KInt
  | KVoid
  | KReturn
  deriving (Eq)

instance Show TokenType where
  show TError = "ERROR"
  show TEOF = "EOF"
  show TWhitespace = "WHITESPACE"
  show TIdentifier = "IDENTIFIER"
  show TIntLiteral = "INT_LITERAL"
  show (TKeyword k) = show k
  show TLParen = "LPAREN"
  show TRParen = "RPAREN"
  show TLBrace = "LBRACE"
  show TRBrace = "RBRACE"
  show TSemicolon = "SEMICOLON"

instance Show Keyword where
  show KInt = "KW_INT"
  show KVoid = "KW_VOID"
  show KReturn = "KW_RETURN"

data Token = Token
  { tokenType :: TokenType
  , tokenSpan :: Span
  , tokenText :: T.Text
  }
