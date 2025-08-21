{-# LANGUAGE TemplateHaskell #-}

module WaCC.Syntax.Parser (
  parseFile,
  ParseResult (..),
) where

import Control.Lens (makeLensesFor, use, (%=))
import Control.Monad (when)
import Control.Monad.State (State, evalState)
import Data.List ((!?))
import Data.Maybe (fromMaybe)
import Prelude hiding (exp)

import Data.Text qualified as T

import WaCC.Diagnostics.Reports (reportUnexpectedToken)
import WaCC.Diagnostics.Types (DiagnosticReporter)
import WaCC.Syntax.AST (
  Expression (Constant),
  FunctionDefinition (Function),
  Identifier (Identifier),
  Program (Program),
  Statement (Return),
 )
import WaCC.Syntax.Token (Keyword (..), Token (..), TokenType (..))

data ParserState = ParserState
  { _pTokens :: [Token]
  , _pReporter :: DiagnosticReporter
  }
makeLensesFor
  [ ("_pTokens", "pTokens")
  , ("_pReporter", "pReporter")
  ]
  ''ParserState

type Parser a = State ParserState a

data ParseResult = ParseResult
  { parseRoot :: Program
  , parseReporter :: DiagnosticReporter
  }

initParser :: DiagnosticReporter -> [Token] -> ParserState
initParser reporter tokens =
  ParserState
    tokens
    reporter

peek :: Int -> Parser Token
peek n = do
  tokens <- use pTokens
  return $ fromMaybe (last tokens) (tokens !? n)

current :: Parser Token
current = peek 0

matchToken :: TokenType -> Parser Token
matchToken expected = do
  token <- current
  if tokenType token == expected
    then do
      when (tokenType token /= TEOF) (pTokens %= drop 1)
      return token
    else do
      pReporter %= reportUnexpectedToken (tokenType token) expected (tokenSpan token)
      return (Token expected (tokenSpan token) T.empty [])

parseExpression :: Parser Expression
parseExpression = do
  token <- matchToken TIntLiteral
  let value :: Int = read (T.unpack $ tokenText token)
  return $ Constant token value

parseStatement :: Parser Statement
parseStatement = do
  _ <- matchToken $ TKeyword KReturn
  exp <- parseExpression
  _ <- matchToken TSemicolon
  return $ Return exp

parseFunction :: Parser FunctionDefinition
parseFunction = do
  _ <- matchToken $ TKeyword KInt
  nameToken <- matchToken TIdentifier
  let name = Identifier (tokenSpan nameToken) (T.unpack $ tokenText nameToken)
  _ <- matchToken TLParen
  _ <- matchToken $ TKeyword KVoid
  _ <- matchToken TRParen
  _ <- matchToken TLBrace
  statement <- parseStatement
  _ <- matchToken TRBrace
  return $ Function name statement

parseProgram :: Parser Program
parseProgram = do
  function <- parseFunction
  return $ Program function

parseFile :: DiagnosticReporter -> [Token] -> ParseResult
parseFile reporter tokens = evalState go $ initParser reporter tokens
 where
  go :: Parser ParseResult
  go = do
    program <- parseProgram
    _ <- matchToken TEOF
    reporter' <- use pReporter
    return $ ParseResult program reporter'
