{-# LANGUAGE TemplateHaskell #-}

module WaCC.Lexer (
  lexDocument,
  LexResult (..),
)
where

import Control.Lens (
  makeLensesFor,
  use,
  (%=),
  (+=),
  (.=),
  (<~),
 )
import Control.Monad (when)
import Control.Monad.State (State, evalState)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Prelude hiding (lex, span)

import Data.Text qualified as T

import WaCC.Diagnostics (
  DiagnosticReporter,
  initReporter,
  reportInvalidIdentifier,
  reportUnrecognizedToken,
 )
import WaCC.Document (Document (docContents), Span (Span))
import WaCC.Syntax.Token (Keyword (..), Token (..), TokenType (..))

data LexerState = LexerState
  { _lexDoc :: Document
  , _lexText :: T.Text
  , _lexTokenStart :: Int
  , _lexCurrentOffset :: Int
  , _lexPeekBuffer :: [Char]
  , _lexTokenType :: TokenType
  , _lexTokenText :: T.Text
  , _reporter :: DiagnosticReporter
  , _lexHasReported :: Bool
  }

makeLensesFor
  [ ("_lexText", "lexText")
  , ("_lexTokenStart", "lexTokenStart")
  , ("_lexCurrentOffset", "lexCurrentOffset")
  , ("_lexPeekBuffer", "lexPeekBuffer")
  , ("_lexTokenType", "lexTokenType")
  , ("_lexTokenText", "lexTokenText")
  , ("_reporter", "reporter")
  , ("_lexHasReported", "lexHasReported")
  ]
  ''LexerState

data LexResult = LexResult
  { lexTokens :: [Token]
  , lexReporter :: DiagnosticReporter
  }

type Lexer a = State LexerState a

initLexer :: Document -> LexerState
initLexer doc =
  LexerState
    doc
    (docContents doc)
    0
    0
    []
    TError
    T.empty
    initReporter
    False

lexDocument :: Document -> LexResult
lexDocument = evalState lexInner . initLexer

peek :: Int -> Lexer (Maybe Char)
peek offset = do
  pb <- use lexPeekBuffer
  text <- use lexText
  let totalLen = length pb + T.length text
  if offset < length pb
    then do
      let c = pb !! offset
      return $ Just c
    else
      if offset < totalLen
        then do
          let needed = offset - length pb + 1
              newChars = T.unpack $ T.take needed text
              updatedBuffer = pb ++ newChars
          lexPeekBuffer .= updatedBuffer
          lexText %= T.drop needed
          let c = updatedBuffer !! offset
          return $ Just c
        else return Nothing

current :: Lexer (Maybe Char)
current = peek 0

move :: Int -> Lexer ()
move n = do
  lexCurrentOffset += n
  -- pop from peekBuffer
  text <- use lexPeekBuffer
  let (peekHd, peekTl) = splitAt n text
  let pfx = T.pack $ peekHd
  lexTokenText %= (flip T.append) pfx
  lexPeekBuffer .= peekTl
  when (T.length pfx < n) $ do
    -- take rest from lexText
    let needed = n - T.length pfx
    text' <- use lexText
    let (hd, tl) = T.splitAt needed text'
    lexTokenText %= (flip T.append) hd
    lexText .= tl

resetToken :: Lexer ()
resetToken = do
  lexTokenStart <~ use lexCurrentOffset
  lexTokenType .= TError
  lexTokenText .= T.empty
  lexHasReported .= False

simpleToken :: Int -> TokenType -> Lexer ()
simpleToken len ty = do
  lexTokenType .= ty
  move len

whitespace :: [Char]
whitespace = " \t\n\r"

eatWhile :: (Char -> Bool) -> Lexer ()
eatWhile p = do
  cur <- current
  case cur of
    Just c
      | p c -> do
          move 1
          eatWhile p
    _ -> return ()

lexBlockComment :: Lexer ()
lexBlockComment = do
  move 2
  go
 where
  go :: Lexer ()
  go = do
    c1 <- current
    c2 <- peek 1
    case (c1, c2) of
      (Just '*', Just '/') -> do
        move 2
        return ()
      _ -> do
        move 1
        go

lexWhitespace :: Lexer ()
lexWhitespace = do
  lexTokenType .= TWhitespace
  eatWhile (`elem` whitespace)
  cur <- current
  case cur of
    Just '/' -> do
      next <- peek 1
      case next of
        Just '*' -> do
          lexBlockComment
          lexWhitespace
        Just '/' -> do
          eatWhile (/= '\n')
          lexWhitespace
        _ -> return ()
    _ -> return ()

nextToken :: Lexer Token
nextToken = do
  resetToken
  cur <- current
  case cur of
    Nothing -> simpleToken 0 TEOF
    Just '(' -> simpleToken 1 TLParen
    Just ')' -> simpleToken 1 TRParen
    Just '{' -> simpleToken 1 TLBrace
    Just '}' -> simpleToken 1 TRBrace
    Just ';' -> simpleToken 1 TSemicolon
    Just '/' -> do
      next <- peek 1
      case next of
        Just '*' -> do
          lexBlockComment
          lexWhitespace
        Just '/' -> do
          eatWhile (/= '\n')
          lexWhitespace
        _ -> pure () -- error reported later
    Just c | c `elem` whitespace -> lexWhitespace
    Just c | isAlpha c -> do
      eatWhile isAlphaNum
      text <- use lexTokenText
      let ty = case T.unpack text of
            "int" -> TKeyword KInt
            "void" -> TKeyword KVoid
            "return" -> TKeyword KReturn
            _ -> TIdentifier
      lexTokenType .= ty
    Just c | isDigit c -> do
      eatWhile isDigit
      -- special case: int followed by identifier is illegal
      -- since it's ambiguous, report as an invalid identifier
      pc <- current
      case pc of
        Just c2 | isAlpha c2 -> do
          eatWhile isAlphaNum
          text <- use lexTokenText
          start <- use lexTokenStart
          end <- use lexCurrentOffset
          reporter %= reportInvalidIdentifier text (Span start end)
          lexHasReported .= True
        _ -> lexTokenType .= TIntLiteral
    Just _ -> pure ()

  ty <- use lexTokenType
  start <- use lexTokenStart
  end <- use lexCurrentOffset
  hasReported <- use lexHasReported
  span <-
    if hasReported || ty /= TError
      then pure (Span start end)
      else do
        errch <- current
        (ch, n) <- case errch of
          Just ch -> do
            move 1
            return (ch, 1)
          Nothing -> pure ('\0', 0)

        let s = Span start (end + n)
        reporter %= reportUnrecognizedToken ch s
        return s
  text <- use lexTokenText
  return $ Token ty span text

lex :: Lexer [Token]
lex = go []
 where
  go acc = do
    token <- nextToken
    if tokenType token == TEOF
      then return $ reverse acc
      else go (token : acc)

lexInner :: Lexer LexResult
lexInner = do
  tokens <- lex
  r <- use reporter
  return $ LexResult tokens r
