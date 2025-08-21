module Main (main) where

import System.Exit (exitFailure)

import Args (CompileStage (..), Options (..), getOptions)
import WaCC.Diagnostics (getDiagnostics, printDiagnostics)
import WaCC.Document (Document, Span (Span), docPosition, readDocument)
import WaCC.Lexer (LexResult (LexResult), lexDocument)
import WaCC.Syntax.Parser (ParseResult (ParseResult), parseFile)
import WaCC.Syntax.Token (Token (Token))
import WaCC.Tree (AnyNode (AnyNode))

lexerStage :: Document -> LexResult -> IO ()
lexerStage doc (LexResult tokens lexReporter) = do
  putStrLn "Tokens:"
  mapM_
    ( \(Token ty s text trivia) ->
        let Span start _ = s
            pos = docPosition doc start
         in putStrLn $
              "@ "
                ++ show pos
                ++ ": "
                ++ show ty
                ++ " "
                ++ show text
                ++ " ("
                ++ show (length trivia)
                ++ " trivia)"
    )
    tokens
  case getDiagnostics lexReporter of
    [] -> return ()
    diagnostics -> do
      printDiagnostics doc diagnostics
      exitFailure

parserStage :: Document -> ParseResult -> IO ()
parserStage doc (ParseResult program parseReporter) = do
  print $ AnyNode program
  case getDiagnostics parseReporter of
    [] -> return ()
    diagnostics -> do
      printDiagnostics doc diagnostics
      exitFailure

main :: IO ()
main = do
  Options{inputPath = input, stopStage = stage} <- getOptions
  putStrLn $ "Input file: " ++ input
  putStrLn $ "Stop stage: " ++ show stage
  doc <- readDocument input
  let lexResult@(LexResult tokens lexReporter) = lexDocument doc
  case stage of
    Lex -> lexerStage doc lexResult
    _ -> do
      let parseResult@(ParseResult _ _) = parseFile lexReporter tokens
      case stage of
        Parse -> parserStage doc parseResult
        _ -> do
          return ()
