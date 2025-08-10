module Main (main) where

import System.Exit (exitFailure)

import Args (CompileStage (Lex), Options (..), getOptions)
import WaCC.Diagnostics (printDiagnostics)
import WaCC.Document (Span (Span), docPosition, readDocument)
import WaCC.Lexer (LexResult (LexResult), Token (Token), lexDocument)

main :: IO ()
main = do
  Options{inputPath = input, stopStage = stage} <- getOptions
  putStrLn $ "Input file: " ++ input
  putStrLn $ "Stop stage: " ++ show stage
  doc <- readDocument input
  let LexResult tokens diagnostics = lexDocument doc
   in case stage of
        Lex -> do
          putStrLn "Tokens:"
          mapM_
            ( \(Token ty s text) ->
                let Span start _ = s
                    pos = docPosition doc start
                 in putStrLn $ "@ " ++ show pos ++ ": " ++ show ty ++ " " ++ show text
            )
            tokens
          case diagnostics of
            [] -> return ()
            _ -> do
              printDiagnostics doc diagnostics
              exitFailure
        _ -> return ()
