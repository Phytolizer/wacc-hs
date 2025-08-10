module Args (
  Options (..),
  getOptions,
  CompileStage (..),
)
where

import Options.Applicative (
  ParserInfo,
  argument,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  optional,
  progDesc,
  short,
  str,
  strOption,
  switch,
  (<**>),
 )
import Prelude hiding (lex)

data Args = Args
  { inputFile :: String
  , lex :: Bool
  , parse :: Bool
  , codegen :: Bool
  , outputFile :: Maybe String
  }

parseArgs :: ParserInfo Args
parseArgs =
  info
    ( Args
        <$> argument
          str
          ( metavar "FILE"
              <> help "Input file to process"
          )
        <*> switch
          ( long "lex"
              <> short 'l'
              <> help "Perform lexical analysis"
          )
        <*> switch
          ( long "parse"
              <> short 'p'
              <> help "Perform parsing"
          )
        <*> switch
          ( long "codegen"
              <> short 'c'
              <> help "Perform code generation"
          )
        <*> optional
          ( strOption
              ( long "output"
                  <> short 'o'
                  <> metavar "FILE"
                  <> help "Output file"
              )
          )
          <**> helper
    )
    ( fullDesc
        <> progDesc "Compile C source file"
        <> header "A simple C compiler"
    )

data CompileStage
  = Lex
  | Parse
  | Codegen
  | Output String
  deriving (Show, Eq)

data Options = Options
  { inputPath :: String
  , stopStage :: CompileStage
  }

getOptions :: IO Options
getOptions = do
  args <- execParser parseArgs
  let input = inputFile args
      output = case outputFile args of
        Just file -> file
        Nothing -> stripSuffix input
      stage as
        | lex as = Lex
        | parse as = Parse
        | codegen as = Codegen
        | otherwise = Output output
  return
    Options
      { inputPath = input
      , stopStage = stage args
      }

stripSuffix :: String -> String
stripSuffix path =
  let rp = reverse path
      suffixLen = length (takeWhile (/= '.') rp) + 1
   in take (max 0 (length path - suffixLen)) path
