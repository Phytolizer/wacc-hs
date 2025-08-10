module WaCC.Document (
  Document,
  readDocument,
  parseDocument,
  docLineIndex,
  docPosition,
  docText,
  docContents,
  Span (..),
)
where

import Data.Function ((&))
import Data.Text (Text)

import Data.Text qualified as T
import Data.Text.IO qualified as TIO

data Document = Document
  { path :: FilePath
  , docContents :: Text
  , lineStarts :: [Int]
  {- ^ List of starting indices of each line in the document

  Can be binary-searched for a given offset to determine the line number
  and column number of a character in the document.
  This is useful for error reporting and diagnostics.
  The first element is always 0, representing the start of the document.
  The last element is the total length of the document, and not a real line start.
  The length of this list is the number of lines in the document.
  -}
  }

readDocument :: FilePath -> IO Document
readDocument filePath = do
  text <- TIO.readFile filePath
  return $ parseDocument filePath text

parseDocument :: FilePath -> Text -> Document
parseDocument filePath text =
  let lineLengths = map T.length (T.lines text)
      lineEnds = zipWith (+) lineLengths (replicate (length lineLengths - 1) 1 ++ [0])
      starts = scanl (+) 0 lineEnds
   in Document filePath text starts

docLineIndex :: Document -> Int -> Int
docLineIndex doc offset =
  let left = takeWhile (<= offset) (lineStarts doc)
   in length left - 1

data Position = Position
  { _posFilePath :: FilePath
  , _posLine :: Int
  , _posColumn :: Int
  }

instance Show Position where
  show (Position filePath line column) =
    filePath ++ ":" ++ show (line + 1) ++ ":" ++ show (column + 1)

docPosition :: Document -> Int -> Position
docPosition doc offset =
  let line = docLineIndex doc offset
      column = offset - (lineStarts doc !! line)
   in Position (path doc) line column

{- | Pair of offsets. `spanStart` is inclusive, while `spanEnd` is exclusive.

In other words, `Span 0 5` is the range `[0, 5)`.
-}
data Span = Span
  { spanStart :: Int
  , spanEnd :: Int
  }

instance Show Span where
  show (Span start end) = show start ++ ".." ++ show end

docText :: Document -> Span -> T.Text
docText doc (Span start end) =
  docContents doc
    & T.drop start
    & T.take (end - start)
