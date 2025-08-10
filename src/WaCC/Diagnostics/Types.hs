module WaCC.Diagnostics.Types (
  Message,
  Diagnostic (Diagnostic),
  DiagnosticReporter (_diagnostics),
  initReporter,
  getDiagnostics,
  printDiagnostics,
  hPrintDiagnostics,
  printDiagnostic,
  hPrintDiagnostic,
) where

import System.IO (Handle, hPutStrLn, stderr)
import WaCC.Document (Document, Span (Span), docPosition)

type Message = String

data Diagnostic = Diagnostic Span Message

data DiagnosticReporter = DiagnosticReporter
  { _diagnostics :: [Diagnostic]
  }

initReporter :: DiagnosticReporter
initReporter = DiagnosticReporter []

-- | Return them in the order they were reported, requires a list-reverse
getDiagnostics :: DiagnosticReporter -> [Diagnostic]
getDiagnostics = reverse <$> _diagnostics

printDiagnostics :: Document -> [Diagnostic] -> IO ()
printDiagnostics = hPrintDiagnostics stderr

hPrintDiagnostics :: Handle -> Document -> [Diagnostic] -> IO ()
hPrintDiagnostics h doc = mapM_ (hPrintDiagnostic h doc)

printDiagnostic :: Document -> Diagnostic -> IO ()
printDiagnostic = hPrintDiagnostic stderr

hPrintDiagnostic :: Handle -> Document -> Diagnostic -> IO ()
hPrintDiagnostic h doc (Diagnostic (Span start _end) m) = do
  let pos = docPosition doc start
  hPutStrLn h $ show pos ++ ": ERROR: " ++ m
  return ()
