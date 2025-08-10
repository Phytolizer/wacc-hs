module WaCC.Diagnostics.Reports (
  reportUnrecognizedToken,
  reportInvalidIdentifier,
) where

import WaCC.Diagnostics.Types (
  Diagnostic (Diagnostic),
  DiagnosticReporter (_diagnostics),
  Message,
 )
import WaCC.Document (Span)

import qualified Data.Text as T

report :: Message -> Span -> DiagnosticReporter -> DiagnosticReporter
report m s reporter =
  reporter{_diagnostics = Diagnostic s m : _diagnostics reporter}

reportUnrecognizedToken :: Char -> Span -> DiagnosticReporter -> DiagnosticReporter
reportUnrecognizedToken c = report ("Unrecognized token: '" ++ [c] ++ "'")

reportInvalidIdentifier :: T.Text -> Span -> DiagnosticReporter -> DiagnosticReporter
reportInvalidIdentifier ident = report ("Invalid identifier: " ++ show (T.unpack ident))
