module WaCC.Diagnostics.Reports (
  reportUnrecognizedToken,
  reportInvalidIdentifier,
) where

import Data.Text qualified as T

import WaCC.Diagnostics.Types (
  Diagnostic (Diagnostic),
  DiagnosticReporter (_diagnostics),
  Message,
 )
import WaCC.Document (Span)

report :: Message -> Span -> DiagnosticReporter -> DiagnosticReporter
report m s reporter =
  reporter{_diagnostics = Diagnostic s m : _diagnostics reporter}

reportUnrecognizedToken :: Char -> Span -> DiagnosticReporter -> DiagnosticReporter
reportUnrecognizedToken c = report ("Unrecognized token: '" ++ [c] ++ "'")

reportInvalidIdentifier :: T.Text -> Span -> DiagnosticReporter -> DiagnosticReporter
reportInvalidIdentifier ident = report ("Invalid identifier: " ++ show (T.unpack ident))
