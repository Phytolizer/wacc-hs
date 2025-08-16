module WaCC.Syntax.AST (
  Identifier (..),
  Program (..),
  FunctionDefinition (..),
  Statement (..),
  Expression (..),
) where

import WaCC.Document (Span, joinSpans)
import WaCC.Syntax.Token (Token (tokenSpan))
import WaCC.Tree (AnyNode (AnyNode), Node (..))

data Identifier = Identifier Span String
instance Node Identifier where
  nodeType _ = "IDENTIFIER"
  nodeSpan (Identifier s _) = s
  nodeProperties (Identifier _ name) = [("name", name)]
  nodeChildren _ = []

data Program = Program FunctionDefinition
data FunctionDefinition = Function
  { fdName :: Identifier
  , fdBody :: Statement
  }
data Statement = Return Expression
data Expression = Constant Token Int

instance Node Program where
  nodeType _ = "PROGRAM"
  nodeSpan (Program fd) = nodeSpan fd
  nodeProperties _ = []
  nodeChildren (Program fd) = [AnyNode fd]

instance Node FunctionDefinition where
  nodeType _ = "FUNCTION_DEFINITION"
  nodeSpan (Function name body) = joinSpans (nodeSpan name) (nodeSpan body)
  nodeProperties (Function name _) =
    let (Identifier _ nameStr) = name
     in [("name", nameStr)]
  nodeChildren (Function _ body) = [AnyNode body]

instance Node Statement where
  nodeType _ = "STATEMENT"
  nodeSpan (Return expr) = nodeSpan expr
  nodeProperties _ = []
  nodeChildren (Return expr) = [AnyNode expr]

instance Node Expression where
  nodeType _ = "EXPRESSION"
  nodeSpan (Constant t _) = tokenSpan t
  nodeProperties _ = []
  nodeChildren _ = []
