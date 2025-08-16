{-# LANGUAGE LambdaCase #-}

module WaCC.Tree (Node (..), AnyNode (..)) where

import WaCC.Document (Span)

data AnyNode where
  AnyNode :: (Node a) => a -> AnyNode

class Node a where
  nodeType :: a -> String
  nodeSpan :: a -> Span
  nodeChildren :: a -> [AnyNode]
  nodeProperties :: a -> [(String, String)]

instance Show AnyNode where
  show (AnyNode n) = go n 0 True ""
   where
    go :: (Node a) => a -> Int -> Bool -> String -> String
    go node indent isLast acc =
      let
        linePrefix = replicate indent ' ' ++ if isLast then "└── " else "├── "
        nodeInfo = nodeType node ++ " " ++ show (nodeProperties node)
        children = nodeChildren node
        acc' = acc ++ "\n" ++ linePrefix ++ " " ++ nodeInfo ++ "\n"
        numChildren = length children
        indexedChildren = zip [0 ..] children
       in
        foldr
          ( \(index, AnyNode child) acc'' ->
              go child (indent + 2) (index == numChildren - 1) acc''
          )
          acc'
          indexedChildren
