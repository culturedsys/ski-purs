module Ski
  ( Term(..)
  , evaluate
  , evaluateFully
  , parse
  , parseE
  , toGraph
  , NodeDatum(..)
  , toTree
  )
  where

import Prelude

import Data.Either (Either(..), note)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.String as S
import Effect (Effect) 
import Effect.Exception (error, throwException)

data Term = S | K | I | Placeholder S.CodePoint | Cat Term Term
derive instance eqTerm :: Eq Term

instance Show Term where
    show S = "S"
    show K = "K"
    show I = "I"
    show (Placeholder c) = S.fromCodePointArray [c]
    show (Cat l r@(Cat _ _)) = show l <> "(" <> show r <> ")"
    show (Cat l r) = show l <> show r

evaluate :: Term -> Term
evaluate (Cat I t) = t
evaluate (Cat (Cat K alpha) _) = alpha
evaluate (Cat (Cat (Cat S alpha) beta) gamma) = (Cat (Cat alpha gamma) (Cat beta gamma))
evaluate (Cat l r) = if (evaluate l) /= l then Cat (evaluate l) r else Cat l (evaluate r)
evaluate t = t

evaluateFully :: Term -> Term
evaluateFully t = let next = evaluate t in if next == t then t else evaluateFully next 

push :: Maybe Term -> Maybe Term -> Maybe Term
push Nothing Nothing = Nothing
push Nothing term = term
push term Nothing = term
push (Just prev) (Just term) = Just $ Cat prev term

parse :: String -> Either String Term
parse s = parseImpl s L.Nil Nothing

parseE :: String -> Effect Term
parseE s = case parse s of
  Right t -> pure t
  Left e -> throwException $ error e

parseImpl :: String -> L.List (Maybe Term) -> Maybe Term -> Either String Term
parseImpl s stack t = case S.uncons s of
  Nothing -> note "Empty string" t
  Just { head: c, tail} -> 
    if c == (S.codePointFromChar 'S') then 
      parseImpl tail stack (push t (Just S))
    else if c == (S.codePointFromChar 'K') then
      parseImpl tail stack (push t (Just K))
    else if c == (S.codePointFromChar 'I') then
      parseImpl tail stack (push t (Just I))
    else if c == (S.codePointFromChar '(') then
      parseImpl tail (L.Cons t stack) Nothing
    else if c == (S.codePointFromChar ')') then
      case L.uncons stack of
        Nothing -> Left "Unexpected ')'"
        Just { head: top, tail: stack' } -> parseImpl tail stack' (push top t)
    else if c >= (S.codePointFromChar 'a') && c <= (S.codePointFromChar 'z') then
      parseImpl tail stack (push t (Just (Placeholder c)))
    else 
      Left $ "Unexpected '" <> (S.fromCodePointArray [c]) <> "'"
    

type PrintResult = { s :: String, id :: String, counter :: Int }
toGraph :: Term -> String
toGraph t = let {s, id: _, counter: _ } = toGraphImpl t 0 in s

nodeDef :: String -> String -> String -> Int -> PrintResult
nodeDef tag label shape counter =
  let id = tag <> (show $ counter + 1)
      s = id <> " [label=" <> label <> " shape=" <> shape <> "]" in
      { s, id, counter: counter + 1 }

toGraphImpl :: Term -> Int -> PrintResult
toGraphImpl (Placeholder c) counter = nodeDef "s" (S.fromCodePointArray [c]) "square" counter
toGraphImpl S counter = nodeDef "c" "S" "ellipse" counter
toGraphImpl K counter = nodeDef "c" "K" "ellipse" counter
toGraphImpl I counter = nodeDef "c" "I" "ellipse" counter
toGraphImpl (Cat l r) counter =
  let { s: sL, id: idL, counter: counterL } = toGraphImpl l counter
      { s: sR, id: idR, counter: counterR } = toGraphImpl r counterL
      id = "n" <> (show $ counterR + 1)
      s = id <> " [shape=point]\n" <> id <> " -- {" <> idL <> "," <> idR <> "}" in
    {s: sL <> "\n" <> sR <> "\n" <> s, id, counter: counterR + 1 }


newtype NodeDatum = NodeDatum {
  name :: String,
  children :: Array NodeDatum
}

derive instance eqNodeDatum :: Eq NodeDatum
instance Show NodeDatum where 
  show (NodeDatum { name, children }) = "{ " <> "name: \"" <> name <> "\", children: " <> (show children) <> " }"

toTree :: Term -> NodeDatum
toTree (Placeholder c) = NodeDatum { name: S.fromCodePointArray [c], children: [] }
toTree S = NodeDatum { name: "S", children: [] }
toTree K = NodeDatum { name: "K", children: [] }
toTree I = NodeDatum { name: "I", children: [] }
toTree (Cat l r) = NodeDatum { name: "", children: [(toTree l), (toTree r)] }

-- SKI S(KI)

-- (Cat (Cat S K) I)

-- (Cat )