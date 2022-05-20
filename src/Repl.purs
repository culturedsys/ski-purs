module Repl where

import Effect (Effect)
import Effect.Console (error, log)
import Prelude (Unit, bind, discard, ($))
import Ski (parse, toGraph)
import Data.Either (Either(..))

import Node.ReadLine (close, createConsoleInterface, noCompletion, prompt, setLineHandler) as RL

main :: Effect Unit
main = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setLineHandler (lineHandler interface) interface
  RL.prompt interface
  where
    lineHandler interface line = do 
      case line of
        "quit" -> RL.close interface   
        expr -> case parse expr of
          Right e -> log $ toGraph e
          Left e -> error $ e 