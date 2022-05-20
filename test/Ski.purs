module Test.Main where

import Prelude
import Ski

import Data.Either (Either(Right))
import Data.String as S
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Ski" do
    let x = Placeholder (S.codePointFromChar 'x')
    let y = Placeholder (S.codePointFromChar 'y')
    let z = Placeholder (S.codePointFromChar 'z')
    describe "evaluate" do
      it "should evaluate a symbol to itself" do
        (evaluate x) `shouldEqual` x    
      it "should evaluate I" do
        (evaluate (Cat I x)) `shouldEqual` x
      it "should evaluate K" do
        (evaluate (Cat (Cat K x) y)) `shouldEqual` x
      it "should evaluate S" do
        (evaluate (Cat (Cat (Cat S x) y) z)) `shouldEqual` (Cat (Cat x z) (Cat y z))
    
    describe "parse" do
      it "should parse a symbol" do
        parse "x" `shouldEqual` (Right x)
      it "should parse I" do
        parse "I" `shouldEqual` (Right I)
      it "should parse K" do
        parse "K" `shouldEqual` (Right K)
      it "should parse S" do
        parse "S" `shouldEqual` (Right S)
      it "should parse a sequence" do
        parse "SKx" `shouldEqual` (Right (Cat (Cat S K) x))
      it "shoud parse brackets" do
        parse "S(Kx)" `shouldEqual` (Right (Cat S (Cat K x)))

    describe "toGraph" do
      it "should toGraph a symbol" do
        let printed = toGraph (Placeholder (S.codePointFromChar 'x'))
        printed `shouldEqual` "s1 [label=x shape=square]"
      it "should toGraph a combinator" do
        let printed = toGraph S
        printed `shouldEqual` "c1 [label=S shape=ellipse]"
      it "should toGraph a cat" do
        let printed = toGraph (Cat S K)
        printed `shouldEqual` "c1 [label=S shape=ellipse]\nc2 [label=K shape=ellipse]\nn3 [shape=point]\nn3 -- {c1,c2}"

    describe "toTree" do
      it "should create a tree for a symbol" do
        toTree (Placeholder (S.codePointFromChar 'x')) `shouldEqual` NodeDatum { name: "x", children: [] }
      it "should create a tree for a combinator" do
        toTree S `shouldEqual` NodeDatum { name: "S", children: [] }
      it "should create a tree for a cat" do
        toTree (Cat S K) `shouldEqual` NodeDatum { 
          name: "", 
          children: [NodeDatum { name: "S", children: [] }, NodeDatum { name: "K", children: [] }] 
        }