module Mu.Evaluator (Evaluator, Aliases, evaluate, reduce, alphaConversion, betaReduction) where

import Control.Monad.State
import qualified Data.Map.Strict as M
import Mu.Parser

alphaConversion :: AST -> Identifier -> AST -> AST
alphaConversion (Variable n) v r
  | n == v    = r
  | otherwise = Variable n
alphaConversion (Application f a) v r =
  let f' = alphaConversion f v r
      a' = alphaConversion a v r
   in Application f' a'
alphaConversion (Abstraction n b) v r
  | n == v    = Abstraction n b
  | otherwise =
    let b' = alphaConversion b v r
     in Abstraction n b'

betaReduction :: AST -> AST
betaReduction (Variable v) = Variable v
betaReduction (Abstraction v b) = Abstraction v (betaReduction b)
betaReduction (Application f a) =
  case betaReduction f of
    Abstraction v b -> alphaConversion b v a
    x               -> Application x (betaReduction a)

-- | Recursively reduce the tree until no more changes can be made.
reduce :: AST -> AST
reduce ast =
  let ast' = betaReduction ast
   in if ast /= ast'
        then reduce ast'
        else ast

type Aliases = M.Map Identifier AST

type Evaluator = State Aliases AST

evaluate :: Aliased -> Evaluator
evaluate (Aliased n e) = do
  e' <- eval e
  modify $ M.insert n e'
  return e'
evaluate (Unaliased e) = eval e

eval :: AST -> Evaluator
eval e = do
  as <- M.toList <$> get
  return . reduce $ foldl (uncurry . alphaConversion) e as
