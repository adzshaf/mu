module Main (main) where

import System.IO
import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List (intercalate)
import Text.Megaparsec
import Mu.Parser
import Mu.Evaluator
import Mu.Util

run :: Aliases -> T.Text -> IO Aliases
run as source =
  case runParser program "repl" source of
    Left e -> do
      putStrLn $ errorBundlePretty e
      return as
    Right exprs -> do
      let (res, as') = runState (sequence $ map evaluate exprs) as
      putStrLn $ intercalate " ; " $ map (T.unpack . prettyAST) res
      return as'

-- Same as run, but return without IO (not evaluating directly the inputs) 
runWithoutIO :: Aliases -> T.Text -> Aliases
runWithoutIO as source =
  case runParser program "repl" source of
    Left _ -> as
    Right exprs -> do
      let (_, as') = runState (sequence $ map evaluate exprs) as
      as'
      
repl :: Aliases -> IO ()
repl as = do
  putStr "> "
  hFlush stdout
  input <- getLine
  as' <- run as $ T.pack input
  repl as'

-- run the initialization first before user can use the interpreter
replBefore :: Aliases -> IO ()
replBefore start = do
  _ <- run start $ T.pack "You can start to use this interpreter"
  repl start

-- define aliased inputs
inputs :: [String]
inputs =
  [ "S := λw.λy.λx.y(w y x)"
  , "+ := S"
  , "* := λx.λy.λz.x(y z)"
  , "T := λx.λy.x"
  , "F := λx.λy.y"
  , "True  := T"
  , "False  := F"
  , "not  := λp.p F T"
  , "and  := λp.λq.p q F"
  , "or := λp.λq.p T q"
  , "Z  := λx.x F not F"
  , "∧  := and"
  , "∨  := or"
  , "∥  := or"
  , "&  := and"
  , "¬  := not"
  , "0  := λs.λz.z"
  , "1  := S 0"
  , "2  := S 1"
  , "3  := S 2"
  , "4  := S 3"
  , "5  := S 4"
  , "6  := S 5"
  , "7  := S 6"
  , "8  := S 7"
  , "9  := S 8"
  ]

-- | Evaluates a list of Text in the environment.
initialEnvironment :: Foldable t => t T.Text -> Aliases
initialEnvironment = foldl runWithoutIO M.empty

initialMessage :: String
initialMessage = "Welcome to Lambda Calculus Interpreter! Preparing..."

main :: IO ()
main = do
  putStrLn(initialMessage)
  replBefore $ initialEnvironment $ map T.pack inputs