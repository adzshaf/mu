module Mu.Parser (Parser, program) where

import Data.Void
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

-- | An identifier for a bound or free variable.
type Identifier = T.Text

-- | Represents the possible constructs of the language.
data AST
  = Variable Identifier        -- ^ Reference to a variable
  | Abstraction Identifier AST -- ^ Function definition with a bound variable and body
  | Application AST AST        -- ^ Function application
  deriving (Show)

-- | Parser for skipping whitespace and line comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

-- | Parser for turning any parser into the same parser that also consumes any trailing
--   whitespace or comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parser for matching any text and consuming any trailing whitespace or comments.
symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

-- | Parse an identifier
identifier :: Parser Identifier
identifier = T.singleton <$> lexeme letterChar

-- | Parse a variable reference.
variable :: Parser AST
variable = Variable <$> identifier

-- | Parse an abstraction (function definition).
abstraction :: Parser AST
abstraction = do
  _ <- symbol "\\"
  ident <- identifier
  _ <- symbol "."
  body <- term
  return $ Abstraction ident body

-- | Parse a function application.
application :: Parser AST
application = foldl1 Application <$> some term

-- | Parse a variable or abstraction
term :: Parser AST
term = choice
  [ variable
  , parenthesized abstraction
  ]

-- | Turns any parser into the same parser that requires parentheses around its
--   input.
parenthesized :: Parser a -> Parser a
parenthesized p = do
  _ <- symbol "("
  v <- p
  _ <- symbol ")"
  return v

-- | Parse a whole lambda calculus program (expression followed by eof).
program :: Parser AST
program = do
  ast <- application
  _ <- eof
  return ast