module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           Types

tokenize :: TokenParser u
tokenize = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["T", "B"] -- T := True | B := Bottom
    , reservedOpNames = [ "->", "&&", "||", "~", "<>", "[]"]
    }
  )

-- Este parser utiliza el parser que recibe de argumento ignorando espacios en blanco
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace tokenize
  t <- p
  eof
  return t

-- Precedencia de operadordes y asociacion:
-- (->) (Derecha)
-- (||) (Izquierda)
-- (&&) (Izquierda)
-- (~) | ([]) | (<>) (Unarios)

pForm :: Parser Form
pForm = pForm1

pForm1 :: Parser Form
pForm1 = chainr1 pForm2 pImplies

pForm2 :: Parser Form
pForm2 = chainl1 pForm3 pOr

pForm3 :: Parser Form
pForm3 = chainl1 pFactor pAnd

pFactor :: Parser Form
pFactor = try (pUnary) <|>
          try (pVar) <|>
          try (parens tokenize pForm) <|>
          try (pConst)

pConst :: Parser Form
pConst = do try (reserved tokenize "T")
            return FTrue
        <|>
         do try (reserved tokenize "B")
            return FBottom

pVar :: Parser Form
pVar = do s <- identifier tokenize
          return (Var s)

pImplies :: Parser (Form -> Form -> Form)
pImplies = do try (reservedOp tokenize "->")
              return Implies

pAnd :: Parser (Form -> Form -> Form)
pAnd = do try (reservedOp tokenize "&&")
          return And

pOr :: Parser (Form -> Form -> Form)
pOr = do try (reservedOp tokenize "||")
         return Or

pUnary :: Parser Form
pUnary = do try (reservedOp tokenize "~")
            f <- pFactor
            return (Not f)
        <|>
         do try (reservedOp tokenize "[]")
            f <- pFactor
            return (Box f)
        <|>
         do try (reservedOp tokenize "<>")
            f <- pFactor
            return (Dia f)

parseForm :: SourceName -> String -> Either ParseError Form
parseForm = parse (totParser pForm)