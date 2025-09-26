module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until","case"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        ,":"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

intexpr :: Parser (Exp Int)
intexpr = 
    do 
        t <- intterm
        ((try
          (do 
            reservedOp lis "+"
            e <- intexpr
            return (Plus t e ))) 
        <|>
        (try
          do 
            reservedOp lis "-"
            e <- intexpr
            return (Minus t e))
        <|> 
        (return t))

intterm :: Parser (Exp Int)
intterm = 
  try
    (do 
      f <- intfactor
      try
        (do 
          reservedOp lis "*"
          t <- intterm
          return (Times f  t ))
      <|>
      try
        (do 
          reservedOp lis "/"
          t <- term
          return (Div f  t))
      <|>
        return f)


intfactor :: Parser (Exp Int)
intfactor = 
  try
  (do 
    n<-natural lis
    return (Const (fromInteger n)))
  <|>
  try
  (do
    v<- identifier lis 
    return (Var v))
  <|>
  try (do
    reservedOp lis "-"
    i <- intexp
    return (UMinus i))
  <|>
  try 
  (do
    i <- parens lis intexp
    return i)
------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = 
    do
      i<-intexp
      return (Let "salida" i) 

  {-try 
    (do 
      reserved lis "skip"
      return Skip)
  <|>
  do 
    c1<-comm
    reservedOp lis ";"
    c2<-comm
    return (Seq c1 c2)
-}


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
