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
intexpr = (chainl1 intterm addop)
 
intterm :: Parser (Exp Int)
intterm    = (chainl1 intfactor mulop)
 
intfactor :: Parser (Exp Int)
intfactor  = 
  try (parens lis intexpr)
  <|> 
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
 try 
   (do
      reservedOp lis "-"
      i <- intexpr
      return (UMinus i))

mulop   =   do{ reservedOp lis "*"; return (Times)   }
        <|> do{ reservedOp lis "/"; return (Div) }
addop   =   do{ reservedOp lis "+"; return (Plus) }
        <|> do{ reservedOp lis "-"; return (Minus) }

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexpr :: Parser (Exp Bool)
boolexpr = (chainl1 boolexpr2 booland)

boolexpr2 :: Parser (Exp Bool)
boolexpr2 = (chainl1 boolterm boolor)

boolterm:: Parser (Exp Bool)
boolterm =
  try (
    do 
      i<- intexpr
    
      op <- (try (do{ reservedOp lis "=="; return (Eq) })
            <|> 
            try (do{ reservedOp lis "!="; return (NEq) })
            <|>
            try (do{ reservedOp lis ">"; return (Gt) })
            <|>
            try (do{ reservedOp lis "<"; return (Lt) }))
      j<- intexpr
      return (op i j))
    <|>
    boolneg

boolneg :: Parser (Exp Bool)
boolneg = 
  try
  (do
    reservedOp lis "!"
    b<-boolneg
    return (Not b))
  <|>
  boolatom 

boolatom:: Parser (Exp Bool)
boolatom =
  try (parens lis boolexpr)
  <|>
  try
    (do
      t<- reserved lis "true"
      return (BTrue))
  <|>
  try
    (do
      t<- reserved lis "false"
      return (BFalse))
  <|>
  do
    reservedOp lis "!"
    b<-boolexpr
    return (Not b)

booland = do{ reservedOp lis "&&"; return (And) }
boolor  = do{ reservedOp lis "||"; return (Or) } 
-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = 
    do
      b <- boolexpr
      return (IfThenElse b Skip Skip)

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
