module Parser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language ( emptyDef )
import AST


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

--  braces = braces emptyDef

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
      reservedOp lis "++" 
      return (VarInc v))
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

booland = do{ reservedOp lis "&&"; return (And) }
boolor  = do{ reservedOp lis "||"; return (Or) } 

boolterm :: Parser (Exp Bool)
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

boolatom :: Parser (Exp Bool)
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

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = (chainl1 commterm sequential)

sequential = do{ reservedOp lis ";"; return (Seq) }
  
commterm :: Parser Comm
commterm = 
  try
    (do
      reservedOp lis "repeat" 
      c<-braces lis comm
      reservedOp lis "until"
      b <- boolexpr
      return (RepeatUntil c b))
  <|>
  try
    (do
      reservedOp lis "if"
      b <- boolexpr
      c <- braces lis comm
      (try
        (do
          reservedOp lis "else"
          c2<- braces lis comm
          return (IfThenElse b c c2)
        )<|> return (IfThen b c))
      )
    <|>
    try
      (do
        reservedOp lis "case"
        c <- braces lis clausula
        return (Case c)
      )
  <|>
  commatom

commatom :: Parser Comm
commatom =
  try
    (do
      v<-identifier lis
      reservedOp lis "="
      i <- intexpr
      return (Let v i))
  <|>
  do
    reservedOp lis "skip"
    return Skip 

clausula :: Parser [(Exp Bool, Comm)]
clausula =
  many1 (do
    b <- boolexpr
    reservedOp lis ":"
    c <- braces lis comm
    return (b,c))

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
