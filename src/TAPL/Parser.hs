module TAPL.Parser where

import TAPL.Support
import TAPL.Core
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Lexer
languageDef = 
  emptyDef { Token.reservedNames = [ "if"
                                    , "then"
                                    , "else"
                                    , "succ"
                                    , "pred"
                                    , "isZero"
                                    ]
           }

lexer = Token.makeTokenParser languageDef
reserved = Token.reserved lexer
parens = Token.parens lexer
integer = Token.integer lexer

-- Parser
eol = oneOf "\n\r"

stm :: GenParser Char st Term
stm = do
  result <- command
  char ';'
  spaces >> optional eol
  return result
  
genAST :: GenParser Char st [Term]
genAST = do 
  result <- many stm
  eof
  return result
  
command :: GenParser Char st Term
command = appTerm <|> ifTerm
    
ifTerm = do
  reserved "if"
  t1 <- command
  reserved "then"
  t2 <- command
  reserved "else"
  t3 <- command
  return $ TmIf dummyInfo t1 t2 t3
  
appTerm = do t <- aTerm 
             return t
          <|> do reserved "succ"
                 t <- aTerm
                 return $ TmSucc dummyInfo t
          <|> do reserved "pred"
                 t <- aTerm
                 return $ TmPred dummyInfo t
          <|> do reserved "iszero"
                 t <- aTerm
                 return $ TmIsZero dummyInfo t
          
aTerm =  do t <- parens command
            return t
         <|> do reserved "true"
                return $ TmTrue dummyInfo
         <|> do reserved "false"
                return $ TmFalse dummyInfo
         <|> do num <- integer
                return $ let f 0 = TmZero dummyInfo
                             f n = TmSucc dummyInfo $ f (n-1)
                         in f num

parseLang :: String -> Either ParseError [Term]
parseLang content = parse genAST "(unknown)" content