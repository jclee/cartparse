import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.Functor.Identity
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)

main :: IO ()
main = do
    cartDir <- getEnv "CARTLIFE"
    files <- sort <$> getFiles cartDir
    mapM_ parseFile files
    --let file = cartDir </> "AskOnly.asc"
    --let file = cartDir </> "KeyboardMovement_102.asc"
    --let file = cartDir </> "Parallax_ASH.asc"
    --parseFile file

parseFile :: String -> IO ()
parseFile file = do
    print file
    content <- fileContent file
    let toks = cartScan content
    let ast = parse cartParse "(parser input)" =<< toks
    -- dumpToks $ take 200 <$> toks
    print ast

getFiles :: FilePath -> IO [FilePath]
getFiles p = filterM doesFileExist =<< getRelDirectoryContents p

getRelDirectoryContents :: FilePath -> IO [FilePath]
getRelDirectoryContents p = map (p </>) <$> getDirectoryContents p

fileContent :: FilePath -> IO String
fileContent p = do
    inFile <- openFile p ReadMode
    hGetContents inFile

data Decoration = DLineComment String
    | DDirective String
    | DBlankLine
    | DBlockComment String
    | DEndComment String
    | DInlineComment String
    deriving (Show)

type PreToken = (SourcePos, PreTok)
data PreTok = PTDecoration Decoration
    | PTContent String
    deriving (Show)

type DecoratedString = (SourcePos, [Decoration], [Decoration], String)

type Token = (SourcePos, [Decoration], Tok)
data Tok =
      TString String
    | TInteger Integer
    | TFloat Double
    | TTrue
    | TFalse
    | TMember
    | TSemicolon
    | TAssign
    | TComma
    | TDot
    | TEq
    | TNeq
    | TLt
    | TGt
    | TLtEq
    | TGtEq
    | TPlus
    | TMinus
    | TDiv
    | TMult
    | TNot
    | TBinAnd
    | TBinOr
    | TLogAnd
    | TLogOr
    | TPlusEq
    | TMinusEq
    | TMultEq
    | TDivEq
    | TLBrace
    | TRBrace
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TIdentifier String
    | TElse
    | TEnum
    | TExport
    | TFunction
    | TIf
    | TImport
    | TNew
    | TReturn
    | TStatic
    | TStruct
    | TThis
    | TWhile
    | TEof
    deriving (Eq)

instance Show Tok where
    show (TString s) = "string \"" ++ show s ++ "\""
    show (TInteger i) = "integer \"" ++ show i ++ "\""
    show (TFloat d) = "double \"" ++ show d ++ "\""
    show TTrue = "true"
    show TFalse = "false"
    show TMember = "::"
    show TSemicolon = ";"
    show TAssign = "="
    show TComma = ","
    show TDot = "."
    show TEq = "=="
    show TNeq = "!="
    show TLt = "<"
    show TGt = ">"
    show TLtEq = "<="
    show TGtEq = ">="
    show TPlus = "+"
    show TMinus = "-"
    show TDiv = "/"
    show TMult = "*"
    show TNot = "!"
    show TBinAnd = "&"
    show TBinOr = "|"
    show TLogAnd = "&&"
    show TLogOr = "||"
    show TPlusEq = "+="
    show TMinusEq = "-="
    show TMultEq = "*="
    show TDivEq = "/="
    show TLBrace = "{"
    show TRBrace = "}"
    show TLParen = "("
    show TRParen = ")"
    show TLBracket = "["
    show TRBracket = "]"
    show (TIdentifier s) = "identifier \"" ++ show s ++ "\""
    show TElse = "else"
    show TEnum = "enum"
    show TExport = "export"
    show TFunction = "function"
    show TIf = "if"
    show TImport = "import"
    show TNew = "new"
    show TReturn = "return"
    show TStatic = "static"
    show TStruct = "struct"
    show TThis = "this"
    show TWhile = "while"
    show TEof = "EOF"

type Ast = [ATopLevel]

-- TODO DO NOT COMMIT - need to preserve constituent token decorations...
data ATopLevel =
    AVarDec { avdTypename :: String
            , avdVars :: [AVarInit]
            }
    deriving (Show)

data AVarInit =
    AVarInit { aviId :: String
             , aviSubscripts :: [AVarSubscript]
             , aviInit :: Maybe AExpr
             }
    deriving (Show)

data AVarSubscript =
    AVSEmpty
    | AVSId String
    | AVSInt Integer
    deriving (Show)

data AExpr =
    AFalse
    | ATrue
    deriving (Show)

type TokenParser a = GenParser Token () a
type PreTokenParser a = GenParser PreToken () a

cartParse :: TokenParser Ast
cartParse = do
    topLevels <- many pTopLevel
    eof
    return topLevels

pTopLevel :: TokenParser ATopLevel
pTopLevel =
    pVarDec
    <?> "toplevel declaration"

pVarDec :: TokenParser ATopLevel
pVarDec = do
    typename <- pIdentifier
    vars <- sepBy1 pVarInit pComma
    pSemicolon
    return $ AVarDec { avdTypename=typename, avdVars=vars }
    <?> "variable declaration"

pVarInit :: TokenParser AVarInit
pVarInit = do
    id <- pIdentifier
    subscripts <- many (between pLBracket pRBracket pInitSubscript)
    init <- option Nothing (Just <$> (pAssign >> pExpr))
    return $ AVarInit { aviId=id, aviSubscripts=subscripts, aviInit=init }

pInitSubscript :: TokenParser AVarSubscript
pInitSubscript = do
    (AVSId <$> pIdentifier)
    <|> (AVSInt <$> pInteger)
    <|> return AVSEmpty

pExpr :: TokenParser AExpr
pExpr =
    (pTrue >> return ATrue)
    <|> (pFalse >> return AFalse)
    <?> "expression"

pInteger :: TokenParser Integer
pInteger =
    matchToken test
    where
      test (TInteger i) = Just i
      test _ = Nothing

pIdentifier :: TokenParser String
pIdentifier =
    matchToken test
    where
      test (TIdentifier s) = Just s
      test _ = Nothing

pAssign :: TokenParser ()
pAssign = matchToken' TAssign

pComma :: TokenParser ()
pComma = matchToken' TComma

pFalse :: TokenParser ()
pFalse = matchToken' TFalse

pLBracket :: TokenParser ()
pLBracket = matchToken' TLBracket

pRBracket :: TokenParser ()
pRBracket = matchToken' TRBracket

pSemicolon :: TokenParser ()
pSemicolon = matchToken' TSemicolon

pTrue :: TokenParser ()
pTrue = matchToken' TTrue

{-
TODO DO NOT COMMIT - convert to code

toplevel
  - function
  - vardec
  - enum
  - struct
  - export
  - import

export
  - "export" id ";"

import
  - "import"
    - functionsig ";"
    - vardec

function
  - functionsig block

functionsig
  - "static"? ("function" | id) id ("::" id) paramdec

paramdecs
  - "(" (paramdec ("," paramdec)*)? ")"

paramdec
  - "this" id "*"
  - typename varinit

block
  - "{" command* "}"

command
  - if
  - while
  - return
  - vardec
  - exprcommand ";"
  - block
  - expr ";" (might not be necessary?)

if
  - "if" "(" expr ")" command ("else" command)?

while
  - "while" "(" expr ")" command

return
  - "return" expr? ";"

vardec
  - typename varinit ("," varinit)* ";"

varinit
  - id ("[" (int | id)? "]")* ("=" expr)

enum
  - "enum" id "{" (id ("," id)*)? "}"

struct
  - "struct" id "{" (vardec | import)* "}" ";"

expr
  - assignexpr

assign_expr
  - logicalorexpr ("=" | "+=" | "-=" | "*=" | "/=" assign_expr)?

logicalorexpr
  - logicalandexpr ("||" logicalorexpr)?

logicalandexpr
  - binorexpr ("&&" logicalandexpr)?

binorexpr
  - binandexpr ("|" binorexpr)?

binandexpr
  - eqexpr ("&" binandexpr)?

eqexpr
  - relationalexpr (("==" | "!=") eqexpr)?

relationalexpr
  - addexpr (("<" | ">" | "<=" | ">=") relationalexpr)?

addexpr
  - multexpr (("+" | "-")? addexpr)?

multexpr
  - castexpr (("*" | "/")? multexpr)?

castexpr
  - unaryexpr
  - "(" unaryexpr ")"
  - "(" typename ")"

unaryexpr
  - ("++" | "--" | "+" | "-" | "&" | "*" | "!") unaryexpr
  - postfixexpr

postfixexpr
  - primaryexpr
    - "[" expr "]"
    - callparams
    - "." id
    - "++"
    - "--"

primaryexpr
  - "(" expr ")"
  - float
  - int
  - string
  - "this"
  - "new" typename "[" expr "]"

callparams
  - "(" (expr ("," expr)*)? ")"

typename
  - "*"? id

-}

matchToken' :: Tok -> TokenParser ()
matchToken' tok = do
    _ <- matchToken (\t -> if t == tok then Just t else Nothing)
    return ()

matchToken :: (Tok -> Maybe a) -> TokenParser a
matchToken test
    = token showToken posToken testToken
    where
      showToken (_, _, tok) = show tok
      posToken  (pos, _, _) = pos
      testToken (_, _, tok) = test tok

dumpToks :: Either ParseError [Token] -> IO ()
dumpToks (Left err) = putStrLn $ "Scanning error: " ++ show err
dumpToks (Right toks) = mapM_ dumpTok toks

dumpTok :: Token -> IO ()
dumpTok (pos, decs, tok) = putStrLn $ show (sourceLine pos) ++ " " ++ show tok

cartScan :: String -> Either ParseError [Token]
cartScan s = do
    preTokens <- parse parsePreTokens "(parser input)" s
    decoratedStrings <- parse associateDecorations "(parser input)" preTokens
    concat <$> sequence (scanDecoratedString <$> decoratedStrings)

scanDecoratedString :: DecoratedString -> Either ParseError [Token]
scanDecoratedString (pos, lDecs, rDecs, "") = Right [(pos, lDecs ++ rDecs, TEof)]
scanDecoratedString (pos, lDecs, rDecs, s) = do
    toks <- parse (scanString pos) "(parser input)" s
    case toks of
        [] -> fail (show pos ++ "empty tok list")
        _ -> return $ decorateFirst lDecs (decorateLast rDecs toks)
    where
        decorateFirst [] ts = ts
        decorateFirst decs ts = [addDecorations decs (head ts)] ++ tail ts
        decorateLast [] ts = ts
        decorateLast decs ts = init ts ++ [addDecorations decs (last ts)]
        addDecorations newDecs (pos', decs, c) = (pos', decs ++ newDecs, c)

scanString :: SourcePos -> Parser [Token]
scanString pos = do
    setPosition pos
    many scanToken

scanToken :: Parser Token
scanToken = do
    pos <- getPosition
    tok <- scanTok
    return (pos, [], tok)

scanTok :: Parser Tok
scanTok =
    (reserved "else" >> return TElse)
    <|> (reserved "enum" >> return TEnum)
    <|> (reserved "export" >> return TExport)
    <|> (reserved "false" >> return TFalse)
    <|> (reserved "function" >> return TFunction)
    <|> (reserved "if" >> return TIf)
    <|> (reserved "import" >> return TImport)
    <|> (reserved "new" >> return TNew)
    <|> (reserved "return" >> return TReturn)
    <|> (reserved "static" >> return TStatic)
    <|> (reserved "struct" >> return TStruct)
    <|> (reserved "true" >> return TTrue)
    <|> (reserved "this" >> return TThis)
    <|> (reserved "while" >> return TWhile)
    <|> try (TInteger <$> integer)
    <|> try (TFloat <$> float)
    <|> try (TString <$> stringLiteral)
    <|> scanTokFromString "&&" TLogAnd
    <|> scanTokFromString "||" TLogOr
    <|> scanTokFromString "==" TEq
    <|> scanTokFromString "!=" TNeq
    <|> scanTokFromString "<=" TLtEq
    <|> scanTokFromString ">=" TGtEq
    <|> scanTokFromString "+=" TPlusEq
    <|> scanTokFromString "-=" TMinusEq
    <|> scanTokFromString "*=" TMultEq
    <|> scanTokFromString "/=" TDivEq
    <|> scanTokFromString "::" TMember
    <|> scanTokFromString "<" TLt
    <|> scanTokFromString ">" TGt
    <|> scanTokFromString "+" TPlus
    <|> scanTokFromString "-" TMinus
    <|> scanTokFromString "/" TDiv
    <|> scanTokFromString "*" TMult
    <|> scanTokFromString "!" TNot
    <|> scanTokFromString "&" TBinAnd
    <|> scanTokFromString "|" TBinOr
    <|> scanTokFromString "=" TAssign
    <|> scanTokFromString "," TComma
    <|> scanTokFromString "." TDot
    <|> scanTokFromString ";" TSemicolon
    <|> scanTokFromString "{" TLBrace
    <|> scanTokFromString "}" TRBrace
    <|> scanTokFromString "[" TLBracket
    <|> scanTokFromString "]" TRBracket
    <|> scanTokFromString "(" TLParen
    <|> scanTokFromString ")" TRParen
    <|> TIdentifier <$> identifier

scanTokFromString :: String -> Tok -> Parser Tok
scanTokFromString s t = try (lexeme $ string s >> return t)

-- Better signature?
agsStyle :: P.GenLanguageDef String u Identity
agsStyle
    = javaStyle
      { P.nestedComments = False
      , P.reservedNames = [
          "else",
          "enum",
          "export",
          "false",
          "function",
          "if",
          "import",
          "new",
          "return",
          "static",
          "struct",
          "this",
          "true",
          "while"
        ]
      }

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser agsStyle

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

identifier :: Parser String
identifier = P.identifier lexer

integer :: Parser Integer
integer = P.integer lexer

float :: Parser Double
float = P.float lexer

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

associateDecorations :: PreTokenParser [DecoratedString]
associateDecorations = do
    ts <- many (try decoratedContent)
    eofDecs <- many leftDecoration
    eofPos <- getPosition
    _ <- eof
    return $ ts ++ [(eofPos, eofDecs, [], "")]

decoratedContent :: PreTokenParser DecoratedString
decoratedContent = do
    lDecs <- many leftDecoration
    pos <- getPosition
    content <- matchPTContent
    rDecs <- many rightDecoration
    return (pos, lDecs, rDecs, content)

leftDecoration :: PreTokenParser Decoration
leftDecoration
    = matchPreToken lDecTest
    where
      lDecTest (PTDecoration (DEndComment _)) = Nothing
      lDecTest (PTDecoration d) = Just d
      lDecTest _ = Nothing

rightDecoration :: PreTokenParser Decoration
rightDecoration
    = matchPreToken rDecTest
    where
      rDecTest (PTDecoration d@(DEndComment _)) = Just d
      rDecTest _ = Nothing

matchPTContent :: PreTokenParser String
matchPTContent
    = matchPreToken contentTest
    where
      contentTest (PTContent s) = Just s
      contentTest _ = Nothing

matchPreToken :: (PreTok -> Maybe a) -> PreTokenParser a
matchPreToken test
    = token showToken posToken testToken
    where
      showToken (_, tok) = show tok
      posToken  (pos, _) = pos
      testToken (_, tok) = test tok

parsePreTokens :: Parser [PreToken]
parsePreTokens = ptFile

ptFile :: Parser [PreToken]
ptFile = do
    toks <- sepEndBy ptLine eol
    _ <- eof
    return $ concat toks

ptLine :: Parser [PreToken]
ptLine = do
    pos <- getPosition
    _ <- ptSpacePrefix
    (try ((:[]) <$> ptDirective)
       <|> try ((:[]) <$> (ptLineComment DLineComment))
       <|> try ptLineContent
       <|> return [(pos, PTDecoration DBlankLine)])

ptSpacePrefix :: Parser String
ptSpacePrefix = many (oneOf " \t\v")

ptDirective :: Parser PreToken
ptDirective = do
    pos <- getPosition
    _ <- lookAhead (char '#')
    s <- many (noneOf "\n\r")
    return (pos, PTDecoration (DDirective s))

ptLineContent :: Parser [PreToken]
ptLineContent = many (try (ptLineComment DEndComment)
                        <|> try ptBlockComment
                        <|> ptNonComment)

ptLineComment :: (String -> Decoration) -> Parser PreToken
ptLineComment decorator = do
    pos <- getPosition
    _ <- lookAhead ptLineCommentStart
    s <- many (noneOf "\n\r")
    return $ (pos, PTDecoration (decorator s))

ptBlockComment :: Parser PreToken
ptBlockComment = do
    pos <- getPosition
    _ <- ptBlockCommentStart
    s <- manyTill anyChar (try ptBlockCommentEnd)
    return (pos, PTDecoration $ ctor s)
        where ctor s = if elem '\n' s || elem '\r' s
                       then DBlockComment s
                       else DInlineComment s

ptNonComment :: Parser PreToken
ptNonComment = do
    pos <- getPosition
    s <- many1Till anyChar (try (lookAhead ptLineCommentStart)
                              <|> try (lookAhead ptBlockCommentStart)
                              <|> toLineEnding)
    return $ (pos, PTContent s)

many1Till :: (Show end) => Parser a -> Parser end -> Parser [a]
many1Till p end = do
    s <- manyTill p end
    guard (not $ null s)
    return s

ptLineCommentStart :: Parser String
ptLineCommentStart = string "//"

ptBlockCommentStart :: Parser String
ptBlockCommentStart = string "/*"

ptBlockCommentEnd :: Parser String
ptBlockCommentEnd = string "*/"

toLineEnding :: Parser String
toLineEnding = try (lookAhead ((eof >> return "") <|> eol))

eol :: Parser String
eol = try (string "\r\n") <|> string "\r" <|> string "\n" <?> "end of line"
