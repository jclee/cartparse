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
    let ast = parse cartParse "(parser input)" <$> toks
    --dumpToks $ take 20 <$> toks
    print ast

getFiles :: FilePath -> IO [FilePath]
getFiles p = filterM doesFileExist =<< getRelDirectoryContents p

getRelDirectoryContents :: FilePath -> IO [FilePath]
getRelDirectoryContents p = map (p </>) `liftM` getDirectoryContents p

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
    deriving (Show)

data Ast = FunDec Token
    deriving (Show)

type TokenParser a = GenParser Token () a
type PreTokenParser a = GenParser PreToken () a

cartParse :: TokenParser Ast
cartParse = do
    -- TODO: Do something better
    pos <- getPosition
    return $ FunDec (pos,[],TFunction)

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
    <|> (reserved "function" >> return TFunction)
    <|> (reserved "if" >> return TIf)
    <|> (reserved "import" >> return TImport)
    <|> (reserved "new" >> return TNew)
    <|> (reserved "return" >> return TReturn)
    <|> (reserved "static" >> return TStatic)
    <|> (reserved "struct" >> return TStruct)
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
          "function",
          "if",
          "import",
          "new",
          "return",
          "static",
          "struct",
          "this",
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
    (pos, content) <- matchPTContent
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

matchPTContent :: PreTokenParser (SourcePos, String)
matchPTContent
    = token showToken posToken testToken
    where
      showToken (_, tok) = show tok
      posToken  (pos, _) = pos
      testToken (pos, tok)
        = case tok of
            PTContent s -> Just (pos, s)
            _ -> Nothing

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
