import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    cartDir <- getEnv "CARTLIFE"
    files <- sort <$> getFiles cartDir
    --let file = head files
    --let file = ("parse.hs")
    --let file = cartDir </> "AskOnly.asc"
    --let file = cartDir </> "KeyboardMovement_102.asc"
    let file = cartDir </> "Parallax_ASH.asc"
    print file
    content <- fileContent file
    --print $ take 20 <$> cartParse content
    print $ cartParse content
    putStrLn "Hello, World!"

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

type Token = (SourcePos, Tok)
data Tok = TString String
    deriving (Show)

-- TODO: Implement me!
cartParse :: String -> Either ParseError [Token]
cartParse s = do
    preTokens <- parse parsePreTokens "(parser input)" s
    parse scanTokens "(parser input)" preTokens

type PreTokenParser a = GenParser PreToken () a

scanTokens :: PreTokenParser [Token]
scanTokens = do
    -- TODO: Implement me!
    pos <- getPosition
    return [(pos, TString "x")]

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
    return (pos, PTDecoration $ ctor s pos)
        where ctor s pos = if elem '\n' s || elem '\r' s
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
