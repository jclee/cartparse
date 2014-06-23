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

data PreToken = PTDecoration Decoration
    | PTContent String
    deriving (Show)

-- TODO: Implement me!
cartParse :: String -> Either ParseError [PreToken]
cartParse s = parse parsePreTokens "(parser input)" s

parsePreTokens :: Parser [PreToken]
parsePreTokens = ptFile

ptFile :: Parser [PreToken]
ptFile = do
    toks <- sepEndBy ptLine eol
    _ <- eof
    return $ concat toks

ptLine :: Parser [PreToken]
ptLine = ptSpacePrefix >>
           (try ptDirective
              <|> try ((:[]) <$> (ptLineComment DLineComment))
              <|> try ptLineContent
              <|> return [PTDecoration DBlankLine])

ptSpacePrefix :: Parser String
ptSpacePrefix = many (oneOf " \t\v")

-- Technically, we could have comments at end of directive, but that doesn't
-- happen in the files of interest.
ptDirective :: Parser [PreToken]
ptDirective = do
    _ <- lookAhead (char '#')
    s <- many (noneOf "\n\r")
    return [PTDecoration (DDirective s)]

ptLineContent :: Parser [PreToken]
ptLineContent = many (try (ptLineComment DEndComment)
                        <|> try ptBlockComment
                        <|> ptNonComment)

ptLineComment :: (String -> Decoration) -> Parser PreToken
ptLineComment decorator = do
    _ <- lookAhead ptLineCommentStart
    s <- many (noneOf "\n\r")
    return $ PTDecoration (decorator s)

ptBlockComment :: Parser PreToken
ptBlockComment = do
    _ <- ptBlockCommentStart
    s <- manyTill anyChar (try ptBlockCommentEnd)
    return $ PTDecoration $ ctor s
        where ctor s = if elem '\n' s || elem '\r' s
                        then DBlockComment s
                        else DInlineComment s

ptNonComment :: Parser PreToken
ptNonComment = do
    s <- many1Till anyChar (try (lookAhead ptLineCommentStart)
                              <|> try (lookAhead ptBlockCommentStart)
                              <|> toLineEnding)
    return $ PTContent s

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
