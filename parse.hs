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
    let file = (cartDir </> "AskOnly.asc")
    print file
    content <- fileContent file
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
ptFile = concat <$> sepEndBy ptLine eol

ptLine :: Parser [PreToken]
ptLine = ptSpacePrefix >>
           (try ptDirective
              <|> try ptLineContent
              <|> return [PTDecoration DBlankLine])

ptSpacePrefix :: Parser String
ptSpacePrefix = many (oneOf " \t")

-- Technically, could have comments at end of directive...
ptDirective :: Parser [PreToken]
ptDirective = do
    _ <- lookAhead (char '#')
    s <- many (noneOf "\n\r")
    return [PTDecoration . DDirective $ s]

ptLineContent :: Parser [PreToken]
ptLineContent = listPT . PTContent <$> many1 (noneOf "\n\r")

listPT :: PreToken -> [PreToken]
listPT = (:[])

eol :: Parser String
eol = try (string "\r\n") <|> string "\r" <|> string "\n"
