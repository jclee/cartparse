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

type DecoratedString = (SourcePos, [Decoration], [Decoration], String)

type Token = (SourcePos, [Decoration], Tok)
data Tok = TString String
    | TEof
    deriving (Show)

cartParse :: String -> Either ParseError [Token]
cartParse s = do
    preTokens <- parse parsePreTokens "(parser input)" s
    decoratedStrings <- parse associateDecorations "(parser input)" preTokens
    concat <$> sequence (scanDecoratedString <$> decoratedStrings)
    -- TODO: Implement parsing

type PreTokenParser a = GenParser PreToken () a

scanDecoratedString :: DecoratedString -> Either ParseError [Token]
scanDecoratedString (pos, lDecs, rDecs, s) = do
    toks <- parse (scanString pos) "(parser input)" s
    return $ decorateFirst lDecs (decorateLast rDecs toks)
    where
        decorateFirst decs ts = [addDecorations decs (head ts)] ++ tail ts
        decorateLast decs ts = init ts ++ [addDecorations decs (last ts)]
        addDecorations newDecs (pos, decs, c) = (pos, decs ++ newDecs, c)

scanString :: SourcePos -> Parser [Token]
scanString pos = do
    -- TODO: Implement scanning
    return [(pos, [], TString "x")]

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
