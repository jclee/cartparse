import Control.Applicative hiding (many, (<|>))
import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.ParserCombinators.Parsec hiding (State)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)

main :: IO ()
main = do
    cartDir <- getEnv "CARTLIFE"
    files <- sort <$> filter (isSuffixOf ".asc") <$> (getFiles cartDir)
    --files <- sort <$> filter (isSuffixOf ".ash") <$> (getFiles cartDir)
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
    --dumpToks $ take 200 <$> toks
    putStrLn $ either show (renderToString . render) ast

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
    | TPlusAssign
    | TMinusAssign
    | TMultAssign
    | TDivAssign
    | TInc
    | TDec
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
    show TPlusAssign = "+="
    show TMinusAssign = "-="
    show TMultAssign = "*="
    show TDivAssign = "/="
    show TInc = "++"
    show TDec = "--"
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

data Ast = Ast [ATopLevel]
    deriving (Show)

-- TODO - need to preserve constituent token decorations...
data ATopLevel =
    AEof
    | AFunctionDec {
        afdSignature :: AFunctionSignature
      , afdBlock :: ABlock
    }
    | ATopLevelVarDec AVarDec
    | ATopLevelImportDec AImportDec
    | AEnumDec {
        aedName :: String
      , aedValues :: [String]
    }
    | AStructDec {
        asdName :: String
      , asdMembers :: [AStructMember]
    }
    | AExportDec {
        axdName :: String
    }
    deriving (Show)

data AStructMember =
    AStructMemberImport AImportDec
    | AStructMemberVar AVarDec
    deriving (Show)

data AImportDec =
    AImportFunctionSig AFunctionSignature
    | AImportVarDec AVarDec
    deriving (Show)

data AFunctionSignature =
    AFunctionSignature {
        afsIsStatic :: Bool
      , afsReturnType :: String
      , afsName :: String
      , afsParams :: [AFunctionDecParam]
    }
    deriving (Show)

data AFunctionDecParam =
    AFunctionDecExtenderParam {
        afdepName :: String
    }
    | AFunctionDecRegularParam {
        afdrpTypeName :: ATypeName
      , afdrpVarInit :: AVarInit
    }
    deriving (Show)

data ABlock = ABlock [ACommand]
    deriving (Show)

data ACommand =
    AIfCommand {
        aicTestExpr :: AExpr
      , aicTrueCommand :: ACommand
      , aicFalseCommand :: Maybe ACommand
    }
    | AWhileCommand {
          awcTestExpr :: AExpr
        , awcCommand :: ACommand
      }
    | AReturnCommand (Maybe AExpr)
    | AVarDecCommand AVarDec
    | AExprCommand AExpr
    | ABlockCommand ABlock
    deriving (Show)

data AVarDec =
    AVarDec {
        avdTypeName :: ATypeName
      , avdVars :: [AVarInit]
    }
    deriving (Show)

data AVarInit =
    AVarInit {
        aviId :: String
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
    | AThisExpr
    | ANewExpr {
          aneTypeName :: ATypeName
        , aneSizeExpr :: AExpr
      }
    | AFloatExpr Double
    | AIntExpr Integer
    | AStringExpr String
    | AIdentifierExpr String
    | AIndexExpr {
          aieExpr :: AExpr
        , aieIndexExpr :: AExpr
      }
    | ACallExpr {
          aceFunctionExpr :: AExpr
        , aceParams :: [AExpr]
      }
    | AMemberExpr {
          ameExpr :: AExpr
        , ameMember :: String
      }
    | AUnaryOpExpr {
          auoeExpr :: AExpr
        , auoeOp :: String
      }
    | APostOpExpr {
          apoeExpr :: AExpr
        , apoeOp :: String
      }
    | ABinOpExpr {
          aboeExpr1 :: AExpr
        , aboeExpr2 :: AExpr
        , aboeOp :: String
      }
    | AParenExpr AExpr
    | ACastExpr {
          aceTypeName :: ATypeName
        , aceCastExpr :: AExpr
      }
    deriving (Show)

data ATypeName =
    ATypeName {
        atnName :: String
      , atnIsPointer :: Bool
    }
    deriving (Show)

type TokenParser a = GenParser Token () a
type PreTokenParser a = GenParser PreToken () a

-- Data.ByteString might be more performant?
data Doc =
    Text String
    | Line
    | Space
    deriving (Show)

class Renderable a where
    render :: a -> [Doc]

instance Renderable Ast where
    render (Ast topLevels) = concat (render <$> topLevels)

instance Renderable ATopLevel where
    render AEof = [Text ""]
    render AFunctionDec {
        afdSignature=signature
      , afdBlock=block
    } = render signature ++ [Space] ++ render block ++ [Line]
    render (ATopLevelVarDec varDec) = render varDec ++ [Line]
    render (ATopLevelImportDec importDec) = render importDec ++ [Line]
    render AEnumDec {
        aedName=name
      , aedValues=values
    } = [Text "enum", Space, Text name, Space, Text "{"] ++ renderValues values ++ [Text "}", Text ";", Line]
        where
          renderValues [] = []
          renderValues items = [Line] ++ intercalate [Text ",", Line] ((:[]) . Text <$> items) ++ [Line]
    render AStructDec {
        asdName=name
      , asdMembers=members
    } = [Text "struct", Space, Text name, Space] ++ renderBetween "{" "}" members ++ [Text ";", Line]
    render AExportDec {
        axdName=name
    } = [Text "export", Space, Text name, Text ";", Line]

instance Renderable ABlock where
    render (ABlock []) = [Text "{", Text "}"]
    render (ABlock commands) =
        [Text "{", Line]
        ++ intercalate [Line] (render <$> commands)
        ++ [Line, Text "}"]

instance Renderable ACommand where
    render AIfCommand {
        aicTestExpr=testExpr
      , aicTrueCommand=trueCommand
      , aicFalseCommand=falseCommand
    } = [Text "if", Space, Text "("]
        ++ render testExpr
        ++ [Text ")", Space]
        ++ render trueCommand
        ++ maybe [] (\x -> [Space, Text "else", Space] ++ render x) falseCommand
    render AWhileCommand {
        awcTestExpr=testExpr
      , awcCommand=command
    } = [Text "while", Space, Text "("]
        ++ render testExpr
        ++ [Text ")", Space]
        ++ render command
    render (AReturnCommand Nothing) = [Text "return", Text ";"]
    render (AReturnCommand (Just expr)) =
      [Text "return", Space] ++ render expr ++ [Text ";"]
    render (AVarDecCommand varDec) = render varDec
    render (AExprCommand expr) = render expr ++ [Text ";"]
    render (ABlockCommand block) = render block

instance Renderable AStructMember where
    render (AStructMemberImport imp) = render imp
    render (AStructMemberVar var) = render var

instance Renderable AImportDec where
    render (AImportFunctionSig sig) = [Text "import", Space] ++ render sig
    render (AImportVarDec varDec) = [Text "import", Space] ++ render varDec

instance Renderable AFunctionSignature where
    render AFunctionSignature {
        afsIsStatic=isStatic
      , afsReturnType=returnType
      , afsName=name
      , afsParams=params
    } = (if isStatic then [Text "static", Space] else [])
        ++ [Text (if returnType == "void" then "function" else "returnType"), Space]
        ++ [Text name, Text "("]
        ++ intercalate [Text ",", Space] (render <$> params)
        ++ [Text ")"]

instance Renderable AFunctionDecParam where
    render AFunctionDecExtenderParam {
        afdepName=name
    } = [Text "this", Space, Text name, Text "*"]
    render AFunctionDecRegularParam {
        afdrpTypeName=typeName
      , afdrpVarInit=varInit
    } = render typeName ++ [Space] ++ render varInit

instance Renderable AVarDec where
    render AVarDec {
        avdTypeName=name
      , avdVars=vars
    } = render name ++ [Space] ++ intercalate [Text ",", Space] (fmap render vars) ++ [Text ";"]

instance Renderable ATypeName where
    render ATypeName {
        atnName=name
      , atnIsPointer=isPointer
    } = [Text name] ++ (if isPointer then [Space, Text "*"] else [])

instance Renderable AVarInit where
    render AVarInit {
        aviId=varId
      , aviSubscripts=subscripts
      , aviInit=initializer
    } =
        [Text varId]
        ++ (concat $ render <$> subscripts)
        ++ (maybe [] (\e -> [Space, Text "=", Space] ++ render e) initializer)

instance Renderable AVarSubscript where
    render AVSEmpty = [Text "[", Text "]"]
    render (AVSId s) = [Text "[", Text s, Text "]"]
    render (AVSInt i) = [Text "[", Text $ show i, Text "]"]

instance Renderable AExpr where
    render AFalse = [Text "false"]
    render ATrue = [Text "true"]
    render AThisExpr = [Text "this"]
    render ANewExpr {
        aneTypeName=name
      , aneSizeExpr=expr
    } = [Text "new", Space] ++ render name ++ [Text "["] ++ render expr ++ [Text "]"]
    render (AFloatExpr d) = [Text $ show d]
    render (AIntExpr i) = [Text $ show i]
    render (AStringExpr s) = [Text $ show s] -- TODO: escapes OK?
    render (AIdentifierExpr s) = [Text $ s]
    render AIndexExpr {
        aieExpr=expr
      , aieIndexExpr=indexExpr
    } = render expr ++ [Text "["] ++ render indexExpr ++ [Text "]"]
    render ACallExpr {
        aceFunctionExpr=expr
      , aceParams=params
    } = render expr ++ [Text "("] ++ intercalate [Text ",", Space] (render <$> params) ++ [Text ")"]
    render AMemberExpr {
        ameExpr=expr
      , ameMember=member
    } = render expr ++ [Text ".", Text member]
    render AUnaryOpExpr {
        auoeExpr=expr
      , auoeOp=op
    } = [Text op] ++ render expr
    render APostOpExpr {
        apoeExpr=expr
      , apoeOp=op
    } = render expr ++ [Text op]
    render ABinOpExpr {
        aboeExpr1=expr1
      , aboeExpr2=expr2
      , aboeOp=op
    } = render expr1 ++ [Space, Text op, Space] ++ render expr2
    render (AParenExpr expr) = [Text "("] ++ render expr ++ [Text ")"]
    render ACastExpr {
        aceTypeName=typeName
      , aceCastExpr=castExpr
    } = [Text "("] ++ render typeName ++ [Text ")", Space] ++ render castExpr

renderBetween :: (Renderable a) => String -> String -> [a] -> [Doc]
renderBetween start end [] = [Text start, Text end]
renderBetween start end items = [Text start, Line] ++ intercalate [Line] (fmap render items) ++ [Line, Text end]

renderToString :: [Doc] -> String
renderToString docs =
    concat $ fst $ runState ((mapM renderToString') docs) (0, True)

renderToString' :: Doc -> State (Int, Bool) String
renderToString' (Text s) = do
    (indent, shouldIndent) <- get
    let indent' =
            -- NOTE: Not strictly correct, if
            -- comments are also Text.
            if s `elem` ["(", "[", "{"] then
                indent + 1
            else if s `elem` [")", "]", "}"] then
                indent - 1
            else indent
    put (indent', shouldIndent)
    doIndent s
renderToString' Line = do
    (indent, _shouldIndent) <- get
    put (indent, True)
    return "\n"
renderToString' Space = return " "

doIndent :: String -> State (Int, Bool) String
doIndent s = do
    (indent, shouldIndent) <- get
    put (indent, False)
    return $ if shouldIndent then
                 replicate (indent * 4) ' ' ++ s
             else s

cartParse :: TokenParser Ast
cartParse = do
    topLevels <- many pTopLevel
    eof
    return $ Ast topLevels

pTopLevel :: TokenParser ATopLevel
pTopLevel =
    pEnumDec
    <|> pStructDec
    <|> pExportDec
    <|> (ATopLevelImportDec <$> pImportDec)
    <|> (pEof >> return AEof)
    <|> (try pTopLevelVarDec)
    <|> pFunctionDec
    <?> "toplevel declaration"

pStructDec :: TokenParser ATopLevel
pStructDec = do
    _ <- pStruct
    name <- pIdentifier
    members <- between pLBrace pRBrace (many pStructDecMember)
    _ <- pSemicolon
    return AStructDec {
        asdName=name
      , asdMembers=members
    }

pStructDecMember :: TokenParser AStructMember
pStructDecMember =
    (AStructMemberImport <$> pImportDec)
    <|> (AStructMemberVar <$> pVarDec)
    <?> "structure member"

pExportDec :: TokenParser ATopLevel
pExportDec = do
    _ <- pExport
    name <- pIdentifier
    _ <- pSemicolon
    return AExportDec { axdName=name }

pImportDec :: TokenParser AImportDec
pImportDec = do
    _ <- pImport
    (try pImportFunctionDec) <|> pImportVarDec

pImportFunctionDec :: TokenParser AImportDec
pImportFunctionDec = do
    functionSig <- pFunctionSignature
    _ <- pSemicolon
    return $ AImportFunctionSig functionSig

pImportVarDec :: TokenParser AImportDec
pImportVarDec = AImportVarDec <$> pVarDec

pFunctionDec :: TokenParser ATopLevel
pFunctionDec = do
    signature <- pFunctionSignature
    block <- pBlock
    return AFunctionDec {
        afdSignature=signature
      , afdBlock=block
    }
    <?> "function declaration"

pFunctionSignature :: TokenParser AFunctionSignature
pFunctionSignature = do
    isStatic <- option Nothing (Just <$> (pStatic))
    returnType <- ((pFunction >> return "void") <|> pIdentifier)
    name <- pIdentifier
    params <- between pLParen pRParen (sepBy pFunctionDecParam pComma)
    return AFunctionSignature {
        afsIsStatic=(isJust isStatic)
      , afsReturnType=returnType
      , afsName=name
      , afsParams=params
    }
    <?> "function signature"

pFunctionDecParam :: TokenParser AFunctionDecParam
pFunctionDecParam =
    pFunctionDecExtenderParam
    <|> pFunctionDecRegularParam
    <?> "function parameter"

pFunctionDecExtenderParam :: TokenParser AFunctionDecParam
pFunctionDecExtenderParam = do
    _ <- pThis
    name <- pIdentifier
    _ <- pMult
    return AFunctionDecExtenderParam {
        afdepName=name
    }

pFunctionDecRegularParam :: TokenParser AFunctionDecParam
pFunctionDecRegularParam = do
    typeName <- pTypeName
    varInit <- pVarInit
    return AFunctionDecRegularParam {
        afdrpTypeName=typeName
      , afdrpVarInit=varInit
    }

pEnumDec :: TokenParser ATopLevel
pEnumDec = do
    _ <- pEnum
    name <- pIdentifier
    values <- between pLBrace pRBrace (sepBy1 pIdentifier pComma)
    _ <- pSemicolon
    return AEnumDec {
        aedName=name
      , aedValues=values
    }

pTopLevelVarDec :: TokenParser ATopLevel
pTopLevelVarDec = ATopLevelVarDec <$> pVarDec

pVarDec :: TokenParser AVarDec
pVarDec = do
    typeName <- pTypeName
    vars <- sepBy1 pVarInit pComma
    _ <- pSemicolon
    return $ AVarDec { avdTypeName=typeName, avdVars=vars }
    <?> "variable declaration"

pVarInit :: TokenParser AVarInit
pVarInit = do
    ident <- pIdentifier
    subscripts <- many (between pLBracket pRBracket pInitSubscript)
    varInit <- option Nothing (Just <$> (pAssign >> pExpr))
    return $ AVarInit { aviId=ident, aviSubscripts=subscripts, aviInit=varInit }

pInitSubscript :: TokenParser AVarSubscript
pInitSubscript = do
    (AVSId <$> pIdentifier)
    <|> (AVSInt <$> pInteger)
    <|> return AVSEmpty

pBlock :: TokenParser ABlock
pBlock = do
    commands <- between pLBrace pRBrace (many pCommand)
    return $ ABlock commands
    <?> "block of commands"

pCommand :: TokenParser ACommand
pCommand =
    pIfCommand
    <|> pWhileCommand
    <|> pReturnCommand
    <|> pBlockCommand
    <|> (try pVarDecCommand)
    <|> pExprCommand
    <?> "command";

pIfCommand :: TokenParser ACommand
pIfCommand = do
    _ <- pIf
    testExpr <- between pLParen pRParen pExpr
    trueCommand <- pCommand
    falseCommand <- option Nothing (Just <$> (pElseCommand))
    return AIfCommand {
        aicTestExpr=testExpr
      , aicTrueCommand=trueCommand
      , aicFalseCommand=falseCommand
    }

pElseCommand :: TokenParser ACommand
pElseCommand = do
    _ <- pElse
    pCommand

pWhileCommand :: TokenParser ACommand
pWhileCommand = do
    _ <- pWhile
    testExpr <- between pLParen pRParen pExpr
    command <- pCommand
    return AWhileCommand {
        awcTestExpr=testExpr
      , awcCommand=command
    }

pReturnCommand :: TokenParser ACommand
pReturnCommand = do
    _ <- pReturn
    expr <- option Nothing (Just <$> pExpr)
    _ <- pSemicolon
    return $ AReturnCommand expr

pVarDecCommand :: TokenParser ACommand
pVarDecCommand = AVarDecCommand <$> pVarDec

pExprCommand :: TokenParser ACommand
pExprCommand = do
    expr <- pExpr
    _ <- pSemicolon
    return $ AExprCommand expr

pBlockCommand :: TokenParser ACommand
pBlockCommand = ABlockCommand <$> pBlock

pExpr :: TokenParser AExpr
pExpr = chainl1 pLogicalOrExpr pAssignBinOp
        <?> "expression"

pLogicalOrExpr :: TokenParser AExpr
pLogicalOrExpr = chainl1 pLogicalAndExpr pLogOrBinOp

pLogicalAndExpr :: TokenParser AExpr
pLogicalAndExpr = chainl1 pBinOrExpr pLogAndBinOp

pBinOrExpr :: TokenParser AExpr
pBinOrExpr = chainl1 pBinAndExpr pBinOrBinOp

pBinAndExpr :: TokenParser AExpr
pBinAndExpr = chainl1 pEqExpr pBinAndBinOp

pEqExpr :: TokenParser AExpr
pEqExpr = chainl1 pRelationalExpr pEqBinOp

pRelationalExpr :: TokenParser AExpr
pRelationalExpr = chainl1 pAddExpr pRelationalBinOp

pAddExpr :: TokenParser AExpr
pAddExpr = chainl1 pMultExpr pAddBinOp

pMultExpr :: TokenParser AExpr
pMultExpr = chainl1 pMaybeCastExpr pMultBinOp

pAssignBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pAssignBinOp = pBinOp (pAssign
                       <|> pPlusAssign
                       <|> pMinusAssign
                       <|> pMultAssign
                       <|> pDivAssign)

pLogOrBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pLogOrBinOp = pBinOp pLogOr

pLogAndBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pLogAndBinOp = pBinOp pLogAnd

pBinOrBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pBinOrBinOp = pBinOp pBinOr

pBinAndBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pBinAndBinOp = pBinOp pBinAnd

pEqBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pEqBinOp = pBinOp (pEq <|> pNeq)

pRelationalBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pRelationalBinOp = pBinOp (pLtEq <|> pGtEq <|> pLt <|> pGt)

pAddBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pAddBinOp = pBinOp (pPlus <|> pMinus)

pMultBinOp :: TokenParser (AExpr -> AExpr -> AExpr)
pMultBinOp = pBinOp (pMult <|> pDiv)

pBinOp :: TokenParser String -> TokenParser (AExpr -> AExpr -> AExpr)
pBinOp opParser = do
    opString <- opParser
    return (\expr1 expr2 -> ABinOpExpr {
          aboeExpr1=expr1
        , aboeExpr2=expr2
        , aboeOp=opString
      })

pMaybeCastExpr :: TokenParser AExpr
pMaybeCastExpr = (try pCastExpr) <|> pMaybeUnaryExpr

pCastExpr :: TokenParser AExpr
pCastExpr = do
    typeName <- between pLParen pRParen pTypeName
    castExpr <- pMaybeCastExpr
    return ACastExpr {
        aceTypeName=typeName
      , aceCastExpr=castExpr
    }

pMaybeUnaryExpr :: TokenParser AExpr
pMaybeUnaryExpr = pUnaryExpr <|> pMaybePostfixExpr

pUnaryExpr :: TokenParser AExpr
pUnaryExpr = do
    op <- (pInc <|> pDec <|> pPlus <|> pMinus <|> pBinAnd <|> pMult <|> pNot)
    expr <- pMaybeUnaryExpr
    return AUnaryOpExpr {
        auoeExpr=expr
      , auoeOp=op
    }

pMaybePostfixExpr :: TokenParser AExpr
pMaybePostfixExpr = do
    primaryExpr <- pPrimaryExpr
    pMaybePostfixExpr' primaryExpr

pMaybePostfixExpr' :: AExpr -> TokenParser AExpr
pMaybePostfixExpr' rootExpr = do
    pIndexExpr rootExpr
    <|> pCallExpr rootExpr
    <|> pMemberExpr rootExpr
    <|> pPostOpExpr rootExpr
    <|> (return rootExpr)

pIndexExpr :: AExpr -> TokenParser AExpr
pIndexExpr rootExpr = do
    indexExpr <- between pLBracket pRBracket pExpr
    pMaybePostfixExpr' AIndexExpr {
        aieExpr=rootExpr
      , aieIndexExpr=indexExpr
    }

pCallExpr :: AExpr -> TokenParser AExpr
pCallExpr rootExpr = do
    params <- between pLParen pRParen (sepBy pExpr pComma)
    pMaybePostfixExpr' ACallExpr {
        aceFunctionExpr=rootExpr
      , aceParams=params
    }

pMemberExpr :: AExpr -> TokenParser AExpr
pMemberExpr rootExpr = do
    _ <- pDot
    member <- pIdentifier
    pMaybePostfixExpr' AMemberExpr {
        ameExpr=rootExpr
      , ameMember=member
    }

pPostOpExpr :: AExpr -> TokenParser AExpr
pPostOpExpr rootExpr = do
    op <- (pInc <|> pDec)
    pMaybePostfixExpr' APostOpExpr {
        apoeExpr=rootExpr
      , apoeOp=op
    }

pPrimaryExpr :: TokenParser AExpr
pPrimaryExpr = do
    (AParenExpr <$> (between pLParen pRParen pExpr))
    <|> (AFloatExpr <$> pFloat)
    <|> (AIntExpr <$> pInteger)
    <|> (AStringExpr <$> pString)
    <|> (AIdentifierExpr <$> pIdentifier)
    <|> (pFalse >> return AFalse)
    <|> (pTrue >> return ATrue)
    <|> (pThis >> return AThisExpr)
    <|> pNewExpr

pNewExpr :: TokenParser AExpr
pNewExpr = do
    _ <- pNew
    typeName <- pTypeName
    sizeExpr <- between pLBracket pRBracket pExpr
    return ANewExpr {
        aneTypeName=typeName
      , aneSizeExpr=sizeExpr
    }

pTypeName :: TokenParser ATypeName
pTypeName = do
    name <- pIdentifier
    isPointer <- option False (pMult >> return True)
    return ATypeName { atnName=name, atnIsPointer=isPointer }
    <?> "type name"

pInteger :: TokenParser Integer
pInteger =
    matchToken test
    where
      test (TInteger i) = Just i
      test _ = Nothing

pFloat :: TokenParser Double
pFloat =
    matchToken test
    where
      test (TFloat i) = Just i
      test _ = Nothing

pString :: TokenParser String
pString =
    matchToken test
    where
      test (TString i) = Just i
      test _ = Nothing

pIdentifier :: TokenParser String
pIdentifier =
    matchToken test
    where
      test (TIdentifier s) = Just s
      test _ = Nothing

pAssign :: TokenParser String
pAssign = matchToken' TAssign

pBinAnd :: TokenParser String
pBinAnd = matchToken' TBinAnd

pBinOr :: TokenParser String
pBinOr = matchToken' TBinOr

pComma :: TokenParser String
pComma = matchToken' TComma

pDiv :: TokenParser String
pDiv = matchToken' TDiv

pDivAssign :: TokenParser String
pDivAssign = matchToken' TDivAssign

pDec :: TokenParser String
pDec = matchToken' TDec

pDot :: TokenParser String
pDot = matchToken' TDot

pElse :: TokenParser String
pElse = matchToken' TElse

pEnum :: TokenParser String
pEnum = matchToken' TEnum

pEq :: TokenParser String
pEq = matchToken' TEq

pEof :: TokenParser String
pEof = matchToken' TEof

pExport :: TokenParser String
pExport = matchToken' TExport

pFalse :: TokenParser String
pFalse = matchToken' TFalse

pFunction :: TokenParser String
pFunction = matchToken' TFunction

pGt :: TokenParser String
pGt = matchToken' TGt

pGtEq :: TokenParser String
pGtEq = matchToken' TGtEq

pIf :: TokenParser String
pIf = matchToken' TIf

pInc :: TokenParser String
pInc = matchToken' TInc

pImport :: TokenParser String
pImport = matchToken' TImport

pLBrace :: TokenParser String
pLBrace = matchToken' TLBrace

pLBracket :: TokenParser String
pLBracket = matchToken' TLBracket

pLogAnd :: TokenParser String
pLogAnd = matchToken' TLogAnd

pLogOr :: TokenParser String
pLogOr = matchToken' TLogOr

pLParen :: TokenParser String
pLParen = matchToken' TLParen

pLt :: TokenParser String
pLt = matchToken' TLt

pLtEq :: TokenParser String
pLtEq = matchToken' TLtEq

pMinus :: TokenParser String
pMinus = matchToken' TMinus

pMinusAssign :: TokenParser String
pMinusAssign = matchToken' TMinusAssign

pMult :: TokenParser String
pMult = matchToken' TMult

pMultAssign :: TokenParser String
pMultAssign = matchToken' TMultAssign

pNeq :: TokenParser String
pNeq = matchToken' TNeq

pNew :: TokenParser String
pNew = matchToken' TNew

pNot :: TokenParser String
pNot = matchToken' TNot

pPlus :: TokenParser String
pPlus = matchToken' TPlus

pPlusAssign :: TokenParser String
pPlusAssign = matchToken' TPlusAssign

pRBrace :: TokenParser String
pRBrace = matchToken' TRBrace

pRBracket :: TokenParser String
pRBracket = matchToken' TRBracket

pReturn :: TokenParser String
pReturn = matchToken' TReturn

pRParen :: TokenParser String
pRParen = matchToken' TRParen

pSemicolon :: TokenParser String
pSemicolon = matchToken' TSemicolon

pStatic :: TokenParser String
pStatic = matchToken' TStatic

pStruct :: TokenParser String
pStruct = matchToken' TStruct

pThis :: TokenParser String
pThis = matchToken' TThis

pTrue :: TokenParser String
pTrue = matchToken' TTrue

pWhile :: TokenParser String
pWhile = matchToken' TWhile

matchToken' :: Tok -> TokenParser String
matchToken' tok = matchToken (\t -> if t == tok then Just (show t) else Nothing)

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
dumpTok (pos, _decs, tok) = putStrLn $ show (sourceLine pos) ++ " " ++ show tok

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
    <|> scanTokFromChar'' '+' TPlus '+' TInc '=' TPlusAssign
    <|> scanTokFromChar'' '-' TMinus '-' TDec '=' TMinusAssign
    <|> scanTokFromChar' '!' TNot '=' TNeq
    <|> scanTokFromChar' '&' TBinAnd '&' TLogAnd
    <|> scanTokFromChar' '*' TMult '=' TMultAssign
    <|> scanTokFromChar' '/' TDiv '=' TDivAssign
    <|> scanTokFromChar' '<' TLt '=' TLtEq
    <|> scanTokFromChar' '=' TAssign '=' TEq
    <|> scanTokFromChar' '>' TGt '=' TGtEq
    <|> scanTokFromChar' '|' TBinOr '|' TLogOr
    <|> scanTokFromChar '(' TLParen
    <|> scanTokFromChar ')' TRParen
    <|> scanTokFromChar ',' TComma
    <|> scanTokFromChar '.' TDot
    <|> scanTokFromChar ';' TSemicolon
    <|> scanTokFromChar '[' TLBracket
    <|> scanTokFromChar ']' TRBracket
    <|> scanTokFromChar '{' TLBrace
    <|> scanTokFromChar '}' TRBrace
    <|> scanTokFromString "::" TMember
    <|> try (TFloat <$> float)
    <|> TInteger <$> integer
    <|> TString <$> stringLiteral
    <|> TIdentifier <$> identifier

scanTokFromString :: String -> Tok -> Parser Tok
scanTokFromString s t = lexeme $ string s >> return t

scanTokFromChar :: Char -> Tok -> Parser Tok
scanTokFromChar c t = lexeme $ char c >> return t

scanTokFromChar' :: Char -> Tok -> Char -> Tok -> Parser Tok
scanTokFromChar' c1 t1 c2 t2 =
  lexeme $ char c1 >> ((char c2 >> return t2) <|> return t1)

scanTokFromChar'' :: Char -> Tok -> Char -> Tok -> Char -> Tok -> Parser Tok
scanTokFromChar'' c1 t1 c2 t2 c3 t3 =
  lexeme $ char c1 >> ((char c2 >> return t2)
                       <|> (char c3 >> return t3)
                       <|> return t1)

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
    s <- many1Till anyChar toNonCommentEnd
    return $ (pos, PTContent s)

toNonCommentEnd :: Parser String
toNonCommentEnd =
    lookAhead (
        (eof >> return "")
         <|> (string "\r")
         <|> (string "\n")
         <|> try (string "/" >> (string "*" <|> string "/"))
    )

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

eol :: Parser String
eol = try (string "\r\n")
      <|> string "\r"
      <|> string "\n"
      <?> "end of line"
