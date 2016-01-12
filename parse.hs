import Control.Monad
import Control.Monad.State
import Data.Functor.Identity
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
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
    files <- sort <$> filter isSourceFile <$> (getFiles cartDir)
    mapM_ parseFile files
    --let file = cartDir </> "AskOnly.asc"
    --parseFile file

isSourceFile :: String -> Bool
isSourceFile s = isSuffixOf ".asc" s || isSuffixOf ".ash" s

parseFile :: String -> IO ()
parseFile filePath = do
    print filePath
    content <- fileContent filePath
    let toks = cartScan content
    let eitherAst = parse cartParse "(parser input)" =<< toks
    let eitherAst2 = rewriteWith blockify <$> eitherAst
    --dumpToks $ take 200 <$> toks
    case eitherAst2 of
        Left err -> putStrLn $ show err
        Right ast2 -> writeFile (filePath <> "_b") $ renderToString $ render $ ast2

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
    deriving (Show, Eq)

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

data ATopLevel =
    AEof [Decoration]
    | AFunctionDec {
        afdSignature :: AFunctionSignature
      , afdBlock :: ABlock
    }
    | ATopLevelVarDec AVarDec
    | ATopLevelImportDec AImportDec
    | AEnumDec {
        aedName :: String
      , aedValues :: [String]
      , aedEnumDecs :: [Decoration]
      , aedNameDecs :: [Decoration]
      , aedLBraceDecs :: [Decoration]
      , aedValueDecs :: [[Decoration]]
      , aedCommaDecs :: [[Decoration]]
      , aedRBraceDecs :: [Decoration]
      , aedSemicolonDecs :: [Decoration]
    }
    | AStructDec {
        asdName :: String
      , asdMembers :: [AStructMember]
      , asdStructDecs :: [Decoration]
      , asdNameDecs :: [Decoration]
      , asdLBraceDecs :: [Decoration]
      , asdRBraceDecs :: [Decoration]
      , asdSemicolonDecs :: [Decoration]
    }
    | AExportDec {
        axdName :: String
      , axdExportDecs :: [Decoration]
      , axdNameDecs :: [Decoration]
      , axdSemicolonDecs :: [Decoration]
    }
    deriving (Show)

data AStructMember =
    AStructMemberImport AImportDec
    | AStructMemberVar AVarDec
    deriving (Show)

data AImportDec =
    AImportFunctionSig {
        aifsFunctionSig :: AFunctionSignature
      , aifsImportDecs :: [Decoration]
      , aifsSemicolonDecs :: [Decoration]
    }
    | AImportVarDec {
        aivdVarDec :: AVarDec
      , aivdImportDecs :: [Decoration]
    }
    deriving (Show)

data AFunctionSignature =
    AFunctionSignature {
        afsIsStatic :: Bool
      , afsReturnType :: String
      , afsName :: String
      , afsMember :: Maybe String
      , afsParams :: [AFunctionDecParam]
      , afsStaticDecs :: [Decoration]
      , afsReturnTypeDecs :: [Decoration]
      , afsNameDecs :: [Decoration]
      , afsMemberDecs :: [Decoration]
      , afsMemberNameDecs :: [Decoration]
      , afsLParenDecs :: [Decoration]
      , afsCommaDecs :: [[Decoration]]
      , afsRParenDecs :: [Decoration]
    }
    deriving (Show)

data AFunctionDecParam =
    AFunctionDecExtenderParam {
        afdepName :: String
      , afdepThisDecs :: [Decoration]
      , afdepNameDecs :: [Decoration]
      , afdepMultDecs :: [Decoration]
    }
    | AFunctionDecRegularParam {
        afdrpTypeName :: ATypeName
      , afdrpVarInit :: AVarInit
    }
    deriving (Show)

data ABlock =
    ABlock {
        abCommands :: [ACommand]
      , abLBraceDecs :: [Decoration]
      , abRBraceDecs :: [Decoration]
    }
    deriving (Show)

data ACommand =
    AIfCommand {
        aicTestExpr :: AExpr
      , aicTrueCommand :: ACommand
      , aicFalseCommand :: Maybe ACommand
      , aicIfDecs :: [Decoration]
      , aicLParenDecs :: [Decoration]
      , aicRParenDecs :: [Decoration]
    }
    | AElseCommand {
          aecCommand :: ACommand
        , aecElseDecs :: [Decoration]
      }
    | AWhileCommand {
          awcTestExpr :: AExpr
        , awcCommand :: ACommand
        , awcWhileDecs :: [Decoration]
        , awcLParenDecs :: [Decoration]
        , awcRParenDecs :: [Decoration]
      }
    | AReturnCommand {
          arcExpr :: Maybe AExpr
        , arcReturnDecs :: [Decoration]
        , arcSemicolonDecs :: [Decoration]
      }
    | AVarDecCommand AVarDec
    | AExprCommand {
          aecExpr :: AExpr
        , aecSemicolonDecs :: [Decoration]
      }
    | ABlockCommand ABlock
    deriving (Show)

data AVarDec =
    AVarDec {
        avdTypeName :: ATypeName
      , avdVars :: [AVarInit]
      , avdCommaDecs :: [[Decoration]]
      , avdSemicolonDecs :: [Decoration]
    }
    deriving (Show)

data AVarInit =
    AVarInit {
        aviId :: String
      , aviSubscripts :: [AVarSubscript]
      , aviInit :: Maybe AExpr
      , aviIdDecs :: [Decoration]
      , aviLBracketDecs :: [[Decoration]]
      , aviRBracketDecs :: [[Decoration]]
      , aviAssignDecs :: [Decoration]
    }
    deriving (Show)

data AVarSubscript =
    AVarSubscriptEmpty
    | AVarSubscriptId {
          avsidId :: String
        , avsidIdDecs :: [Decoration]
      }
    | AVarSubscriptInt {
          avsiInt :: Integer
        , avsiIntDecs :: [Decoration]
      }
    deriving (Show)

data AExpr =
    AFalseExpr {
        afeFalseDecs :: [Decoration]
    }
    | ATrueExpr {
          ateTrueDecs :: [Decoration]
      }
    | AThisExpr {
          ateThisDecs :: [Decoration]
      }
    | ANewExpr {
          aneTypeName :: ATypeName
        , aneSizeExpr :: AExpr
        , aneNewDecs :: [Decoration]
        , aneLBracketDecs :: [Decoration]
        , aneRBracketDecs :: [Decoration]
      }
    | AFloatExpr {
          afeFloat :: Double
        , afeFloatDecs :: [Decoration]
      }
    | AIntExpr {
          aieInt :: Integer
        , aieIntDecs :: [Decoration]
      }
    | AStringExpr {
          aseString :: String
        , aseStringDecs :: [Decoration]
      }
    | AIdentifierExpr {
          aieId :: String
        , aieIdDecs :: [Decoration]
      }
    | AIndexExpr {
          aieExpr :: AExpr
        , aieIndexExpr :: AExpr
        , aieLBracketDecs :: [Decoration]
        , aieRBracketDecs :: [Decoration]
      }
    | ACallExpr {
          aceFunctionExpr :: AExpr
        , aceParams :: [AExpr]
        , aceLParenDecs :: [Decoration]
        , aceRParenDecs :: [Decoration]
        , aceCommaDecs :: [[Decoration]]
      }
    | AMemberExpr {
          ameExpr :: AExpr
        , ameMember :: String
        , ameDotDecs :: [Decoration]
        , ameMemberDecs :: [Decoration]
      }
    | AUnaryOpExpr {
          auoeExpr :: AExpr
        , auoeOp :: String
        , auoeOpDecs :: [Decoration]
      }
    | APostOpExpr {
          apoeExpr :: AExpr
        , apoeOp :: String
        , apoeOpDecs :: [Decoration]
      }
    | ABinOpExpr {
          aboeExpr1 :: AExpr
        , aboeExpr2 :: AExpr
        , aboeOp :: String
        , aboeOpDecs :: [Decoration]
      }
    | AParenExpr {
          apeExpr :: AExpr
        , apeLParenDecs :: [Decoration]
        , apeRParenDecs :: [Decoration]
      }
    | ACastExpr {
          aceTypeName :: ATypeName
        , aceCastExpr :: AExpr
        , aceLParenDecs :: [Decoration]
        , aceRParenDecs :: [Decoration]
      }
    deriving (Show)

data ATypeName =
    ATypeName {
        atnName :: String
      , atnIsPointer :: Bool
      , atnNameDecs :: [Decoration]
      , atnMultDecs :: Maybe [Decoration]
    }
    deriving (Show)

type TokenParser a = GenParser Token () a
type TokenDecParser a = TokenParser ([Decoration], a)
type PreTokenParser a = GenParser PreToken () a

data Rewriter = Rewriter {
    rewriteAst :: Rewriter -> Ast -> Ast
  , rewriteTopLevel :: Rewriter -> ATopLevel -> ATopLevel
  , rewriteStructMember :: Rewriter -> AStructMember -> AStructMember
  , rewriteImportDec :: Rewriter -> AImportDec -> AImportDec
  , rewriteFunctionSignature :: Rewriter -> AFunctionSignature -> AFunctionSignature
  , rewriteFunctionDecParam :: Rewriter -> AFunctionDecParam -> AFunctionDecParam
  , rewriteBlock :: Rewriter -> ABlock -> ABlock
  , rewriteCommand :: Rewriter -> ACommand -> ACommand
  , rewriteVarDec :: Rewriter -> AVarDec -> AVarDec
  , rewriteVarInit :: Rewriter -> AVarInit -> AVarInit
  , rewriteVarSubscript :: Rewriter -> AVarSubscript -> AVarSubscript
  , rewriteExpr :: Rewriter -> AExpr -> AExpr
  , rewriteTypeName :: Rewriter -> ATypeName -> ATypeName
  }

visitingRewriter :: Rewriter
visitingRewriter = Rewriter {
    rewriteAst =
      \r (Ast ts) -> Ast $ rewriteTopLevel r r <$> ts
  , rewriteTopLevel = \r a -> case a of
      AFunctionDec {
        afdSignature=signature
      , afdBlock=block
      } -> a {
        afdSignature=rewriteFunctionSignature r r signature
      , afdBlock=rewriteBlock r r block
      }
      ATopLevelVarDec b -> ATopLevelVarDec $ rewriteVarDec r r b
      ATopLevelImportDec b -> ATopLevelImportDec $ rewriteImportDec r r b
      AStructDec {
        asdMembers=members
      } -> a {
        asdMembers=rewriteStructMember r r <$> members
      }
      _ -> a
  , rewriteStructMember = \r a -> case a of
      AStructMemberImport b -> AStructMemberImport $ rewriteImportDec r r b
      AStructMemberVar b -> AStructMemberVar $ rewriteVarDec r r b
  , rewriteImportDec = \r a -> case a of
      AImportFunctionSig {
        aifsFunctionSig=functionSig
      } -> a {
        aifsFunctionSig=rewriteFunctionSignature r r functionSig
      }
      AImportVarDec {
        aivdVarDec=varDec
      } -> a {
        aivdVarDec=rewriteVarDec r r varDec
      }
  , rewriteFunctionSignature = \r a -> case a of
      AFunctionSignature {
        afsParams=params
      } -> a {
        afsParams=rewriteFunctionDecParam r r <$> params
      }
  , rewriteFunctionDecParam = \r a -> case a of
      AFunctionDecRegularParam {
        afdrpTypeName=typeName
      , afdrpVarInit=varInit
      } -> a {
        afdrpTypeName=rewriteTypeName r r typeName
      , afdrpVarInit=rewriteVarInit r r varInit
      }
      _ -> a
  , rewriteBlock = \r a -> case a of
      ABlock {
        abCommands=commands
      } -> a {
        abCommands=rewriteCommand r r <$> commands
      }
  , rewriteCommand = \r a -> case a of
      AIfCommand {
        aicTestExpr=testExpr
      , aicTrueCommand=trueCommand
      , aicFalseCommand=falseCommand
      } -> a {
        aicTestExpr=rewriteExpr r r testExpr
      , aicTrueCommand=rewriteCommand r r trueCommand
      , aicFalseCommand=rewriteCommand r r <$> falseCommand
      }
      AElseCommand {
        aecCommand=command
      } -> a {
        aecCommand=rewriteCommand r r command
      }
      AWhileCommand {
        awcTestExpr=testExpr
      , awcCommand=command
      } -> a {
        awcTestExpr=rewriteExpr r r testExpr
      , awcCommand=rewriteCommand r r command
      }
      AReturnCommand {
        arcExpr=expr
      } -> a {
        arcExpr=rewriteExpr r r <$> expr
      }
      AVarDecCommand b -> AVarDecCommand $ rewriteVarDec r r b
      AExprCommand {
        aecExpr=expr
      } -> a {
        aecExpr=rewriteExpr r r expr
      }
      ABlockCommand b -> ABlockCommand $ rewriteBlock r r b
  , rewriteVarDec = \r a -> case a of
    AVarDec {
        avdTypeName=typeName
      , avdVars=vars
      } -> a {
        avdTypeName=rewriteTypeName r r typeName
      , avdVars=rewriteVarInit r r <$> vars
      }
  , rewriteVarInit = \r a -> case a of
      AVarInit {
        aviSubscripts=subscripts
      , aviInit=varInit
      } -> a {
        aviSubscripts=rewriteVarSubscript r r <$> subscripts
      , aviInit=rewriteExpr r r <$> varInit
      }
  , rewriteVarSubscript = \_r -> id
  , rewriteExpr = \r a -> case a of
      ANewExpr {
        aneTypeName=typeName
      , aneSizeExpr=sizeExpr
      } -> a {
        aneTypeName=rewriteTypeName r r typeName
      , aneSizeExpr=rewriteExpr r r sizeExpr
      }
      AIndexExpr {
        aieExpr=expr
      , aieIndexExpr=indexExpr
      } -> a {
        aieExpr=rewriteExpr r r expr
      , aieIndexExpr=rewriteExpr r r indexExpr
      }
      ACallExpr {
        aceFunctionExpr=functionExpr
      , aceParams=params
      } -> a {
        aceFunctionExpr=rewriteExpr r r functionExpr
      , aceParams=rewriteExpr r r <$> params
      }
      AMemberExpr {
        ameExpr=expr
      } -> a {
        ameExpr=rewriteExpr r r expr
      }
      AUnaryOpExpr {
        auoeExpr=expr
      } -> a {
        auoeExpr=rewriteExpr r r expr
      }
      APostOpExpr {
        apoeExpr=expr
      } -> a {
        apoeExpr=rewriteExpr r r expr
      }
      ABinOpExpr {
        aboeExpr1=expr1
      , aboeExpr2=expr2
      } -> a {
        aboeExpr1=rewriteExpr r r expr1
      , aboeExpr2=rewriteExpr r r expr2
      }
      AParenExpr {
        apeExpr=expr
      } -> a {
        apeExpr=rewriteExpr r r expr
      }
      ACastExpr {
        aceTypeName=typeName
      , aceCastExpr=castExpr
      } -> a {
        aceTypeName=rewriteTypeName r r typeName
      , aceCastExpr=rewriteExpr r r castExpr
      }
      _ -> a
  , rewriteTypeName = \_r -> id
  }

blockify :: Rewriter
blockify = visitingRewriter {
  rewriteCommand = \r a ->
    let
      ensureBlocked :: ACommand -> ACommand
      ensureBlocked c@(ABlockCommand _) = c
      ensureBlocked c = ABlockCommand $ ABlock {
          abCommands=[c]
        , abLBraceDecs=[]
        , abRBraceDecs=[]
        }
    in case a of
      AIfCommand {
        aicTestExpr=testExpr
      , aicTrueCommand=trueCommand
      , aicFalseCommand=falseCommand
      } -> a {
        aicTestExpr=rewriteExpr r r testExpr
      , aicTrueCommand=ensureBlocked $ rewriteCommand r r trueCommand
      , aicFalseCommand=rewriteCommand r r <$> falseCommand
      }
      AElseCommand {
        aecCommand=command@(AIfCommand {})
      } -> a {
        aecCommand=rewriteCommand r r command
      }
      AElseCommand {
        aecCommand=command
      } -> a {
        aecCommand=ensureBlocked $ rewriteCommand r r command
      }
      AWhileCommand {
        awcTestExpr=testExpr
      , awcCommand=command
      } -> a {
        awcTestExpr=rewriteExpr r r testExpr
      , awcCommand=ensureBlocked $ rewriteCommand r r command
      }
      _ -> rewriteCommand visitingRewriter r a
}

rewriteWith :: Rewriter -> Ast -> Ast
rewriteWith r = (rewriteAst r) r

-- Data.ByteString might be more performant?
data Doc =
    Text ([Decoration], String)
    | Line
    | Space
    deriving (Show, Eq)

class Renderable a where
    render :: a -> [Doc]

instance Renderable Ast where
    render (Ast topLevels) = concat (render <$> topLevels)

instance Renderable ATopLevel where
    render (AEof decs) = [Text (decs, "")]
    render AFunctionDec {
        afdSignature=signature
      , afdBlock=block
    } = render signature ++ [Space] ++ render block ++ [Line]
    render (ATopLevelVarDec varDec) = render varDec ++ [Line]
    render (ATopLevelImportDec importDec) = render importDec ++ [Line]
    render AEnumDec {
        aedName=name
      , aedValues=values
      , aedEnumDecs=enumDecs
      , aedNameDecs=nameDecs
      , aedLBraceDecs=lBraceDecs
      , aedValueDecs=valueDecs
      , aedCommaDecs=commaDecs
      , aedRBraceDecs=rBraceDecs
      , aedSemicolonDecs=semicolonDecs
    } = [
          Text (enumDecs, "enum")
        , Space
        , Text (nameDecs, name)
        , Space
        , Text (lBraceDecs, "{")
        ]
        ++ renderEnumValues values valueDecs commaDecs
        ++ [
          Text (rBraceDecs, "}")
        , Text (semicolonDecs, ";")
        , Line
        ]
    render AStructDec {
        asdName=name
      , asdMembers=members
      , asdStructDecs=structDecs
      , asdNameDecs=nameDecs
      , asdLBraceDecs=lBraceDecs
      , asdRBraceDecs=rBraceDecs
      , asdSemicolonDecs=semicolonDecs
    } = [
          Text (structDecs, "struct")
        , Space
        , Text (nameDecs, name)
        , Space
        ]
        ++ renderBetween (lBraceDecs, "{") (rBraceDecs, "}") members
        ++ [Text (semicolonDecs, ";"), Line]
    render AExportDec {
        axdName=name
      , axdExportDecs=exportDecs
      , axdNameDecs=nameDecs
      , axdSemicolonDecs=semicolonDecs
    } = [
          Text (exportDecs, "export")
        , Space
        , Text (nameDecs, name)
        , Text (semicolonDecs, ";")
        , Line
        ]

renderEnumValues :: [String] -> [[Decoration]] -> [[Decoration]] -> [Doc]
renderEnumValues [] [] [] = []
renderEnumValues values valueDecs commaDecs =
  [Line] ++ renderInterleaved valueDocs commaDocs ++ [Line]
  where
    valueDocs = (\p -> [Text p]) <$> zip valueDecs values
    commaDocs = renderCommaLine <$> commaDecs
    renderCommaLine dec = [Text (dec, ","), Line]

instance Renderable ABlock where
    render ABlock {
        abCommands=[]
      , abLBraceDecs=lBraceDecs
      , abRBraceDecs=rBraceDecs
    } = [Text (lBraceDecs, "{") , Text (rBraceDecs, "}")]
    render ABlock {
        abCommands=commands
      , abLBraceDecs=lBraceDecs
      , abRBraceDecs=rBraceDecs
    } = [Text (lBraceDecs, "{"), Line]
        ++ intercalate [Line] (render <$> commands)
        ++ [Line, Text (rBraceDecs, "}")]

instance Renderable ACommand where
    render AIfCommand {
        aicTestExpr=testExpr
      , aicTrueCommand=trueCommand
      , aicFalseCommand=falseCommand
      , aicIfDecs=ifDecs
      , aicLParenDecs=lParenDecs
      , aicRParenDecs=rParenDecs
    } = [
          Text (ifDecs, "if")
        , Space
        , Text (lParenDecs, "(")
        ]
        ++ render testExpr
        ++ [Text (rParenDecs, ")"), Space]
        ++ render trueCommand
        ++ maybe [] render falseCommand
    render AElseCommand {
          aecCommand=command
        , aecElseDecs=elseDecs
      } = [
            Space
          , Text (elseDecs, "else")
          , Space
          ]
          ++ render command
    render AWhileCommand {
        awcTestExpr=testExpr
      , awcCommand=command
      , awcWhileDecs=whileDecs
      , awcLParenDecs=lParenDecs
      , awcRParenDecs=rParenDecs
    } = [
          Text (whileDecs, "while")
        , Space
        , Text (lParenDecs, "(")
        ]
        ++ render testExpr
        ++ [Text (rParenDecs, ")"), Space]
        ++ render command
    render AReturnCommand {
        arcExpr=Nothing
      , arcReturnDecs=returnDecs
      , arcSemicolonDecs=semicolonDecs
    } = [
          Text (returnDecs, "return")
        , Text (semicolonDecs, ";")
        ]
    render AReturnCommand {
        arcExpr=Just expr
      , arcReturnDecs=returnDecs
      , arcSemicolonDecs=semicolonDecs
    } = [Text (returnDecs, "return")]
        ++ render expr
        ++ [Text (semicolonDecs, ";")]
    render (AVarDecCommand varDec) = render varDec
    render AExprCommand {
        aecExpr=expr
      , aecSemicolonDecs=semicolonDecs
    } = render expr ++ [Text (semicolonDecs, ";")]
    render (ABlockCommand block) = render block

instance Renderable AStructMember where
    render (AStructMemberImport imp) = render imp
    render (AStructMemberVar var) = render var

instance Renderable AImportDec where
    render AImportFunctionSig {
        aifsFunctionSig=functionSig
      , aifsImportDecs=importDecs
      , aifsSemicolonDecs=semicolonDecs
    } = [Text (importDecs, "import"), Space]
        ++ render functionSig
        ++ [Text (semicolonDecs, ";")]
    render AImportVarDec {
        aivdVarDec=varDec
      , aivdImportDecs=importDecs
    } = [Text (importDecs, "import"), Space] ++ render varDec

instance Renderable AFunctionSignature where
    render AFunctionSignature {
        afsIsStatic=isStatic
      , afsReturnType=returnType
      , afsName=name
      , afsMember=member
      , afsParams=params
      , afsStaticDecs=staticDecs
      , afsReturnTypeDecs=returnTypeDecs
      , afsMemberDecs=memberDecs
      , afsNameDecs=nameDecs
      , afsMemberNameDecs=memberNameDecs
      , afsLParenDecs=lParenDecs
      , afsCommaDecs=commaDecs
      , afsRParenDecs=rParenDecs
    } = (if isStatic then [Text (staticDecs, "static"), Space] else [])
        ++ [Text (returnTypeDecs, if returnType == "void" then "function" else "returnType"), Space]
        ++ [Text (nameDecs, name)]
        ++ maybe [] (\s -> [Text (memberDecs, "::"), Text (memberNameDecs, s)]) member
        ++ [Text (lParenDecs, "(")]
        ++ renderInterleaved paramDocs commaDocs
        ++ [Text (rParenDecs, ")")]
        where
          paramDocs = render <$> params
          commaDocs = renderComma <$> commaDecs

instance Renderable AFunctionDecParam where
    render AFunctionDecExtenderParam {
        afdepName=name
      , afdepThisDecs=thisDecs
      , afdepNameDecs=nameDecs
      , afdepMultDecs=multDecs
    } = [
          Text (thisDecs, "this")
        , Space
        , Text (nameDecs, name)
        , Text (multDecs, "*")
        ]
    render AFunctionDecRegularParam {
        afdrpTypeName=typeName
      , afdrpVarInit=varInit
    } = render typeName ++ [Space] ++ render varInit

instance Renderable AVarDec where
    render AVarDec {
        avdTypeName=name
      , avdVars=vars
      , avdCommaDecs=commaDecs
      , avdSemicolonDecs=semicolonDecs
    } = render name
        ++ [Space]
        ++ renderInterleaved varDocs sepDocs
        ++ [Text (semicolonDecs, ";")]
        where
          varDocs = render <$> vars
          sepDocs = renderComma <$> commaDecs

instance Renderable ATypeName where
    render ATypeName {
        atnName=name
      , atnIsPointer=isPointer
      , atnNameDecs=nameDecs
      , atnMultDecs=multDecs
    } = [Text (nameDecs, name)]
        ++ (if isPointer then
              [Space, Text (maybe [] id multDecs, "*")]
            else [])

instance Renderable AVarInit where
    render AVarInit {
        aviId=varId
      , aviSubscripts=subscripts
      , aviInit=initializer
      , aviIdDecs=idDecs
      , aviLBracketDecs=lBracketDecs
      , aviRBracketDecs=rBracketDecs
      , aviAssignDecs=assignDecs
    } = [Text (idDecs, varId)]
        ++ (concat $ zipWith3 renderSubscript lBracketDecs subscripts rBracketDecs)
        ++ maybe [] (\e -> [Space, Text (assignDecs, "="), Space] ++ render e) initializer
        where
          renderSubscript lbDecs subscript rbDecs =
            [Text (lbDecs, "[")]
            ++ render subscript
            ++ [Text (rbDecs, "]")]

instance Renderable AVarSubscript where
    render AVarSubscriptEmpty = []
    render AVarSubscriptId {
      avsidId=ident
    , avsidIdDecs=idDecs
    } = [Text (idDecs, ident)]
    render AVarSubscriptInt {
      avsiInt=int_
    , avsiIntDecs=intDecs
    } = [Text (intDecs, show int_)]

instance Renderable AExpr where
    render AFalseExpr {
        afeFalseDecs=falseDecs
    } = [Text (falseDecs, "false")]
    render ATrueExpr {
        ateTrueDecs=trueDecs
    } = [Text (trueDecs, "true")]
    render AThisExpr {
        ateThisDecs=thisDecs
    } = [Text (thisDecs, "this")]
    render ANewExpr {
        aneTypeName=name
      , aneSizeExpr=expr
      , aneNewDecs=newDecs
      , aneLBracketDecs=lBracketDecs
      , aneRBracketDecs=rBracketDecs
    } = [Text (newDecs, "new"), Space]
        ++ render name
        ++ [Text (lBracketDecs, "[")]
        ++ render expr
        ++ [Text (rBracketDecs, "]")]
    render AFloatExpr {
        afeFloat=d
      , afeFloatDecs=floatDecs
    } = [Text $ (floatDecs, show d)]
    render AIntExpr {
        aieInt=int_
      , aieIntDecs=intDecs
    } = [Text (intDecs, show int_)]
    render AStringExpr {
        aseString=s
      , aseStringDecs=stringDecs
    } = [Text (stringDecs, formatStringLiteral s)]
    render AIdentifierExpr {
        aieId=ident
      , aieIdDecs=idDecs
    } = [Text (idDecs, ident)]
    render AIndexExpr {
        aieExpr=expr
      , aieIndexExpr=indexExpr
      , aieLBracketDecs=lBracketDecs
      , aieRBracketDecs=rBracketDecs
    } = render expr
        ++ [Text (lBracketDecs, "[")]
        ++ render indexExpr
        ++ [Text (rBracketDecs, "]")]
    render ACallExpr {
        aceFunctionExpr=expr
      , aceParams=params
      , aceLParenDecs=lParenDecs
      , aceRParenDecs=rParenDecs
      , aceCommaDecs=commaDecs
    } = render expr
        ++ [Text (lParenDecs, "(")]
        ++ renderInterleaved paramDocs commaDocs
        ++ [Text (rParenDecs, ")")]
        where
          paramDocs = render <$> params
          commaDocs = renderComma <$> commaDecs
    render AMemberExpr {
        ameExpr=expr
      , ameMember=member
      , ameDotDecs=dotDecs
      , ameMemberDecs=memberDecs
    } = render expr
        ++ [Text (dotDecs, "."), Text (memberDecs, member)]
    render AUnaryOpExpr {
        auoeExpr=expr
      , auoeOp=op
      , auoeOpDecs=opDecs
    } = [Text (opDecs, op)] ++ render expr
    render APostOpExpr {
        apoeExpr=expr
      , apoeOp=op
      , apoeOpDecs=opDecs
    } = render expr ++ [Text (opDecs, op)]
    render ABinOpExpr {
        aboeExpr1=expr1
      , aboeExpr2=expr2
      , aboeOp=op
      , aboeOpDecs=opDecs
    } = render expr1
        ++ [Space, Text (opDecs, op), Space]
        ++ render expr2
    render AParenExpr {
        apeExpr=expr
      , apeLParenDecs=lParenDecs
      , apeRParenDecs=rParenDecs
    } = [Text (lParenDecs, "(")]
        ++ render expr
        ++ [Text (rParenDecs, ")")]
    render ACastExpr {
        aceTypeName=typeName
      , aceCastExpr=castExpr
      , aceLParenDecs=lParenDecs
      , aceRParenDecs=rParenDecs
    } = [Text (lParenDecs, "(")]
        ++ render typeName
        ++ [Text (rParenDecs, ")"), Space]
        ++ render castExpr

formatStringLiteral :: String -> String
formatStringLiteral s =
  concat $ ["\""] ++ (escapeChar <$> s) ++ ["\""]
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\r' = "\\r"
    escapeChar '\n' = "\\n"
    escapeChar ch = [ch]

renderComma :: [Decoration] -> [Doc]
renderComma decs = [Text (decs, ","), Space]

renderInterleaved :: [[Doc]] -> [[Doc]] -> [Doc]
renderInterleaved [] [] = []
renderInterleaved [val] [] = val
renderInterleaved (valh:valt) (seph:sept) =
  valh ++ seph ++ renderInterleaved valt sept
renderInterleaved vals seps =
  error $ "wrong number of interleaved args"
    ++ (show $ length vals)
    ++ " "
    ++ (show $ length seps)

renderBetween :: (Renderable a) => ([Decoration], String) -> ([Decoration], String) -> [a] -> [Doc]
renderBetween start end [] = [Text start, Text end]
renderBetween start end items = [Text start, Line] ++ intercalate [Line] (fmap render items) ++ [Line, Text end]

renderToString :: [Doc] -> String
renderToString docs =
  intercalate "\n" renderedLines
  where
    renderedLines = concat $ fst $ runState ((mapM renderLine) docLines) 0
    docLines = splitOn [Line] docs

renderLine :: [Doc] -> State Int [String]
renderLine [] = return []
renderLine docs = do
  indent <- get
  let
      indentDeltas = getIndentDelta <$> docs
      minIndent = indent + (minimum $ cumSum indentDeltas)
      docLine = concat $ docToString <$> docs
      decs = concat $ getDecs <$> docs
      decLines = concat $ decToLines <$> decs
      indentString = renderIndent minIndent
  put $ indent + sum indentDeltas
  return $ (indentString ++) <$> decLines ++ [docLine]

cumSum :: [Int] -> [Int]
cumSum = scanl1 (+)

getIndentDelta :: Doc -> Int
getIndentDelta (Text (_, s)) =
  case s of
    "(" -> 1
    "[" -> 1
    "{" -> 1
    ")" -> -1
    "]" -> -1
    "}" -> -1
    _ -> 0
getIndentDelta _ = 0

getDecs :: Doc -> [Decoration]
getDecs (Text (decs, _)) = decs
getDecs _ = []

decToLines :: Decoration -> [String]
decToLines (DLineComment s) = ["// " ++ strip s]
decToLines (DDirective s) = [strip s]
decToLines DBlankLine = [""]
decToLines (DBlockComment s) = ("// " ++) . strip <$> lines s
decToLines (DEndComment s) = ["// " ++ strip s]
decToLines (DInlineComment s) = ["// " ++ strip s]

docToString :: Doc -> String
docToString (Text (_, s)) = s
docToString Space = " "
docToString doc = error $ "Unhandled doc type " ++ show doc

renderIndent :: Int -> String
renderIndent i = replicate (i * 4) ' '

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
    <|> pTopLevelEof
    <|> (try pTopLevelVarDec)
    <|> pFunctionDec
    <?> "toplevel declaration"

pTopLevelEof :: TokenParser ATopLevel
pTopLevelEof = do
    (decs, _) <- pEof
    return $ AEof decs

pStructDec :: TokenParser ATopLevel
pStructDec = do
    (structDecs, _) <- pStruct
    (nameDecs, name) <- pIdentifier
    (lBraceDecs, rBraceDecs, members) <- pBetweenDecs pLBrace pRBrace (many pStructDecMember)
    (semicolonDecs, _) <- pSemicolon
    return AStructDec {
        asdName=name
      , asdMembers=members
      , asdStructDecs=structDecs
      , asdNameDecs=nameDecs
      , asdLBraceDecs=lBraceDecs
      , asdRBraceDecs=rBraceDecs
      , asdSemicolonDecs=semicolonDecs
    }

pStructDecMember :: TokenParser AStructMember
pStructDecMember =
    (AStructMemberImport <$> pImportDec)
      <|> (AStructMemberVar <$> pVarDec)
      <?> "structure member"

pExportDec :: TokenParser ATopLevel
pExportDec = do
    (exportDecs, _) <- pExport
    (nameDecs, name) <- pIdentifier
    (semicolonDecs, _) <- pSemicolon
    return AExportDec {
        axdName=name
      , axdExportDecs=exportDecs
      , axdNameDecs=nameDecs
      , axdSemicolonDecs=semicolonDecs
    }

pImportDec :: TokenParser AImportDec
pImportDec = do
    (importDecs, _) <- pImport
    (try $ pImportFunctionDec importDecs)
      <|> pImportVarDec importDecs

pImportFunctionDec :: [Decoration] -> TokenParser AImportDec
pImportFunctionDec importDecs = do
    functionSig <- pFunctionSignature
    (semicolonDecs, _) <- pSemicolon
    return AImportFunctionSig {
        aifsFunctionSig=functionSig
      , aifsImportDecs=importDecs
      , aifsSemicolonDecs=semicolonDecs
    }

pImportVarDec :: [Decoration] -> TokenParser AImportDec
pImportVarDec importDecs = do
    varDec <- pVarDec
    return AImportVarDec {
        aivdVarDec=varDec
      , aivdImportDecs=importDecs
    }

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
    maybeStatic <- optionMaybe pStatic
    (returnTypeDecs, returnType) <- ((pFunction >>= \(decs, _) -> return (decs,"void")) <|> pIdentifier)
    (nameDecs, name) <- pIdentifier
    maybeMember <- optionMaybe (do
        (memberDecs, _) <- pMember
        (memberNameDecs, memberName) <- pIdentifier
        return (memberDecs, memberNameDecs, memberName))
    (lParenDecs, rParenDecs, (commaDecs, params)) <- pBetweenDecs pLParen pRParen (pSepByDecs pFunctionDecParam pComma)
    return AFunctionSignature {
        afsIsStatic=isJust maybeStatic
      , afsReturnType=returnType
      , afsName=name
      , afsMember=(\(_, _, memberName) -> memberName) <$> maybeMember
      , afsParams=params
      , afsStaticDecs=maybe [] id $ fst <$> maybeStatic
      , afsReturnTypeDecs=returnTypeDecs
      , afsMemberDecs=maybe [] id $ (\(memberDecs, _, _) -> memberDecs) <$> maybeMember
      , afsNameDecs=nameDecs
      , afsMemberNameDecs=maybe [] id $ (\(_, memberNameDecs, _) -> memberNameDecs) <$> maybeMember
      , afsLParenDecs=lParenDecs
      , afsCommaDecs=commaDecs
      , afsRParenDecs=rParenDecs
    }
    <?> "function signature"

pFunctionDecParam :: TokenParser AFunctionDecParam
pFunctionDecParam =
    pFunctionDecExtenderParam
    <|> pFunctionDecRegularParam
    <?> "function parameter"

pFunctionDecExtenderParam :: TokenParser AFunctionDecParam
pFunctionDecExtenderParam = do
    (thisDecs, _) <- pThis
    (nameDecs, name) <- pIdentifier
    (multDecs, _) <- pMult
    return AFunctionDecExtenderParam {
        afdepName=name
      , afdepThisDecs=thisDecs
      , afdepNameDecs=nameDecs
      , afdepMultDecs=multDecs
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
    (enumDecs, _) <- pEnum
    (nameDecs, name) <- pIdentifier
    (lBraceDecs, rBraceDecs, (commaDecs, values)) <- pBetweenDecs pLBrace pRBrace (pSepBy1Decs pIdentifier pComma)
    (semicolonDecs, _) <- pSemicolon
    return AEnumDec {
        aedName=name
      , aedValues=snd <$> values
      , aedEnumDecs=enumDecs
      , aedNameDecs=nameDecs
      , aedLBraceDecs=lBraceDecs
      , aedValueDecs=fst <$> values
      , aedCommaDecs=commaDecs
      , aedRBraceDecs=rBraceDecs
      , aedSemicolonDecs=semicolonDecs
    }

pTopLevelVarDec :: TokenParser ATopLevel
pTopLevelVarDec = ATopLevelVarDec <$> pVarDec

pVarDec :: TokenParser AVarDec
pVarDec = do
    typeName <- pTypeName
    (commaDecs, vars) <- pSepBy1Decs pVarInit pComma
    (semicolonDecs, _) <- pSemicolon
    return AVarDec {
        avdTypeName=typeName
      , avdVars=vars
      , avdCommaDecs=commaDecs
      , avdSemicolonDecs=semicolonDecs
    }
    <?> "variable declaration"

pVarInit :: TokenParser AVarInit
pVarInit = do
    (idDecs, ident) <- pIdentifier
    subscripts <- many (pBetweenDecs pLBracket pRBracket pInitSubscript)
    maybeVarInit <- optionMaybe (do
        (assignDecs, _) <- pAssign
        expr <- pExpr
        return (assignDecs, expr))
    return AVarInit {
        aviId=ident
      , aviSubscripts=(\(_, _, c) -> c) <$> subscripts
      , aviInit=snd <$> maybeVarInit
      , aviIdDecs=idDecs
      , aviLBracketDecs=(\(a, _, _) -> a) <$> subscripts
      , aviRBracketDecs=(\(_, b, _) -> b) <$> subscripts
      , aviAssignDecs=maybe [] id $ fst <$> maybeVarInit
    }

pInitSubscript :: TokenParser AVarSubscript
pInitSubscript = do
    pInitSubscriptId
    <|> pInitSubscriptInt
    <|> return AVarSubscriptEmpty

pInitSubscriptId :: TokenParser AVarSubscript
pInitSubscriptId = do
    (idDecs, ident) <- pIdentifier
    return AVarSubscriptId {
        avsidId=ident
      , avsidIdDecs=idDecs
    }

pInitSubscriptInt :: TokenParser AVarSubscript
pInitSubscriptInt = do
    (intDecs, int) <- pInteger
    return AVarSubscriptInt {
        avsiInt=int
      , avsiIntDecs=intDecs
    }

pBlock :: TokenParser ABlock
pBlock = do
    (lBraceDecs, rBraceDecs, commands) <- pBetweenDecs pLBrace pRBrace (many pCommand)
    return ABlock {
        abCommands=commands
      , abLBraceDecs=lBraceDecs
      , abRBraceDecs=rBraceDecs
    }
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
    (ifDecs, _) <- pIf
    (lParenDecs, rParenDecs, testExpr) <- pBetweenDecs pLParen pRParen pExpr
    trueCommand <- pCommand
    falseCommand <- optionMaybe pElseCommand
    return AIfCommand {
        aicTestExpr=testExpr
      , aicTrueCommand=trueCommand
      , aicFalseCommand=falseCommand
      , aicIfDecs=ifDecs
      , aicLParenDecs=lParenDecs
      , aicRParenDecs=rParenDecs
    }

pElseCommand :: TokenParser ACommand
pElseCommand = do
    (elseDecs, _) <- pElse
    command <- pCommand
    return AElseCommand {
        aecCommand=command
      , aecElseDecs=elseDecs
    }

pWhileCommand :: TokenParser ACommand
pWhileCommand = do
    (whileDecs, _) <- pWhile
    (lParenDecs, rParenDecs, testExpr) <- pBetweenDecs pLParen pRParen pExpr
    command <- pCommand
    return AWhileCommand {
        awcTestExpr=testExpr
      , awcCommand=command
      , awcWhileDecs=whileDecs
      , awcLParenDecs=lParenDecs
      , awcRParenDecs=rParenDecs
    }

pReturnCommand :: TokenParser ACommand
pReturnCommand = do
    (returnDecs, _) <- pReturn
    expr <- optionMaybe pExpr
    (semicolonDecs, _) <- pSemicolon
    return AReturnCommand {
        arcExpr=expr
      , arcReturnDecs=returnDecs
      , arcSemicolonDecs=semicolonDecs
    }

pVarDecCommand :: TokenParser ACommand
pVarDecCommand = AVarDecCommand <$> pVarDec

pExprCommand :: TokenParser ACommand
pExprCommand = do
    expr <- pExpr
    (semicolonDecs, _) <- pSemicolon
    return AExprCommand {
        aecExpr=expr
      , aecSemicolonDecs=semicolonDecs
    }

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

pBinOp :: TokenDecParser String -> TokenParser (AExpr -> AExpr -> AExpr)
pBinOp opParser = do
    (opDecs, opString) <- opParser
    return (\expr1 expr2 -> ABinOpExpr {
          aboeExpr1=expr1
        , aboeExpr2=expr2
        , aboeOp=opString
        , aboeOpDecs=opDecs
      })

pMaybeCastExpr :: TokenParser AExpr
pMaybeCastExpr = (try pCastExpr) <|> pMaybeUnaryExpr

pCastExpr :: TokenParser AExpr
pCastExpr = do
    (lParenDecs, rParenDecs, typeName) <- pBetweenDecs pLParen pRParen pTypeName
    castExpr <- pMaybeCastExpr
    return ACastExpr {
        aceTypeName=typeName
      , aceCastExpr=castExpr
      , aceLParenDecs=lParenDecs
      , aceRParenDecs=rParenDecs
    }

pMaybeUnaryExpr :: TokenParser AExpr
pMaybeUnaryExpr = pUnaryExpr <|> pMaybePostfixExpr

pUnaryExpr :: TokenParser AExpr
pUnaryExpr = do
    (opDecs, op) <- (pInc <|> pDec <|> pPlus <|> pMinus <|> pBinAnd <|> pMult <|> pNot)
    expr <- pMaybeUnaryExpr
    return AUnaryOpExpr {
        auoeExpr=expr
      , auoeOp=op
      , auoeOpDecs=opDecs
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
    (lBracketDecs, rBracketDecs, indexExpr) <- pBetweenDecs pLBracket pRBracket pExpr
    pMaybePostfixExpr' AIndexExpr {
        aieExpr=rootExpr
      , aieIndexExpr=indexExpr
      , aieLBracketDecs=lBracketDecs
      , aieRBracketDecs=rBracketDecs
    }

pCallExpr :: AExpr -> TokenParser AExpr
pCallExpr rootExpr = do
    (lParenDecs, rParenDecs, (commaDecs, params)) <- pBetweenDecs pLParen pRParen (pSepByDecs pExpr pComma)
    pMaybePostfixExpr' ACallExpr {
        aceFunctionExpr=rootExpr
      , aceParams=params
      , aceLParenDecs=lParenDecs
      , aceRParenDecs=rParenDecs
      , aceCommaDecs=commaDecs
    }

pMemberExpr :: AExpr -> TokenParser AExpr
pMemberExpr rootExpr = do
    (dotDecs, _) <- pDot
    (memberDecs, member) <- pIdentifier
    pMaybePostfixExpr' AMemberExpr {
        ameExpr=rootExpr
      , ameMember=member
      , ameDotDecs=dotDecs
      , ameMemberDecs=memberDecs
    }

pPostOpExpr :: AExpr -> TokenParser AExpr
pPostOpExpr rootExpr = do
    (opDecs, op) <- (pInc <|> pDec)
    pMaybePostfixExpr' APostOpExpr {
        apoeExpr=rootExpr
      , apoeOp=op
      , apoeOpDecs=opDecs
    }

pPrimaryExpr :: TokenParser AExpr
pPrimaryExpr = do
    pParenExpr
    <|> pFloatExpr
    <|> pIntExpr
    <|> pStringExpr
    <|> pIdentifierExpr
    <|> pFalseExpr
    <|> pTrueExpr
    <|> pThisExpr
    <|> pNewExpr

pParenExpr :: TokenParser AExpr
pParenExpr = do
    (lParenDecs, rParenDecs, expr) <- pBetweenDecs pLParen pRParen pExpr
    return AParenExpr {
        apeExpr=expr
      , apeLParenDecs=lParenDecs
      , apeRParenDecs=rParenDecs
    }

pFloatExpr :: TokenParser AExpr
pFloatExpr = do
    (floatDecs, f) <- pFloat
    return AFloatExpr {
        afeFloat=f
      , afeFloatDecs=floatDecs
    }

pIntExpr :: TokenParser AExpr
pIntExpr = do
    (intDecs, i) <- pInteger
    return AIntExpr {
        aieInt=i
      , aieIntDecs=intDecs
    }

pStringExpr :: TokenParser AExpr
pStringExpr = do
    (stringDecs, s) <- pString
    return AStringExpr {
        aseString=s
      , aseStringDecs=stringDecs
    }

pIdentifierExpr :: TokenParser AExpr
pIdentifierExpr = do
    (idDecs, ident) <- pIdentifier
    return AIdentifierExpr {
        aieId=ident
      , aieIdDecs=idDecs
    }

pFalseExpr :: TokenParser AExpr
pFalseExpr = do
    (falseDecs, _) <- pFalse
    return AFalseExpr {
        afeFalseDecs=falseDecs
    }

pTrueExpr :: TokenParser AExpr
pTrueExpr = do
    (trueDecs, _) <- pTrue
    return ATrueExpr {
        ateTrueDecs=trueDecs
    }

pThisExpr :: TokenParser AExpr
pThisExpr = do
    (thisDecs, _) <- pThis
    return AThisExpr {
        ateThisDecs=thisDecs
    }

pNewExpr :: TokenParser AExpr
pNewExpr = do
    (newDecs, _) <- pNew
    typeName <- pTypeName
    (lBracketDecs, rBracketDecs, sizeExpr) <- pBetweenDecs pLBracket pRBracket pExpr
    return ANewExpr {
        aneTypeName=typeName
      , aneSizeExpr=sizeExpr
      , aneNewDecs=newDecs
      , aneLBracketDecs=lBracketDecs
      , aneRBracketDecs=rBracketDecs
    }

pTypeName :: TokenParser ATypeName
pTypeName = do
    (nameDecs, name) <- pIdentifier
    maybePointer <- optionMaybe pMult
    return ATypeName {
        atnName=name
      , atnIsPointer=isJust maybePointer
      , atnNameDecs=nameDecs
      , atnMultDecs=fst <$> maybePointer
    }
    <?> "type name"

pSepByDecs :: TokenParser a -> TokenDecParser sep -> TokenParser ([[Decoration]], [a])
pSepByDecs p sep = pSepBy1Decs p sep <|> return ([], [])

pSepBy1Decs :: TokenParser a -> TokenDecParser sep -> TokenParser ([[Decoration]], [a])
pSepBy1Decs p sep = do
    x <- p
    pairs <- many (do
        (sepDecs, _) <- sep
        x2 <- p
        return (sepDecs, x2))
    return (fst <$> pairs, [x] ++ (snd <$> pairs))

pBetweenDecs :: TokenDecParser open -> TokenDecParser close -> TokenParser a -> TokenParser ([Decoration], [Decoration], a)
pBetweenDecs open close p = do
    (openDecs, _) <- open
    x <- p
    (closeDecs, _) <- close
    return (openDecs, closeDecs, x)

pInteger :: TokenDecParser Integer
pInteger =
    matchToken test
    where
      test (TInteger i) decs = Just (decs, i)
      test _ _ = Nothing

pFloat :: TokenDecParser Double
pFloat =
    matchToken test
    where
      test (TFloat i) decs = Just (decs, i)
      test _ _ = Nothing

pString :: TokenDecParser String
pString =
    matchToken test
    where
      test (TString i) decs = Just (decs, i)
      test _ _ = Nothing

pIdentifier :: TokenDecParser String
pIdentifier =
    matchToken test
    where
      test (TIdentifier s) decs = Just (decs, s)
      test _ _ = Nothing

pAssign :: TokenDecParser String
pAssign = matchToken' TAssign

pBinAnd :: TokenDecParser String
pBinAnd = matchToken' TBinAnd

pBinOr :: TokenDecParser String
pBinOr = matchToken' TBinOr

pComma :: TokenDecParser String
pComma = matchToken' TComma

pDiv :: TokenDecParser String
pDiv = matchToken' TDiv

pDivAssign :: TokenDecParser String
pDivAssign = matchToken' TDivAssign

pDec :: TokenDecParser String
pDec = matchToken' TDec

pDot :: TokenDecParser String
pDot = matchToken' TDot

pElse :: TokenDecParser String
pElse = matchToken' TElse

pEnum :: TokenDecParser String
pEnum = matchToken' TEnum

pEq :: TokenDecParser String
pEq = matchToken' TEq

pEof :: TokenDecParser String
pEof = matchToken' TEof

pExport :: TokenDecParser String
pExport = matchToken' TExport

pFalse :: TokenDecParser String
pFalse = matchToken' TFalse

pFunction :: TokenDecParser String
pFunction = matchToken' TFunction

pGt :: TokenDecParser String
pGt = matchToken' TGt

pGtEq :: TokenDecParser String
pGtEq = matchToken' TGtEq

pIf :: TokenDecParser String
pIf = matchToken' TIf

pInc :: TokenDecParser String
pInc = matchToken' TInc

pImport :: TokenDecParser String
pImport = matchToken' TImport

pLBrace :: TokenDecParser String
pLBrace = matchToken' TLBrace

pLBracket :: TokenDecParser String
pLBracket = matchToken' TLBracket

pLogAnd :: TokenDecParser String
pLogAnd = matchToken' TLogAnd

pLogOr :: TokenDecParser String
pLogOr = matchToken' TLogOr

pLParen :: TokenDecParser String
pLParen = matchToken' TLParen

pLt :: TokenDecParser String
pLt = matchToken' TLt

pLtEq :: TokenDecParser String
pLtEq = matchToken' TLtEq

pMember :: TokenDecParser String
pMember = matchToken' TMember

pMinus :: TokenDecParser String
pMinus = matchToken' TMinus

pMinusAssign :: TokenDecParser String
pMinusAssign = matchToken' TMinusAssign

pMult :: TokenDecParser String
pMult = matchToken' TMult

pMultAssign :: TokenDecParser String
pMultAssign = matchToken' TMultAssign

pNeq :: TokenDecParser String
pNeq = matchToken' TNeq

pNew :: TokenDecParser String
pNew = matchToken' TNew

pNot :: TokenDecParser String
pNot = matchToken' TNot

pPlus :: TokenDecParser String
pPlus = matchToken' TPlus

pPlusAssign :: TokenDecParser String
pPlusAssign = matchToken' TPlusAssign

pRBrace :: TokenDecParser String
pRBrace = matchToken' TRBrace

pRBracket :: TokenDecParser String
pRBracket = matchToken' TRBracket

pReturn :: TokenDecParser String
pReturn = matchToken' TReturn

pRParen :: TokenDecParser String
pRParen = matchToken' TRParen

pSemicolon :: TokenDecParser String
pSemicolon = matchToken' TSemicolon

pStatic :: TokenDecParser String
pStatic = matchToken' TStatic

pStruct :: TokenDecParser String
pStruct = matchToken' TStruct

pThis :: TokenDecParser String
pThis = matchToken' TThis

pTrue :: TokenDecParser String
pTrue = matchToken' TTrue

pWhile :: TokenDecParser String
pWhile = matchToken' TWhile

matchToken' :: Tok -> TokenDecParser String
matchToken' tok = matchToken (\t decs -> if t == tok then Just (decs, show t) else Nothing)

matchToken :: (Tok -> [Decoration] -> Maybe ([Decoration], a)) -> TokenDecParser a
matchToken test
    = token showToken posToken testToken
    where
      showToken (_, _, tok) = show tok
      posToken  (pos, _, _) = pos
      testToken (_, decs, tok) = test tok decs

-- dumpToks :: Either ParseError [Token] -> IO ()
-- dumpToks (Left err) = putStrLn $ "Scanning error: " ++ show err
-- dumpToks (Right toks) = mapM_ dumpTok toks
--
-- dumpTok :: Token -> IO ()
-- dumpTok (pos, _decs, tok) = putStrLn $ show (sourceLine pos) ++ " " ++ show tok

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
       <|> (lookAhead eol >> return [(pos, PTDecoration DBlankLine)])
       <|> ptLineContent)

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
    _ <- ptLineCommentStart
    s <- many (noneOf "\n\r")
    return (pos, PTDecoration (decorator s))

ptBlockComment :: Parser PreToken
ptBlockComment = do
    pos <- getPosition
    _ <- ptBlockCommentStart
    s <- manyTill anyChar (lookAhead ptBlockCommentEnd)
    _ <- ptBlockCommentEnd
    return (pos, PTDecoration $ ctor s)
        where ctor s = if elem '\n' s || elem '\r' s
                       then DBlockComment s
                       else DInlineComment s

ptNonComment :: Parser PreToken
ptNonComment = do
    pos <- getPosition
    s <- manyTill anyChar toNonCommentEnd
    guard (not $ null s)
    return (pos, PTContent s)

toNonCommentEnd :: Parser String
toNonCommentEnd =
    lookAhead (
         (string "\r")
         <|> (string "\n")
         <|> try (string "/" >> (string "*" <|> string "/"))
         <|> (eof >> return "")
    )

ptLineCommentStart :: Parser String
ptLineCommentStart = string "//"

ptBlockCommentStart :: Parser String
ptBlockCommentStart = string "/*"

ptBlockCommentEnd :: Parser String
ptBlockCommentEnd = string "*/"

eol :: Parser String
eol = (string "\r" >> ((string "\n" >> return "\r\n") <|> return "\r"))
      <|> string "\n"
      <?> "end of line"

strip :: String -> String
strip = T.unpack . T.strip . T.pack
