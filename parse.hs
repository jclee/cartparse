import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import Text.ParserCombinators.Parsec

main :: IO ()
main = do
    cartDir <- getEnv "CARTLIFE"
    files <- getFiles $ cartDir
    print $ map cartParse $ take 1 $ sort files
    putStrLn "Hello, World!"

getFiles :: FilePath -> IO [FilePath]
getFiles p = filterM doesFileExist =<< getRelDirectoryContents p

getRelDirectoryContents :: FilePath -> IO [FilePath]
getRelDirectoryContents p = map (p </>) `liftM` getDirectoryContents p

-- TODO: Implement me!
--cartParse :: String -> CartAst
cartParse = id
