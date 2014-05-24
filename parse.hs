import System.Directory
import System.Environment

main :: IO ()
main = do
    cartDir <- getEnv "CARTLIFE"
    contents <- getDirectoryContents cartDir
    --files <- filter doesFileExist contents
    putStrLn "Hello, World!"
