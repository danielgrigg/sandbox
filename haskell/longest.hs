import System.Environment
import System.IO
import Data.List

longest_line doc = maximum $ map length $ lines doc
longest_line_str = show . longest_line
main = do
  args <- getArgs
  progName <- getProgName
  withFile (head args) ReadMode (\handle -> do
    contents <- hGetContents handle
    putStr $ show (longest_line contents) ++ "\n")
