import System.IO
import Text.Regex.Posix
import qualified Data.Map as Map
import qualified Control.Monad as Monad
type Pattern = String
type Color = String

{- Highlighter in Haskell -}
main :: IO()
main = do 
        putStrLn "Reading config..."
        handle <- openFile "keywords.txt" ReadMode
        pattern <- sequence $ replicate 6 (hGetLine handle)
        color <- sequence $ replicate 6 (hGetLine handle)
        let patCol = zip pattern color
        mapM_ print pattern
        hClose handle

        putStrLn "Processing..."
        contents <- readFile "test.java"
        writeFile "test.java.html" $ htmlen contents patCol
        putStrLn "Finished."

htmlHead = "<!DOCTYPE html><head><meta charset=\"utf-8\"></head>"
htmlStyle = "<style>code{font: 14px/200% \"Consolas\";}</style><body><pre><code>"
htmlLast= "</code></pre></body></html>"
htmlen:: String -> [(Pattern, Color)] -> String
htmlen str patCol = let htmlBody = foldl (\acc patcol -> parser acc patcol) str patCol
                        in htmlHead ++ htmlStyle ++ htmlBody ++ htmlLast

parser :: String -> (Pattern, Color) -> String
parser code patCol@(pattern, color) 
    | code =~ pattern == True 
            = (\(_head, str, _tail) -> _head ++ "<font color=" ++ color ++ ">" ++ str ++ "</font>"++ parser _tail patCol) (code =~ pattern)
    | otherwise = code 
