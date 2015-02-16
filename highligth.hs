import System.IO
import Text.Regex.Posix
type Pattern = String
type Color = String
{- Highlighter in Haskell -}
main :: IO()
main = do 
        putStrLn "Reading config..."
        keywords <- readFile "keywords.txt"
        let pattern = pattern' ++ [keywords]
            patCol = zip pattern color
        putStrLn "Processing..."
        contents <- readFile "test.java"
        writeFile "test.java.html" $ htmlen contents patCol
        putStrLn "Finished."

htmlHead = "<!DOCTYPE html><head><meta charset=\"utf-8\"></head>"
htmlStyle = "<style>code{font: 14px/200% \"Consolas\";}</style><body><pre><code>"
htmlLast= "</code></pre></body></html>"
htmlen:: String -> [(Pattern, Color)] -> String
htmlen str patCol = let htmlBody = foldl (\acc x-> parser acc x) str patCol
                        in htmlHead ++ htmlStyle ++ htmlBody ++ htmlLast

parser :: String -> (Pattern, Color) -> String
parser code patCol@(pattern, color) 
    | code =~ pattern == True 
            = (\(_head, x, _tail) -> _head ++ ls ++ x ++ le ++ parser _tail patCol) (code =~ pattern)
    | otherwise = code 
    where ls = "<font color=" ++ color ++ ">"
          le = "</font>"

pattern' = [ "/\\*(.|\n|\r)*\\*/"
           , "//.*"
           , "#.*"
           , "\"([^\"]|\\\")*\""]
color = ["Purple", "Gray", "Red", "Green", "Blue"]
