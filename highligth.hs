import qualified Data.Map as Map
import qualified Control.Monad as Monad
{- Highlighter in Haskell -}
main :: IO()
main = do 
        contents <- readFile "keywords.txt"
        let kw = consKeyWord contents
        print kw
        Monad.forever $ do 
                x <- getLine
                print $ Map.lookup x kw

pWords :: String -> [String]
pWords = words

consKeyWord :: String -> Map.Map String String
consKeyWord = Map.fromList . map (\key -> (key, "<strong>")) . lines
