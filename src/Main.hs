import TAPL.Core
import TAPL.Parser
import System.Environment

main = do
  args <- getArgs 
  content <- readFile $ head args
  let result = case parseLang content of
                   Left _ -> error "Error parsing the file"
                   Right a -> map eval a
  mapM_ (putStrLn.show) result
