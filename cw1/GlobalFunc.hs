
module GlobalFunc where
    
import System.IO (stdout, hFlush)

-- simple prompt function
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine