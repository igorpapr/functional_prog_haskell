putN :: Int -> String -> IO()
putN n str = if (n <=1) then putStrLn str 
                        else do putStrLn str
                                putN (n-1) str
                                                      
                      
main :: IO()
main = putN 5 "abc"
                      
firstN :: IO()
firstN = do putStr "file> "
            name <- getLine
            putStrLn "N> " 
            ns <- getLine
            file <- readFile name
            putStrLn (takeN file ns)
            
takeN :: String -> String -> String 
takeN file ns = unlines (take (read ns) (lines file))

lastN :: IO()
lastN = do putStr "file> "
           name <- getLine
           putStrLn "N> " 
           ns <- getLine
           file <- readFile name
           putStrLn (takeLN file ns)
            
takeLN :: String -> String -> String 
takeLN file ns = (unlines . reverse . ((take.read) ns). reverse.lines) file