module Main(
    main
    ) where

import Lib (p,c)
import System.IO
import Data.Typeable


main :: IO ()
main = do
            content <- readFile "./p" 
            putStr (content)

