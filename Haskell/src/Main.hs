module Main where

import qualified Data.ByteString as B
import System.Environment
import PNG

main :: IO ()
main = do
    (pngFileName:_) <- getArgs
    contents <- B.readFile pngFileName
    putStrLn $ "Read the PNG file " ++ pngFileName
