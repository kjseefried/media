module Main where

import qualified Data.ByteString as B
import System.Environment
import PNG

main :: IO ()
main = do
    (pngFileName:_) <- getArgs
    contents <- B.readFile pngFileName
    if verifyPNG contents then putStrLn $ "PNG file " ++ pngFileName ++ " verifies correctly!"
                          else putStrLn $ "PNG file " ++ pngFileName ++ " INVALID!"
