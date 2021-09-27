{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib

main :: IO ()
main = do
    let x = runMk
    print x

runMk = $(make "Txt" ''Data)
