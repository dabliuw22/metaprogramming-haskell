{-# LANGUAGE TemplateHaskell #-}

module Adapter.Meta.Base.TH where

import Control.Monad
import Language.Haskell.TH

-- plusOne x = x + 1
plusOne :: Q Exp
-- plusOne = [| (+1) |]
plusOne = do
  x <- newName "x"
  return $
    LamE -- \
      [VarP x] -- x ->
      (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1)))) -- x + 1

-- $plusOne 10 -> 11

hello :: Q Exp
-- hello = [| \name -> $(varE (mkName "hello'")) ++ name |]
hello = do
  let hello' = mkName "hello'"
  name' <- newName "name"
  --return $ LamE [VarP name] (InfixE (Just (LitE (StringL "Hello "))) (VarE '(++)) (Just (VarE name)))
  return $ LamE [VarP name'] (InfixE (Just (VarE hello')) (VarE '(++)) (Just (VarE name')))

-- let hello' = "Hello "

-- $hello "Jhon" -> "Hello Jhon"

data Data = Data String String deriving Show

make :: String -> Name -> Q Exp
make txt name = do
  info <- reify name
  return $ InfixE (Just (LitE (StringL (show info)))) (VarE '(++)) (Just (LitE (StringL txt)))