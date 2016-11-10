{-# LANGUAGE OverloadedStrings #-}

module Prim where

import LispVal

import Data.Text as T
import Data.Monoid
import Control.Monad.Except
import Control.Monad.Reader
import System.Directory
import System.IO
import System.Environment

type Prim = [(T.Text, LispVal)]
type Unary = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

primEnv :: Prim
primEnv = [   ("+"    , Fun $ IFunc $ binopFold (numOp    (+))  (Number 0) )
            , ("*"    , Fun $ IFunc $ binopFold (numOp    (*))  (Number 1) )
            , ("++"   , Fun $ IFunc $ binopFold (strOp    (<>)) (String ""))
            , ("-"    , Fun $ IFunc $ binop $    numOp    (-))
            , ("<"    , Fun $ IFunc $ binop $    numCmp   (<))
            , ("<="   , Fun $ IFunc $ binop $    numCmp   (<=))
            , (">"    , Fun $ IFunc $ binop $    numCmp   (>))
            , (">="   , Fun $ IFunc $ binop $    numCmp   (>=))
            , ("=="   , Fun $ IFunc $ binop $    numCmp   (==))
            , ("even?", Fun $ IFunc $ unop $     numBool   even)
            , ("odd?" , Fun $ IFunc $ unop $     numBool   odd)
            , ("pos?" , Fun $ IFunc $ unop $     numBool ((<) 0))
            , ("neg?" , Fun $ IFunc $ unop $     numBool ((>) 0))
            , ("eq?"  , Fun $ IFunc $ binop     (eqOp     (==)))
            , ("and"  , Fun $ IFunc $ binopFold (eqOp     (&&)) (Bool True))
            , ("or"   , Fun $ IFunc $ binopFold (eqOp     (||)) (Bool False))
            , ("cons" , Fun $ IFunc $ Prim.cons)
            , ("cdr"  , Fun $ IFunc $ Prim.cdr)
            , ("car"  , Fun $ IFunc $ Prim.car)
            , ("quote", Fun $ IFunc $ quote)
            , ("file?" , Fun $ IFunc $ unop $ fileExists)
            , ("slurp" , Fun $ IFunc $ unop $ slurp)
            ]

unop :: Unary -> [LispVal] -> Eval LispVal 
unop op args@[x] = op x
unop op args     = throwError $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op args@[x,y] = op x y
binop op args          = throwError $ NumArgs 2 args

fileExists :: LispVal -> Eval LispVal 
fileExists x@(Atom lbl) = fileExists (String lbl)
fileExists (String txt) = Bool <$> (liftIO $ doesFileExist $ T.unpack txt)
fileExists val = throwError $ TypeMismatch "read expects string, instead got: " val

slurp :: LispVal -> Eval LispVal 
slurp (String txt) = readTextFile txt
slurp val =  throwError $ TypeMismatch "read expects string, instead got: " val
-- (slurp "test/let.scheme")
-- get this to work with "fileToEvalForm"
readTextFile ::  T.Text -> Eval LispVal
readTextFile script =  do  
  inh <- liftIO $ openFile (T.unpack script) ReadMode
  ineof <- liftIO $ hIsEOF inh 
  if ineof
    -- should throw an IOError (3 error types: parse, eval, io)
    then  (liftIO $ putStr "empty file\n") >> (throwError $ Default "empty file")
      else do fileText <- liftIO $ hGetContents $ inh 
              --liftIO $ hClose inh 
              liftIO $ putStr "FileContents:\n"
              return $ String $ T.pack fileText


binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
                            [a,b] -> op a b
                            (a:as) -> foldM op farg args
                            []-> throwError $ NumArgs 2 args

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal 
numBool op (Number x) = return $ Bool $ op x
numBool op  x         = throwError $ TypeMismatch "numeric op " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp op x          (Number y) = throwError $ TypeMismatch "numeric op " x
numOp op (Number x)  y         = throwError $ TypeMismatch "numeric op " y
numOp op x           y         = throwError $ TypeMismatch "numeric op " (String $ T.pack $ show x ++ show y)
strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp op x          (String y) = throwError $ TypeMismatch "string op " x 
strOp op (String x)  y         = throwError $ TypeMismatch "string op " y
strOp op x           y         = throwError $ TypeMismatch "string op " (String $ T.pack $ show x ++ show y)
-- (==) (||) (&&)
eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y 
eqOp op  x       (Bool y) = throwError $ TypeMismatch "bool op " x
eqOp op (Bool x)  y       = throwError $ TypeMismatch "bool op " y
eqOp op x         y       = throwError $ TypeMismatch "bool op " (String $ T.pack $ show x ++ show y)
-- (<) (>) (>=) (<=)
numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return $ Bool $ op x  y
numCmp op x          (Number y) = throwError $ TypeMismatch "numeric op " x
numCmp op (Number x)  y         = throwError $ TypeMismatch "numeric op " y
numCmp op x         y           = throwError $ TypeMismatch "numeric op " (String $ T.pack $ show x ++ show y)

-- better wa to check args?
cons :: [LispVal] -> Eval LispVal
cons [x,y@(List yList)] = return $ List $ x:yList
cons [c]                = return $ List [c] 
cons []                 = return $ List []
cons _  = throwError $ Default "cons takes two arguments: an S-Expression and a list"

car :: [LispVal] -> Eval LispVal
car [List []    ] = return $ Nil
car [List (x:_)]  = return $ x
car []            = return $ Nil
car x             = throwError $ Default "car expects a list"

cdr :: [LispVal] -> Eval LispVal
cdr [List (x:xs)] = return $ List xs
cdr [(List [])]  = return $ Nil
cdr []           = return $ Nil
cdr x             = throwError $ Default "cdr expects a list" 
--
--
quote :: [LispVal] -> Eval LispVal 
quote [List xs]  = return $ List ((Atom "quote"):xs)
quote [exp]      = return $ List [Atom "quote",exp]

-- default return to Eval monad (no error handling)
binopFixPoint :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> Eval LispVal
binopFixPoint f2 = binop $ (\x y -> return $ f2 x y)

numOpVal :: (Integer -> Integer -> Integer ) -> LispVal -> LispVal -> LispVal
numOpVal op (Number x) (Number y) = Number $ op x  y
