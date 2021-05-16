{-# LANGUAGE OverloadedStrings #-}

module Prim ( primEnv, unop )  where

import LispVal
    ( LispException(NumArgs, IOError, TypeMismatch, ExpectedList),
      IFunc(IFunc),
      LispVal(Atom, Fun, Number, String, Bool, Nil, List),
      Eval )

import Data.Functor ((<&>))
import Data.Text as T ( Text, concat, pack, unpack )
import Data.Text.IO as TIO ( hGetContents, hPutStr )
import System.Directory ( doesFileExist )
import System.IO
    ( Handle, hIsWritable, withFile, IOMode(WriteMode, ReadMode) )
import Network.HTTP ( getRequest, getResponseBody, simpleHTTP )

import Control.Monad.Except ( foldM, MonadIO(liftIO) )
import Control.Exception ( throw )

type Prim   = [(T.Text, LispVal)]
type Unary  = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Fun . IFunc

primEnv :: Prim
primEnv = [
    ("+"     , mkF $ binopFold (numOp    (+))  (Number 0) )
  , ("*"     , mkF $ binopFold (numOp    (*))  (Number 1) )
  , ("string-append", mkF $ binopFold (strOp    (<>)) (String "") )
  , ("-"     , mkF $ binop $    numOp    (-))
  , ("<"     , mkF $ binop $    numCmp   (<))
  , ("<="    , mkF $ binop $    numCmp   (<=))
  , (">"     , mkF $ binop $    numCmp   (>))
  , (">="    , mkF $ binop $    numCmp   (>=))
  , ("=="    , mkF $ binop $    numCmp   (==))
  , ("even?" , mkF $ unop $     numBool   even)
  , ("odd?"  , mkF $ unop $     numBool   odd)
  , ("neg?"  , mkF $ unop $     numBool (< 0))
  , ("pos?"  , mkF $ unop $     numBool (> 0))
  , ("eq?"   , mkF $ binop eqCmd )
  , ("null?" , mkF $ unop (eqCmd Nil) )
  , ("bl-eq?", mkF $ binop $ eqOp (==))
  , ("and"   , mkF $ binopFold (eqOp (&&)) (Bool True))
  , ("or"    , mkF $ binopFold (eqOp (||)) (Bool False))
  , ("not"   , mkF $ unop notOp)
  , ("cons"  , mkF Prim.cons)
  , ("cdr"   , mkF Prim.cdr)
  , ("car"   , mkF Prim.car)
  , ("quote" , mkF quote)
  , ("file?" , mkF $ unop fileExists)
  , ("slurp" , mkF $ unop slurp)
  , ("wslurp", mkF $ unop wSlurp)
  , ("put"   , mkF $ binop put)
  ]

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x]    = op x
unop _ args    = throw $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x,y]  = op x y
binop _  args   = throw $ NumArgs 2 args

fileExists :: LispVal  -> Eval LispVal
fileExists (String txt) = Bool <$> liftIO (doesFileExist $ T.unpack txt)
fileExists val          = throw $ TypeMismatch "read expects string, instead got: " val

slurp :: LispVal  -> Eval LispVal
slurp (String txt) = liftIO $ wFileSlurp txt
slurp val          =  throw $ TypeMismatch "read expects string, instead got: " val

wFileSlurp :: T.Text -> IO LispVal
wFileSlurp fileName = withFile (T.unpack fileName) ReadMode go
  where go = readTextFile fileName

openURL :: T.Text -> IO LispVal
openURL x = do
  req  <- simpleHTTP (getRequest $ T.unpack x)
  body <- getResponseBody req
  return $ String $ T.pack body

wSlurp :: LispVal -> Eval LispVal
wSlurp (String txt) =  liftIO  $  openURL txt
wSlurp val = throw $ TypeMismatch "wSlurp expects a string, instead got: " val

readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile fileName handle = do
  exists <- doesFileExist $ T.unpack fileName
  if exists
  then TIO.hGetContents handle <&> String
  else throw $ IOError $ T.concat [" file does not exits: ", fileName]

put :: LispVal -> LispVal -> Eval LispVal
put (String file) (String msg) =  liftIO $ wFilePut file msg
put (String _)  val = throw $ TypeMismatch "put expects string in the second argument (try using show), instead got: " val
put val  _ = throw $ TypeMismatch "put expects string, instead got: " val

wFilePut :: T.Text -> T.Text -> IO LispVal
wFilePut fileName msg = withFile (T.unpack fileName) WriteMode go
  where go = putTextFile fileName msg

putTextFile :: T.Text -> T.Text -> Handle -> IO LispVal
putTextFile fileName msg handle = do
  canWrite <- hIsWritable handle
  if canWrite
  then TIO.hPutStr handle msg >> return (String msg)
  else throw $ IOError $ T.concat [" file does not exits: ", fileName]

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
                            [a,b]  -> op a b
                            (_a:_as) -> foldM op farg args
                            []-> throw $ NumArgs 2 args

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool _   x         = throw $ TypeMismatch "numeric op " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp _  Nil        (Number y) = return $ Number y
numOp _  (Number x) Nil        = return $ Number x
numOp _  x          (Number _) = throw $ TypeMismatch "numeric op " x
numOp _  (Number _)  y         = throw $ TypeMismatch "numeric op " y
numOp _  x           _         = throw $ TypeMismatch "numeric op " x

strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp _  Nil        (String y) = return $ String y
strOp _  (String x) Nil        = return $ String x
strOp _  x          (String _) = throw $ TypeMismatch "string op " x
strOp _  (String _)  y         = throw $ TypeMismatch "string op " y
strOp _  x           _         = throw $ TypeMismatch "string op " x

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp _   x       (Bool _) = throw $ TypeMismatch "bool op " x
eqOp _  (Bool _)  y       = throw $ TypeMismatch "bool op " y
eqOp _  x         _       = throw $ TypeMismatch "bool op " x

numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return . Bool $ op x  y
numCmp _  x          (Number _) = throw $ TypeMismatch "numeric op " x
numCmp _  (Number _)  y         = throw $ TypeMismatch "numeric op " y
numCmp _  x         _           = throw $ TypeMismatch "numeric op " x


notOp :: LispVal -> Eval LispVal
notOp (Bool True) = return $ Bool False
notOp (Bool False) = return $ Bool True
notOp x = throw $ TypeMismatch " not expects Bool" x

eqCmd :: LispVal -> LispVal -> Eval LispVal
eqCmd (Atom   x) (Atom   y) = return . Bool $ x == y
eqCmd (Number x) (Number y) = return . Bool $ x == y
eqCmd (String x) (String y) = return . Bool $ x == y
eqCmd (Bool   x) (Bool   y) = return . Bool $ x == y
eqCmd  Nil        Nil       = return $ Bool True
eqCmd  _          _         = return $ Bool False

cons :: [LispVal] -> Eval LispVal
cons [x,List yList] = return $ List $ x:yList
cons [x,y]          = return $ List [x,y]
cons _  = throw $ ExpectedList "cons, in second argumnet"


car :: [LispVal] -> Eval LispVal
car [List []    ] = return Nil
car [List (x:_)]  = return x
car []            = return Nil
car _             = throw $ ExpectedList "car"

cdr :: [LispVal] -> Eval LispVal
cdr [List (_:xs)] = return $ List xs
cdr [List []]     = return Nil
cdr []            = return Nil
cdr _             = throw $ ExpectedList "cdr"

quote :: [LispVal] -> Eval LispVal
quote [List xs]   = return $ List $ Atom "quote" : xs
quote [expr]      = return $ List $ Atom "quote" : [expr]
quote args        = throw $ NumArgs 1 args
