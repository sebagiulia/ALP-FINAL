{-# LANGUAGE OverloadedStrings #-}


module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
           -- inferidor de tipos
  ,
    inferConn
  ,
    evalConn
  ,
    inferFile
  ,
    evalFile
  )
where
import Data.Csv hiding (lookup)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V 
import Network.Socket (PortNumber)
import qualified Data.Word as W
import qualified Data.ByteString.Char8 as B
import Control.Exception (try, SomeException, Exception, toException)
import           Data.List
import Data.Int (Int32)
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common
import TableOperators
import  Data.Text (Text, pack, unpack)
import Database.MySQL.Protocol.MySQLValue (MySQLValue(MySQLText, MySQLInt32U))
import Database.MySQL.Base hiding (render)
import Data.Char (isUpper)


-- Separe between TableName and ColumnName
-- Example: "Movies.name" -> C "Movies" "name" 
-- Example: "name" -> C "" "name" 
separeAtDot :: String -> Column
separeAtDot c = let (beforeDot, rest) = span (/= '.') c
                in if null rest then ("", beforeDot)
                                else (beforeDot, drop 1 rest)
columns :: TableCols -> [Column]
columns [] = []
columns ((LVar v):cs) = let cs' = columns cs
                            c' = separeAtDot v
                        in (c':cs')


value :: TableAtom -> Value
value (LVar c) = Col (separeAtDot c)
value (LNum s) = Val (MySQLInt32 (read s))
value (LString s) = Val (MySQLText (pack s))

condition' :: TableCond -> Condition
condition' (LAnd a b) = And (condition' a) (condition' b)
condition' (LOr a b) = Or (condition' a) (condition' b)
condition' (LGr a b) = Gr (value a) (value b)
condition' (LLr a b) = Lr (value a) (value b)
condition' (LGrEq a b) = Greq (value a) (value b)
condition' (LLrEq a b) = Lreq (value a) (value b)
condition' (LEquals a b) = Eq (value a) (value b)


-- conversion a términos localmente sin nombres
conversion :: TableTerm -> Term
conversion = conversion' []

conversion' :: [String] -> TableTerm -> Term
conversion' b (LTableVar n) =  if isUpper (head n) then GlobalTableVar n else LocalTableVar n 
conversion' b (LProy cols t) = Proy (columns cols) (conversion' b t)
conversion' b (LSel cond t) = Sel (condition' cond) (conversion' b t)
conversion' b (LPNat t1 t2) =  PNat (conversion' b t1) (conversion' b t2)
conversion' b (LRen tn t) =  Ren tn (conversion' b t)
conversion' b (LPCart t1 t2) = PCart (conversion' b t1) (conversion' b t2)
conversion' b (LDiv t1 t2) = Div (conversion' b t1) (conversion' b t2)
conversion' b (LDiff t1 t2) = Diff (conversion' b t1) (conversion' b t2)
conversion' b (LUni t1 t2) = Uni (conversion' b t1) (conversion' b t2)
conversion' b (LInt t1 t2) = Int (conversion' b t1) (conversion' b t2)


-- evaluador de términos
eval :: NameEnv Table TableType -> NameEnv Table TableType -> Term -> Table
eval e l (GlobalTableVar v) = fst $ fromJust $ lookup v e
eval e l (LocalTableVar i) =  fst $ fromJust $ lookup i l
eval e l (Sel cond t) = sel (eval e l t) cond
eval e l (Proy cs t) = proy cs (eval e l t)
eval e l (Ren n t) = ren (eval e l t) n
eval e l (PCart t1 t2) = pcart (eval e l t1) (eval e l t2)
eval e l (PNat t1 t2) = pnat (eval e l t1) (eval e l t2)
eval e l (Div t1 t2) = divtables (eval e l t1) (eval e l t2)
eval e l (Diff t1 t2) = difftables (eval e l t1) (eval e l t2)
eval e l (Uni t1 t2) = uni (eval e l t1) (eval e l t2)
eval e l (Int t1 t2) = int (eval e l t1) (eval e l t2)

evalConn :: ConnectInfo -> NameEnv Table TableType -> IO (Either SomeException (NameEnv Table TableType))
evalConn cinfo e = try (conn' cinfo)
  where conn' inf = do conn <- connect inf
                       (_, is) <- query_ conn "show tables"
                       rows <- traduce is
                       tables <- getTables conn rows -- [ Table ]
                       let st = convertToEnv e tables
                       return st

data Except = Except String deriving (Show)
instance Exception Except

evalFile :: String -> String -> NameEnv Table TableType -> IO (Either SomeException (NameEnv Table TableType))
evalFile file name st = do
    csvData <- BL.readFile file
    case decode NoHeader csvData of
      Right rows -> if V.null rows ||(V.null (V.tail rows))
                    then return $ Left $ toException $ Except "Tabla vacía.\n"
                    else let (cols, rows', typ) = processRows (V.toList rows)
                         in case rows' of
                              Right rs -> let cols' = map (\c -> (name,c)) cols
                                          in return $ Right [(name, ((rs, name, cols'), (name, zip cols' typ)))]
                              Left err -> return $ Left $ toException $ Except err
      Left err -> return $ Left $ toException $ Except err

-- Convierte una cadena en Int32, si es posible
convertToRowValues :: [Type] -> [String] -> Either String Row
convertToRowValues t r = foldr convertToMySqlValue (Right [])  $ zip t r

convertToMySqlValue :: (Type, String) ->  Either String Row  -> Either String Row
convertToMySqlValue _ (Left e) = Left e
convertToMySqlValue (t, s) (Right r) =
    case reads (unpack (pack s)) of
        [(val, "")]     -> case t of
                             StrT -> Left "Tabla inconsistente.\n"
                             _ -> Right $ (MySQLInt32 val):r
        _               -> case t of
                            IntT -> Left "Tabla inconsistente.\n"
                            _ -> Right $ (MySQLText (pack s)):r

-- Procesa filas y convierte los valores numéricos
processRows :: [[String]] -> ([String], Either String [Row], [Type])
processRows [] = ([], Right [], [])
processRows (header:rest) = let typ = getType (head rest) 
                            in (header, (foldl (processRow typ) (Right []) rest), typ)
  where
    processRow :: [Type] -> Either String [Row] -> [String] -> Either String [Row]
    processRow t rs r = case rs of
                          Right rows -> case convertToRowValues t r of
                                          Right nrow -> Right (nrow:rows)
                                          _ -> Left "Tabla inconsistente.\n"
                          err -> err
    getType row = map toType row 
    toType s = case reads (unpack (pack s)) :: [(Int32, String)]of
                [(val, "")] -> IntT
                _           -> StrT

inferConn :: ConnWords -> Either String ConnectInfo
inferConn = inferConn' defaultConnectInfo

inferConn' :: ConnectInfo -> ConnWords -> Either String ConnectInfo
inferConn' c (w:ws) = case w of
                        LHost (LString s) -> inferConn' (c {ciHost = s}) ws
                        LPort (LNum n) -> inferConn' (c {ciPort = fromIntegral (read n :: W.Word16) }) ws
                        LDb (LString s) -> inferConn' (c {ciDatabase = (B.pack s)}) ws
                        LUser (LString s) -> inferConn' (c {ciUser = (B.pack s)}) ws
                        LPw (LString s) -> inferConn' (c {ciPassword = (B.pack s)}) ws
                        _ -> Left "Parametro de conexion desconocido\n"
inferConn' c [] = Right c

inferFile :: String -> String -> Either String String
inferFile file name = let (nf, ext) = separeAtDot file
                      in if nf == "" || ext /= "csv" then Left "No se trata de un archivo csv.\n"
                         else Right file
                  
convertToEnv ::  NameEnv Table TableType -> [(Table, [ColumnDef])] -> NameEnv Table TableType
convertToEnv e [] = e
convertToEnv e ((t@(rows, name, cols), cts):ts) = case lookup name e of
                                        Just _ -> convertToEnv e ts
                                        Nothing -> let typ = (name, getType cts cols)
                                                   in ((name, (t, typ)):convertToEnv e ts)
                                        where getType [] _ = []
                                              getType (c:cts) (col:cols) = case columnType c of
                                                                              t -> if t == mySQLTypeLong then (col, IntT):getType cts cols
                                                                                   else (col, StrT):getType cts cols


-- type checker
infer :: NameEnv Table TableType -> NameEnv Table TableType -> Term -> Either String TableType
infer = infer' []
-- definiciones auxiliares
ret :: TableType -> Either String TableType
ret = Right

err :: String -> Either String TableType
err = Left

(>>=)
  :: Either String TableType -> (TableType -> Either String TableType) -> Either String TableType
(>>=) v f = either Left f v
-- fcs. de error

matchError :: TableType -> TableType -> Either String TableType
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

nameError :: TableName -> Either String TableType
nameError n =
  err $  "Se esperaban nombres de tablas distintos: " ++ n

coltypeError :: (Column, Type) -> (Column, Type) -> Either String TableType
coltypeError (c1,t1) (c2,t2) =
  err $  "Columnas con mismo nombre pero distinto tipo: " ++ (snd c1) ++ "->" ++ show t1 ++ " y " ++ show t2

tablesizeError :: TableName -> TableName -> Either String TableType
tablesizeError n1 n2 =
  err $  "Tablas con numero invalidos de columnas: " ++ n1 ++ " y " ++ n2

colnameError :: TableName -> TableName -> Either String TableType
colnameError n1 n2 =
  err $  "Tablas con diferentes columnas: " ++ n1 ++ " y " ++ n2

notfunError :: TableType -> Either String TableType
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notfoundError :: TableName -> Either String TableType
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Table TableType -> NameEnv Table TableType -> Term -> Either String TableType
infer' _ _ l (LocalTableVar n) = case lookup n l of
                                   Just (_, t) -> ret t
                                   _           -> notfoundError n
infer' _ e l (GlobalTableVar n) = case lookup n e of
                                  Just (_, t) -> ret t
                                  _           -> notfoundError n
infer' c e l (Sel cond t) = infer' c e l t
infer' c e l (Proy cs t) = case infer' c e l t of
                          Right t -> ret (proyInfer cs t)
                          err     -> err
infer' c e l (Ren n t) = case infer' c e l t of
                       Right t -> ret (n, snd t)
infer' c e l (PCart t1 t2) = case infer' c e l t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) ->  if n1 == n2
                                                                       then nameError n1
                                                                       else ret (n1 ++ "*"++ n2, t1cs ++ t2cs)
infer' c e l (PNat t1 t2) = case infer' c e l (PCart t1 t2) of
                            Left e -> Left e
                            _ -> case infer' c e l t1 of
                                   Left e  -> err e
                                   Right (n1, t1cs) -> case infer' c e l t2 of
                                                          Left e  -> err e
                                                          Right (n2, t2cs) -> case matchCols (n1, t1cs) (n2, t2cs) of
                                                                                  Right t -> ret t
                                                                                  err -> err
infer' c e l (Uni t1 t2) = case infer' c e l t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareCols (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ " U " ++ n2, t)
                                                                       err -> err
infer' c e l (Int t1 t2) = case infer' c e l t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareCols (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ " I " ++ n2, t)
                                                                       err -> err
infer' c e l (Diff t1 t2) = case infer' c e l t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareCols (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ " - " ++ n2, t)
                                                                       err -> err
infer' c e l (Div t1 t2) = case infer' c e l t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareColsDiv (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ " / " ++ n2, t)
                                                                       err -> err

proyInfer :: [Column] -> TableType -> TableType
proyInfer [] (n,_) = (n, [])
proyInfer _ (n, []) = (n, [])
proyInfer (c:cs) (n, ts) = case lookup (snd c) (map (\((x,y),z) -> (y,z)) ts) of
                        Nothing -> proyInfer cs (n, ts)
                        Just t -> let (_, ts') = proyInfer cs (n, ts)
                                  in (n, (c,t):ts')

matchCols :: TableType -> TableType -> Either String TableType
matchCols (n1, []) (n2, ts) = Right (n1 ++ "|x|" ++ n2, ts)
matchCols (n1, t:ts) (n2, ts') = case filterCols t ts' of
                                      Right ts'' -> case matchCols (n1, ts) (n2, ts'') of
                                                    Right (_, ts''') -> Right (n1 ++ "|x|" ++ n2, t:ts''')
                                                    err -> err
                                      Left n     -> coltypeError n n
                                where filterCols _ [] = Right []
                                      filterCols tcname (tc:tcs) = if fst tcname == fst tc
                                                                   then if snd tcname == snd tc
                                                                        then filterCols tcname tcs
                                                                        else Left tcname
                                                                   else case filterCols tcname tcs of
                                                                          Right tcs' -> Right (tc:tcs')
                                                                          err -> err

compareCols :: TableType -> TableType -> Either String TableType
compareCols (n1, []) _ = Right (n1, [])
compareCols _ (n2, []) = Right (n2, [])
compareCols (n1, ts1) (n2, ts2) = if length ts1 /= length ts2
                                  then tablesizeError n1 n2
                                  else case compareCols' (n1, ts1) (n1, ts2) of
                                          Right _ -> Right (n1, ts1)
                                          err -> err
                                where compareCols' (n1, []) (n2, _) = Right (n1,[])
                                      compareCols' (n1, (t:ts)) (n2, (t':ts')) = if fst t == fst t'
                                                                                 then if snd t == snd t'
                                                                                      then compareCols' (n1, ts) (n2, ts')
                                                                                      else coltypeError t t'
                                                                                 else colnameError n1 n2

compareColsDiv :: TableType -> TableType -> Either String TableType
compareColsDiv (n1, ts) (n2, []) = Right (n1 ++ " / " ++ n2, ts)
compareColsDiv (n1, []) (n2, ts') = tablesizeError n1 n2
compareColsDiv (n1, ts') (n2, (t:ts)) = case filterCols t ts' of
                                          Right ts'' -> case compareColsDiv (n1, ts'') (n2, ts) of
                                                          Right (_, ts''') -> Right (n1 ++ " / " ++ n2, ts''')
                                                          err              -> err
                                          Left n     -> coltypeError n n
                                where filterCols _ [] = Right []
                                      filterCols tcname (tc:tcs) = if (fst tcname) == (fst tc)
                                                                   then if snd tcname == snd tc
                                                                        then filterCols tcname tcs
                                                                        else Left tcname
                                                                   else case filterCols tcname tcs of
                                                                          Right tcs' -> Right (tc:tcs')
                                                                          err -> err
