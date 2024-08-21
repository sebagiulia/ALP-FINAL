{-# LANGUAGE OverloadedStrings #-}



module Simplytyped
  (
  conversion,
  infer,
  evalDef,
  eval,
  evalImportCSV,
  evalImportDB,
  evalExportCSV,
  evalOperator,
  evalDropTable,
  evalDropOp,
  getDBData,
  checkNameFileExport,
  checkNameFileImport,
  )
where

import           Mysql
import           Csv
import           Error
import           PrettyPrinter
import           Common
import           TableOperators

import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromJust)
import           Data.Text (pack, unpack, strip)
import           Data.Char (isUpper)
import           Data.List (nub)
import           Prelude  hiding ( (>>=), tail )
import           Text.PrettyPrint.HughesPJ ( render )
import           Control.Exception (SomeException, IOException, catch, Exception (toException), try)
import           Database.MySQL.Base (ConnectInfo)
import           System.Directory (doesFileExist)
import           System.IO (writeFile)


emptytable :: Table
emptytable = ([], "null", [])

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
value (LNum s) = Val (Numb (read s))
value (LString s) = Val (Str s)

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

conversion' :: [(String, Int)] -> TableTerm -> Term
conversion' l (LTableVar n) =  if isUpper (head n) then GlobalTableVar n
                               else case lookup n l of
                                            Nothing -> LocalTableVar n
                                            Just i -> Bound i
conversion' l (LProy cols t) = Proy (columns cols) (conversion' l t)
conversion' l (LSel cond t) = Sel (condition' cond) (conversion' l t)
conversion' l (LPNat t1 t2) =  PNat (conversion' l t1) (conversion' l t2)
conversion' l (LRen tn t) =  Ren tn (conversion' l t)
conversion' l (LPCart t1 t2) = PCart (conversion' l t1) (conversion' l t2)
conversion' l (LDiv t1 t2) = Div (conversion' l t1) (conversion' l t2)
conversion' l (LDiff t1 t2) = Diff (conversion' l t1) (conversion' l t2)
conversion' l (LUni t1 t2) = Uni (conversion' l t1) (conversion' l t2)
conversion' l (LInt t1 t2) = Int (conversion' l t1) (conversion' l t2)
conversion' l (LApp op args) = App op args

evalDef :: TableName -> NameEnv Table TableType -> NameEnv Table TableType -> [(String, Term)] -> Term -> Table
evalDef n g l o t = let tab = eval g l o t
                    in ren tab n 

-- evaluador de términos
eval :: NameEnv Table TableType -> NameEnv Table TableType -> [(String, Term)] -> Term -> Table
eval = eval' []

eval' :: [(Table, TableType)] -> NameEnv Table TableType -> NameEnv Table TableType ->  [(String, Term)] -> Term -> Table
eval' _ e l o (GlobalTableVar v) = fst $ fromJust $ lookup v e
eval' _ e l o (LocalTableVar i) =  fst $ fromJust $ lookup i l
eval' a e l o (Sel cond t) = sel (eval' a e l o t) cond
eval' a e l o (Proy cs t) = proy cs (eval' a e l o t)
eval' a e l o (Ren n t) = ren (eval' a e l o t) n
eval' a e l o (PCart t1 t2) = pcart (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (PNat t1 t2) = pnat (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (Div t1 t2) = divtables (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (Diff t1 t2) = difftables (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (Uni t1 t2) = uni (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (Int t1 t2) = int (eval' a e l o t1) (eval' a e l o t2)
eval' a _ _ _ (Bound n) = fst (a !! n)
eval' a g l o (App op args) = case lookup op o of
                                Nothing -> emptytable
                                Just term -> case foldr existargs (Right []) args  of
                                          Right argtables -> eval' argtables g l o term
                                          Left err -> emptytable
                            where existargs _ (Left err) = Left err
                                  existargs a (Right ags') = if isUpper (head a)
                                    then case lookup a g of
                                           Nothing -> Left $ "No se encuentra variable global " ++ a ++ "."
                                           Just tt -> Right $ tt:ags'
                                    else case lookup a l of
                                           Nothing -> Left $ "No se encuentra variable local " ++ a ++ "."
                                           Just tt -> Right $ tt:ags'
 
conversionOperator :: [TableName] -> TableTerm -> Term
conversionOperator args t = conversion' (zip args [0..]) t

evalOperator :: [(String, Term)] -> String -> OperatorArgs -> TableTerm -> Either String [(String, Term)]
evalOperator ops v args t = case lookup v ops of
                              Just _ -> Left "Opeador existente."
                              Nothing -> Right $ (v,conversionOperator args t):ops



evalImportDB :: ConnectInfo -> NameEnv Table TableType -> IO (Either SomeException (NameEnv Table TableType))
evalImportDB cinfo e = try (mysqlconn cinfo e)

evalExportCSV :: String -> String -> NameEnv Table TableType -> IO (Either SomeException String)
evalExportCSV v f s = do
            fileExists <- try $ doesFileExist f
            case fileExists of
              Right bool -> if bool then return $ Left $ toException $ Error  "Archivo existente en exports/."
                            else  case lookup v s of
                                   Nothing -> return $ Left $ toException $ Error  "Variable inexistente." --No deberia entrar
                                   Just (t,_)  -> let tablecsv = tableToCsv t
                                                  in do result <- try (writeFile f tablecsv) :: IO (Either IOError ())
                                                        case result of
                                                            Left ex  ->  return $ Left $ toException $ Error  "No se pudo crear el archivo."
                                                            Right _  -> return $ Right "Archivo creado con exito."
              Left err -> return $ Left err

evalImportCSV :: String -> String -> NameEnv Table TableType -> IO (Either SomeException (NameEnv Table TableType))
evalImportCSV file name st = do
    csvData <- catch (BL.readFile file) ((\e -> return "") :: IOException -> IO BL.ByteString )
    if csvData == "" then return $ Left $ toException $ Error $ "No se pudo abrir el archivo: " ++ file ++ "."
    else case csvToTable name csvData of
           Right st' -> return $ Right st'
           Left err  -> return $ Left $ toException $ Error err

evalDropTable:: NameEnv Table TableType -> String -> Either String (NameEnv Table TableType)
evalDropTable s v = case lookup v s of
                 Nothing -> Left "Variable inexistente."
                 Just _ -> Right $ filter (\(k,val) -> k /= v) s
evalDropOp:: [(String, Term)] -> String -> Either String [(String, Term)]
evalDropOp s v = case lookup v s of
                 Nothing -> Left "Operador inexistente."
                 Just _ -> Right $ filter (\(k,val) -> k /= v) s

checkNameFileImport :: NameEnv Table TableType -> String -> String -> Either String String
checkNameFileImport s file name = let (nf, ext) = separeAtDot file
                        in if nf == "" || ext /= "csv" then Left "No se trata de un archivo csv.\n"
                           else if not (isUpper (head name)) then Left "Nombre de variable invalido."
                           else case lookup name s of
                                  Nothing -> Right $ file
                                  _       -> Left $ "Variable existente: " ++ name ++ ".\n"

checkNameFileExport :: NameEnv Table TableType -> String -> String -> Either String String
checkNameFileExport s v f = case lookup v s of
                     Nothing -> Left "Variable inexistente."
                     _       -> let (ext, n) = break (=='.') (reverse f)
                                in if ext /= "vsc" then Left "Falta extension csv en archivo."
                                   else if n == "" then Left "Nombre de archivo invalido."
                                        else Right $ "exports/" ++ f

-- type checker
infer :: NameEnv Table TableType -> NameEnv Table TableType -> [(String, Term)] -> Term -> Either String TableType
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

infer' :: [(Table, TableType)] -> NameEnv Table TableType -> NameEnv Table TableType -> [(String, Term)] -> Term -> Either String TableType
infer' _ _ l _ (LocalTableVar n) = case lookup n l of
                                   Just (_, t) -> ret t
                                   _           -> notfoundError n
infer' _ e l _ (GlobalTableVar n) = case lookup n e of
                                  Just (_, t) -> ret t
                                  _           -> notfoundError n
infer' c e l o (Sel cond t) = infer' c e l o t
infer' c e l o (Proy cs t) = case infer' c e l o t of
                          Right t -> ret (proyInfer cs t)
                          err     -> err
infer' c e l o (Ren n t) = case infer' c e l o t of
                          Right t -> ret (n, snd t)
                          err -> err
infer' c e l o (PCart t1 t2) = case infer' c e l o t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) ->  if n1 == n2
                                                                       then nameError n1
                                                                       else ret (n1 ++ "*"++ n2, t1cs ++ t2cs)
infer' c e l o (PNat t1 t2) = case infer' c e l o (PCart t1 t2) of
                            Left e -> Left e
                            _ -> case infer' c e l o t1 of
                                   Left e  -> err e
                                   Right (n1, t1cs) -> case infer' c e l o t2 of
                                                          Left e  -> err e
                                                          Right (n2, t2cs) -> case matchCols (n1, t1cs) (n2, t2cs) of
                                                                                  Right t -> ret t
                                                                                  err -> err
infer' c e l o (Uni t1 t2) = case infer' c e l o t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareCols (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ " U " ++ n2, t)
                                                                       err -> err
infer' c e l o (Int t1 t2) = case infer' c e l o t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareCols (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ " I " ++ n2, t)
                                                                       err -> err
infer' c e l o (Diff t1 t2) = case infer' c e l o t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareCols (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ " - " ++ n2, t)
                                                                       err -> err
infer' c e l o (Div t1 t2) = case infer' c e l o t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareColsDiv (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ " / " ++ n2, t)
                                                                       err -> err
infer' c _ _ _ (Bound i) = if i < length c then ret $ snd (c !! i)
                                           else Left "Argumentos invalidos."
infer' c g l ops (App op args) = case lookup op ops of
                           Nothing -> Left "Operador invalido."
                           Just term -> case foldr existargs (Right []) args  of
                                          Right argtables -> if length argtables /= length args 
                                                             then Left "Argumentos invalidos."
                                                             else infer' argtables g l ops term
                                          Left err -> Left err
                            where existargs _ (Left err) = Left err
                                  existargs a (Right ags') = if isUpper (head a)
                                    then case lookup a g of
                                           Nothing -> Left $ "No se encuentra variable global " ++ a ++ "."
                                           Just tt -> Right $ tt:ags'
                                    else case lookup a l of
                                           Nothing -> Left $ "No se encuentra variable local " ++ a ++ "."
                                           Just tt -> Right $ tt:ags'

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
