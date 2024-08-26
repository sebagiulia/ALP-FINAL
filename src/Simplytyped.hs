{-# LANGUAGE OverloadedStrings #-}



module Simplytyped
  (
  conversion,
  infer,
  evalInferDef,
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
import           Data.List (nub, (\\), sortBy, groupBy, find, isSubsequenceOf)
import           Prelude  hiding ( (>>=), tail )
import           Text.PrettyPrint.HughesPJ ( render )
import           Control.Exception (SomeException, IOException, catch, Exception (toException), try)
import           Database.MySQL.Base (ConnectInfo)
import           System.Directory (doesFileExist)
import           System.IO (writeFile)
import Data.Function


emptytable :: Table
emptytable = ([], [])

-- Separe between TableName and ColumnName
-- Example: "Movies.name" -> C "Movies" "name" 
-- Example: "name" -> C "" "name" 
separeAtDot :: String -> Column
separeAtDot c = let (beforeDot, rest) = span (/= '.') c
                in if null rest then ([], beforeDot)
                                else ([beforeDot], drop 1 rest)
columns :: TableCols -> [Column]
columns [] = []
columns ((LVar v):cs) = let cs' = columns cs
                            c' = separeAtDot v
                        in map (\grp -> (concatMap fst grp, snd (head grp))) $ groupBy ((==) `on` snd) (sortBy (compare `on` snd) (c':cs'))


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

evalInferDef :: TableName -> GlobalE -> LocalE -> OperE -> Term -> Either String (Table, TableType)
evalInferDef n g l o t = case infer g l o (Ren n t) of
                           Left err  -> Left err
                           Right typ -> let (rs, cs) = eval g l o (Ren n t)
                                        in Right ((nub rs, cs), typ)   

-- evaluador de términos
eval :: GlobalE -> LocalE -> OperE -> Term -> Table
eval g l o t = let (rs, cs) = eval' [] g l o t
               in (nub rs, cs)

eval' :: [(Table, TableType)] -> GlobalE -> LocalE -> OperE -> Term -> Table
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
                                           Nothing -> Left $ "No se encuentra tabla global " ++ a ++ "."
                                           Just tt -> Right $ tt:ags'
                                    else case lookup a l of
                                           Nothing -> Left $ "No se encuentra tabla local " ++ a ++ "."
                                           Just tt -> Right $ tt:ags'

conversionOperator :: [TableName] -> TableTerm -> Term
conversionOperator args t = conversion' (zip args [0..]) t

checkArgs :: OperatorArgs -> LocalE -> Either String OperatorArgs
checkArgs args local = if foldr (\a g -> (isUpper . head) a || g) False args
                       then Left "Argumentos invalidos, deben iniciar con minuscula."
                       else case foldr f (Right args) args of
                              Left err -> Left err
                              Right _  -> Right args
                      where f s (Left err) = Left err
                            f s _          = case lookup s local of
                                                Just _ -> Left $ "Tabla local existente: " ++ s 
                                                _ -> Right args
                            

evalOperator :: OperE -> LocalE  -> String -> OperatorArgs -> TableTerm -> Either String [(String, Term)]
evalOperator ops local v args t = case lookup v ops of
                              Just _ -> Left "Opeador existente."
                              Nothing -> case checkArgs args local of
                                          Right _ -> Right $ (v,conversionOperator args t):ops
                                          Left err -> Left err



evalImportDB :: ConnectInfo -> GlobalE -> IO (Either SomeException GlobalE)
evalImportDB cinfo e = try (mysqlconn cinfo e)

evalExportCSV :: String -> String -> GlobalE -> IO (Either SomeException String)
evalExportCSV v f s = do
            fileExists <- try $ doesFileExist f
            case fileExists of
              Right bool -> if bool then return $ Left $ toException $ Error  "Archivo existente en exports/."
                            else  case lookup v s of
                                   Nothing -> return $ Left $ toException $ Error  "Tabla inexistente." --No deberia entrar
                                   Just (t,_)  -> let tablecsv = tableToCsv t
                                                  in do result <- try (writeFile f tablecsv) :: IO (Either IOError ())
                                                        case result of
                                                            Left ex  ->  return $ Left $ toException $ Error  "No se pudo crear el archivo."
                                                            Right _  -> return $ Right "Archivo creado con exito."
              Left err -> return $ Left err

evalImportCSV :: String -> String -> GlobalE -> IO (Either SomeException GlobalE)
evalImportCSV file name st = do
    csvData <- catch (BL.readFile file) ((\e -> return "") :: IOException -> IO BL.ByteString )
    if csvData == "" then return $ Left $ toException $ Error $ "No se pudo abrir el archivo: " ++ file ++ "."
    else case csvToTable name csvData of
           Right st' -> return $ Right st'
           Left err  -> return $ Left $ toException $ Error err

evalDropTable:: GlobalE -> String -> Either String GlobalE
evalDropTable s v = case lookup v s of
                 Nothing -> Left "Tabla inexistente."
                 Just _ -> Right $ filter (\(k,val) -> k /= v) s
evalDropOp:: [(String, Term)] -> String -> Either String [(String, Term)]
evalDropOp s v = case lookup v s of
                 Nothing -> Left "Operador inexistente."
                 Just _ -> Right $ filter (\(k,val) -> k /= v) s

checkNameFileImport :: GlobalE -> String -> String -> Either String String
checkNameFileImport s file name = let (nf, ext) = separeAtDot file
                        in if null nf || ext /= "csv" then Left "No se trata de un archivo csv."
                           else if not (isUpper (head name)) then Left "Nombre de tabla invalido."
                           else case lookup name s of
                                  Nothing -> Right $ file
                                  _       -> Left $ "Tabla existente: " ++ name ++ "."

checkNameFileExport :: GlobalE -> String -> String -> Either String String
checkNameFileExport s v f = if not (isUpper (head v))
                            then Left "Solo las variables globales son exportables."
                            else
                              case lookup v s of
                               Nothing -> Left "Tabla inexistente."
                               _       -> let (ext, n) = break (=='.') (reverse f)
                                          in if ext /= "vsc" then Left "Falta extension csv en archivo."
                                             else if n == "" then Left "Nombre de archivo invalido."
                                                  else Right $ "exports/" ++ f

-- type checker
infer :: GlobalE -> LocalE -> OperE -> Term -> Either String TableType
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

infer' :: [(Table, TableType)] -> GlobalE -> LocalE -> OperE -> Term -> Either String TableType
infer' _ _ l _ (LocalTableVar n) = case lookup n l of
                                   Just (_, t) -> ret t
                                   _           -> notfoundError n
infer' _ e l _ (GlobalTableVar n) = case lookup n e of
                                  Just (_, t) -> ret t
                                  _           -> notfoundError n
infer' c e l o (Sel cond t) = case infer' c e l o t of
                               Right typ -> checkCond cond typ
                               err -> err
infer' c e l o (Proy cs t) = case infer' c e l o t of
                          Right t -> case proyInfer cs t of
                                      Right ty -> ret ty
                                      err -> err
                          err     -> err
infer' c e l o (Ren n t) = case infer' c e l o t of
                            Right ty -> ret (n, map (\((l, cn), t) -> 
                                                          if length l == 1 
                                                          then (([n], cn), t)
                                                          else ((l, cn), t)) (snd ty))
                            err -> err
infer' c e l o (PCart t1 t2) = case infer' c e l o t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) ->  if n1 == n2
                                                                       then nameError n1
                                                                       else case concatColsTyp t1cs t2cs of
                                                                              Right ts -> ret (n1 ++ "*" ++ n2, ts)
                                                                              Left err -> coltypeError err err
infer' c e l o (PNat t1 t2) = case infer' c e l o (PCart t1 t2) of
                            Left e -> Left e
                            _ -> case infer' c e l o t1 of
                                   Left e  -> err e
                                   Right (n1, t1cs) -> case infer' c e l o t2 of
                                                          Left e  -> err e
                                                          Right (n2, t2cs) -> case matchCols (n1, t1cs) (n2, t2cs) of
                                                                                  Right t -> ret t
                                                                                  Left err -> coltypeError err err
infer' c e l o (Uni t1 t2) = case infer' c e l o t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareCols (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ "U" ++ n2, t)
                                                                       err -> err
infer' c e l o (Int t1 t2) = case infer' c e l o t1 of
                             Left e  -> err e
                             Right (n1, t1cs) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right (n2, t2cs) -> case compareCols (n1, t1cs) (n2, t2cs) of
                                                                       Right (_,t) -> ret (n1 ++ "I" ++ n2, t)
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
                                           Nothing -> Left $ "No se encuentra tabla global " ++ a ++ "."
                                           Just tt -> Right $ tt:ags'
                                    else case lookup a l of
                                           Nothing -> Left $ "No se encuentra tabla local " ++ a ++ "."
                                           Just tt -> Right $ tt:ags'

proyInfer :: [Column] -> TableType -> Either String TableType
proyInfer [] (n,_) = Right (n, [])
proyInfer _ (n, []) = Right (n, [])
proyInfer (c:cs) (n, ts) = case lookup (snd c) (map (\((x,y),z) -> (y,(x,z))) ts) of
                        Nothing -> Left $ "Columna " ++ snd c ++ " invalida."
                        Just (ot,t) -> case proyInfer cs (n, ts) of
                                        Right (_, ts') -> if null (fst c)
                                                          then if length ot > 1
                                                               then Left "Columna ambigua, nombres repetidos."
                                                               else Right (n, ((ot, snd c),t):ts')
                                                          else if foldr (\e b-> b && elem e ot) True (fst c)
                                                               then Right (n, (c,t):ts')
                                                               else Left "Alguna/s columnas inexistentes."
                                        Left err -> Left err


concatColsTyp :: [(Column, Type)] -> [(Column, Type)] -> Either (Column, Type) [(Column, Type)] -- -> [(([tab1,tab2,...], col), type)] = tab1.col, tab2.col, ...  
concatColsTyp xs ys = foldr combineGroup (Right []) grouped
  where -- [[((ts, c),ty1), (ts1, c),ty1),...],[]]
    combined = xs ++ ys
    sorted = sortBy (compare `on` (snd . fst)) combined
    grouped = groupBy ((==) `on` (snd . fst)) sorted -- Agrupamos por nombre de columna
    -- Para columnas con el mismo nombre, agrupamos segun tabla de origen: ([t1,t2], cname)
    combineGroup grp (Right cs) = if length (nub (map snd grp)) > 1
                                  then Left (head grp)
                                  else Right $ ((concatMap (fst. fst) grp, snd (fst (head grp))), snd (head grp)):cs
    combineGroup grp (Left err) = Left err

matchCols :: TableType -> TableType -> Either (Column, Type) TableType
matchCols (n1, []) (n2, ts) = Right (n1 ++ "|x|" ++ n2, ts)
matchCols (n1, t1) (n2, t2) = case concatColsTyp t1 t2 of
                                Right ts -> Right (n1 ++ "|x|" ++ n2, map f ts)
                                Left err -> Left err
                                where
                                f ((tables, col), typ) = if length tables > 1
                                                         then (([head tables], col), typ)
                                                         else ((tables, col), typ)

compareCols :: TableType -> TableType -> Either String TableType
compareCols (n1, []) _ = Right (n1, [])
compareCols _ (n2, []) = Right (n2, [])
compareCols (n1, ts1) (n2, ts2) = if length ts1 /= length ts2
                                  then tablesizeError n1 n2
                                  else case foldr compare' (Right (n1, ts1)) (zip ts1 ts2) of
                                        Left err -> Left err
                                        Right _  -> Right (n1, ts1)
                                  where compare' _ (Left err) = Left err
                                        compare' (a@((ts, c1),ty1), b@((ts', c2),ty2)) _
                                          | c1 /= c2 = colnameError c1 c2
                                          | ty1 /= ty2 = coltypeError a b
                                          | length ts == 1 && length ts' == 1 = Right (n1, ts1)
                                          | ts \\ ts' /= [] = colnameError c1 c2 
                                          | otherwise = Right (n1, ts1)

checkCond :: Condition -> TableType -> Either String TableType
checkCond Empty ty = Right ty
checkCond (And c1 c2) ty = case checkCond c1 ty of
                             Right _ -> checkCond c2 ty
                             err -> err
checkCond (Or c1 c2) ty = case checkCond c1 ty of
                             Right _ -> checkCond c2 ty
                             err -> err
checkCond (Gr v1 v2) ty = case checkVal v1 ty of
                          Right _ -> checkVal v2 ty 
                          err -> err 
checkCond (Lr v1 v2) ty = case checkVal v1 ty of
                          Right _ -> checkVal v2 ty 
                          err -> err 
checkCond (Greq v1 v2) ty = case checkVal v1 ty of
                          Right _ -> checkVal v2 ty 
                          err -> err 
checkCond (Lreq v1 v2) ty = case checkVal v1 ty of
                          Right _ -> checkVal v2 ty 
                          err -> err 
checkCond (Eq v1 v2) ty = case checkVal v1 ty of
                          Right _ -> checkVal v2 ty 
                          err -> err 

checkVal :: Value -> TableType -> Either String TableType
checkVal (Col c) typ = case find (\a -> snd c == snd a) (map fst (snd typ)) of
                              Nothing -> Left $ "No se encuentra la columna " ++ snd c
                              Just (ts,_) -> if null (fst c)  
                                             then if length ts == 1 then Right typ
                                                  else Left "Columna ambigua, nombres repetidos."
                                             else if isSubsequenceOf (fst c) ts 
                                                  then Right typ
                                                  else Left "Columnas invalidas"
checkVal _ typ = Right typ
   


-- [c1, c2, c3, c4] / [c2,c3] -> [c1,c4]
compareColsDiv :: TableType -> TableType -> Either String TableType
compareColsDiv (n1, ts) (n2, []) = Right (n1 ++ " / " ++ n2, ts)
compareColsDiv (n1, []) (n2, ts') = tablesizeError n1 n2
compareColsDiv (n1, ts) (n2, ts') = case foldr (filtercols ts') (Right []) ts of
                                        Right l -> Right (n1 ++ " / " ++ n2, l)
                                        Left err -> coltypeError err err
                                 where filtercols _ _ (Left err) =  Left err
                                       filtercols den ((t, c), typ) (Right ls) =
                                             case find (\((_,c'),_) -> c == c') den of
                                             Just ((t', _), typ') -> if typ /= typ'
                                                                     then Left ((t, c), typ)
                                                                     else case t \\ t' of
                                                                            [] -> Right ls
                                                                            ls' -> Right $ ((ls', c), typ):ls
                                             Nothing              -> Right ls
