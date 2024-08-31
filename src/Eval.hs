{-# LANGUAGE OverloadedStrings #-}

module Eval
  (
  conversion,
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
import           TypeChecker

import qualified Data.ByteString.Lazy as BL
import           Data.Function
import           Data.Maybe (fromJust)
import           Data.Char (isUpper)
import           Data.List (nub, (\\), sortBy, groupBy)
import           Control.Exception (SomeException, IOException, catch, Exception (toException), try)
import           Database.MySQL.Base (ConnectInfo)
import           System.Directory (doesFileExist)
import           System.IO (writeFile)

-- emptytable
--  tabla para valores inesperados
emptytable :: Table
emptytable = ([], [])

-- separeAtDot
--  separacion de entrada de usuario de columna en (NombreTabla, NombreColumna) 
separeAtDot :: String -> Column
separeAtDot c = let (beforeDot, rest) = span (/= '.') c
                in if null rest then ([], beforeDot)
                                else ([beforeDot], drop 1 rest)

-- columns 
--  a partir de una serie de columnas, se agrupan segun su nombre y tablas de origen (TablasOrigen, NombreColumna)
columns :: TableCols -> [Column]
columns [] = []
columns ((LVar v):cs) = let cs' = columns cs
                            c' = separeAtDot v
                        in map (\grp -> (concatMap fst grp, snd (head grp))) $ groupBy ((==) `on` snd) (sortBy (compare `on` snd) (c':cs'))

-- value
--  convierte entrada de usuario en el valor correspondiente
value :: TableAtom -> Value
value (LVar c) = Col (separeAtDot c)
value (LNum s) = Val (Numb (read s))
value (LString s) = Val (Str s)

-- condition'
--  genera el arbol de condición necesario en la evaluación
condition' :: TableCond -> Condition
condition' (LAnd a b) = And (condition' a) (condition' b)
condition' (LOr a b) = Or (condition' a) (condition' b)
condition' (LGr a b) = Gr (value a) (value b)
condition' (LLr a b) = Lr (value a) (value b)
condition' (LGrEq a b) = Greq (value a) (value b)
condition' (LLrEq a b) = Lreq (value a) (value b)
condition' (LEquals a b) = Eq (value a) (value b)


-- conversion a términos con variables locales, globales y ligadas
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


-- evalInferDef
-- inferidor y evaluador de la operacion de definicion de tabla.
evalInferDef :: TableName -> GlobalE -> LocalE -> OperE -> Term -> Either String (Table, TableType)
evalInferDef n g l o t = case infer g l o (Ren n t) of
                           Left err  -> Left err
                           Right typ -> let (rs, cs) = eval g l o (Ren n t)
                                        in Right ((nub rs, cs), typ)

-- evaluador de términos algebraicos
eval :: GlobalE -> LocalE -> OperE -> Term -> Table
eval g l o t = let (rs, cs) = eval' [] g l o t
               in (nub rs, cs)

eval' :: [(Table, TableType)] -> GlobalE -> LocalE -> OperE -> Term -> Table
eval' _ e _ _ (GlobalTableVar v) = fst $ fromJust $ lookup v e
eval' _ _ l _ (LocalTableVar i) =  fst $ fromJust $ lookup i l
eval' a e l o (Sel cond t) = sel (eval' a e l o t) cond
eval' a e l o (Proy cs t) = proy cs (eval' a e l o t)
eval' a e l o (Ren n t) = ren (eval' a e l o t) n
eval' a e l o (PCart t1 t2) = pcart (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (PNat t1 t2) = pnat (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (Div t1 t2) = divtables (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (Diff t1 t2) = difftables (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (Uni t1 t2) = uni (eval' a e l o t1) (eval' a e l o t2)
eval' a e l o (Int t1 t2) = int (eval' a e l o t1) (eval' a e l o t2)
eval' a _ _ _ (Bound n) = fst (a !! n) -- variable ligada (solo entra en expresión de aplicación)
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

-- conversionOperator
--  crea el termino de la operacion con las variables ligadas 
conversionOperator :: [TableName] -> TableTerm -> Term
conversionOperator args = conversion' (zip args [0..])

-- checkArgs
--  chequea los parametros ingresados por el usuario
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


-- evalOperator
--  evalua la definicion de un operador.
evalOperator :: OperE -> LocalE  -> String -> OperatorArgs -> TableTerm -> Either String [(String, Term)]
evalOperator ops local v args t = case lookup v ops of
                              Just _ -> Left "Opeador existente."
                              Nothing -> case checkArgs args local of
                                          Right _ -> Right $ (v,conversionOperator args t):ops
                                          Left err -> Left err


-- evalImportDB
--  se conecta con una base de datos mysql y devuelve el dataset en tablas.
evalImportDB :: ConnectInfo -> GlobalE -> IO (Either SomeException GlobalE)
evalImportDB cinfo e = try (mysqlconn cinfo e)

-- evalExportCSV
--  vuelca una tabla global sobre un archivo CSV.
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


-- evalImportCSV
--  importa una tabla de un archivo csv y la actualiza en el estado.
evalImportCSV :: String -> String -> GlobalE -> IO (Either SomeException GlobalE)
evalImportCSV file name st = do
    csvData <- catch (BL.readFile file) ((\e -> return "") :: IOException -> IO BL.ByteString )
    if csvData == "" then return $ Left $ toException $ Error $ "No se pudo abrir el archivo: " ++ file ++ "."
    else case csvToTable name csvData of
           Right st' -> return $ Right st'
           Left err  -> return $ Left $ toException $ Error err


-- evalDropTable
--  elimina una tabla del estado de tablas globales. 
evalDropTable:: GlobalE -> String -> Either String GlobalE
evalDropTable s v = case lookup v s of
                 Nothing -> Left "Tabla inexistente."
                 Just _ -> Right $ filter (\(k,val) -> k /= v) s


-- evalDropOp
--  elimina un operador del estado de operadores.                  
evalDropOp:: OperE -> String -> Either String OperE
evalDropOp s v = case lookup v s of
                 Nothing -> Left "Operador inexistente."
                 Just _ -> Right $ filter (\(k,val) -> k /= v) s

-- chequea el nombre del archivo a importar
checkNameFileImport :: GlobalE -> String -> String -> Either String String
checkNameFileImport s file name = let (nf, ext) = separeAtDot file
                        in if null nf || ext /= "csv" then Left "No se trata de un archivo csv."
                           else if not (isUpper (head name)) then Left "Nombre de tabla invalido."
                           else case lookup name s of
                                  Nothing -> Right file
                                  _       -> Left $ "Tabla existente: " ++ name ++ "."

-- chequea el nombre del archivo a exportar
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