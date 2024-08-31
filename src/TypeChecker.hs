module TypeChecker 
where 
import Common
import TableOperators
import PrettyPrinter

import           Data.Function
import           Data.Char (isUpper)
import           Data.List (nub, (\\), sortBy, groupBy, find, isSubsequenceOf)
import           Text.PrettyPrint.HughesPJ ( render )

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

nameError :: TableName -> Either String TableType
nameError n =
  err $  "Se esperaban nombres de tablas distintos: " ++ n

coltypeError :: (Column, Type) -> Either String TableType
coltypeError (c,_) =
  err $  "Columnas con mismo nombre pero distinto tipo: " ++ snd c ++ "."

tablesizeError :: TableName -> TableName -> Either String TableType
tablesizeError n1 n2 =
  err $  "Tablas con numero invalidos de columnas: " ++ n1 ++ " y " ++ n2

colnameError :: TableName -> TableName -> Either String TableType
colnameError n1 n2 =
  err $  "Tablas con diferentes columnas: " ++ n1 ++ " y " ++ n2

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
                             Right a@(n1,_) -> case infer' c e l o t2 of
                                                  Left e  -> err e
                                                  Right b@(n2,_) ->  if n1 == n2
                                                                       then nameError n1
                                                                       else case concatColsTyp a b of
                                                                              Right ty -> ret ty
                                                                              Left err -> Left err
infer' c e l o (PNat t1 t2) = case infer' c e l o (PCart t1 t2) of
                            Left e -> Left e
                            _ -> case infer' c e l o t1 of
                                   Left e  -> err e
                                   Right (n1, t1cs) -> case infer' c e l o t2 of
                                                          Left e  -> err e
                                                          Right (n2, t2cs) -> case matchCols (n1, t1cs) (n2, t2cs) of
                                                                                  Right t -> ret t
                                                                                  Left err -> Left err
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

-- infiere el tipo de una tabla segun la proyeccion de ciertas columnas
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


-- concatena tipos en base a sus columnas
concatColsTyp :: TableType -> TableType -> Either String TableType -- -> [(([tab1,tab2,...], col), type)] = tab1.col, tab2.col, ...  
concatColsTyp (n1,xs) (n2, ys) = foldr combineGroup (Right ("", [])) grouped
  where
    combined = xs ++ ys
    sorted = sortBy (compare `on` (snd . fst)) combined
    grouped = groupBy ((==) `on` (snd . fst)) sorted -- Agrupamos por nombre de columna
    -- Para columnas con el mismo nombre, agrupamos segun tabla de origen: ([t1,t2], cname)
    combineGroup grp (Right (n,cs)) = if length (nub (map snd grp)) > 1 then coltypeError (head grp)
                                      else let ot = concatMap (fst. fst) grp
                                           in if length (nub ot) < length ot then Left "Columnas repetidas. Sugerencia: Renombrar terminos."
                                              else Right (n1 ++ "*" ++ n2, ((ot, snd (fst (head grp))), snd (head grp)):cs)

    combineGroup grp (Left err) = Left err


-- devuelve el tipo de la nueva tabla como resultado de agrupar y combinar dos tipos distintos
matchCols :: TableType -> TableType -> Either String TableType
matchCols (n1, []) (n2, ts) = Right (n1 ++ "|*|" ++ n2, ts)
matchCols a@(n1,_) b@(n2,_) = case concatColsTyp a b of
                                Right (_,ts) -> Right (n1 ++ "|*|" ++ n2, map f ts)
                                Left err -> Left err
                                where
                                f ((tables, col), typ) = if length tables > 1
                                                         then (([head tables], col), typ)
                                                         else ((tables, col), typ)


-- compara dos tipos de tabla
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
                                          | ty1 /= ty2 = coltypeError a
                                          | length ts == 1 && length ts' == 1 = Right (n1, ts1)
                                          | ts \\ ts' /= [] = colnameError c1 c2
                                          | otherwise = Right (n1, ts1)


-- chequea la condicion ingresada por el usuario.
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



-- compara y devuelve el nuevo tipo de una tabla resultado de una division
compareColsDiv :: TableType -> TableType -> Either String TableType
compareColsDiv (n1, ts) (n2, []) = Right (n1 ++ " / " ++ n2, ts)
compareColsDiv (n1, []) (n2, ts') = tablesizeError n1 n2
compareColsDiv (n1, ts) (n2, ts') = case foldr (filtercols ts') (Right []) ts of
                                        Right l -> Right (n1 ++ " / " ++ n2, l)
                                        Left err -> coltypeError err
                                 where filtercols _ _ (Left err) =  Left err
                                       filtercols den ((t, c), typ) (Right ls) =
                                             case find (\((_,c'),_) -> c == c') den of
                                             Just ((t', _), typ') -> if typ /= typ'
                                                                     then Left ((t, c), typ)
                                                                     else case t \\ t' of
                                                                            [] -> Right ls
                                                                            ls' -> Right $ ((ls', c), typ):ls
                                             Nothing              -> Right ls
