module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print, error )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )

import           TableOperators
import           Common
import           PrettyPrinter
import           Simplytyped
import           Parse
import           Error
---------------------
--- Interpreter
---------------------

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" 0 [] [] [])

iname :: String
iname = "AR-SQL: Consultas en algebra relacional."

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  {  inter :: Bool      -- True, si estamos en modo interactivo.
    ,lfile :: String -- Ultimo archivo cargado (para hacer "reload")
    ,nq    :: Int -- Numero de queries.
    ,gv    :: GlobalE  -- Entorno con tablas globales y su valor  [(TableName, (Value, Type))]
    ,lv    :: LocalE -- Entorno con tablas locales y su valor [(TableName, (Value, Type))]
    ,ov    :: OperE -- Entorno con operadores [(OperatorName, Term)]
  }

iprompt :: State -> String
iprompt st = "GV:" ++ show (length (gv st)) ++ "|LV:" ++ show (length (lv st)) ++ "|OV:" ++ show (length (ov st)) ++  "|> "

--read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter lfile nq gv lv ov) =
  let rec st = do
        mx <- MC.catch
          (if inter then getInputLine (iprompt st) else lift $ fmap Just getLine)
          (lift . ioExceptionCatcher)
        case mx of
          Nothing -> return ()
          Just "" -> rec st
          Just x  -> do
            c   <- interpretCommand x
            st' <- handleCommand st c
            maybe (return ()) rec st'
  in  do
        state' <- compileFiles (prelude : args) state
        when inter $ lift $ putStrLn
          (  "Intérprete de "
          ++ iname
          ++ ".\n"
          ++ "Escriba :? para recibir ayuda."
          )
        --  enter loop
        rec state' { inter = True }

data Command = Compile CompileForm
              | Print String
              | Recompile
              | Ls
              | Quit
              | Help
              | Noop
              | FindType String

data CompileForm = CompileInteractive  String
                  | CompileFile         String

interpretCommand :: String -> InputT IO Command
interpretCommand x = lift $ if isPrefixOf ":" x
  then do
    let (cmd, t') = break isSpace x
    let t         = dropWhile isSpace t'
    --  find matching commands
    let matching = filter (\(Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
    case matching of
      [] -> do
        putStrLn
          ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda."
          )
        return Noop
      [Cmd _ _ f _] -> do
        return (f t)
      _ -> do
        putStrLn
          (  "Comando ambigüo, podría ser "
          ++ concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ])
          ++ "."
          )
        return Noop
  else return (Compile (CompileInteractive x))

handleCommand :: State -> Command -> InputT IO (Maybe State)
handleCommand state@(S inter lfile nq gv lv ov) cmd = case cmd of
  Quit   -> lift $ when (not inter) (putStrLn "!@#$^&*") >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Ls -> lift $ do
    when (length gv > 0) $ do {
      putStrLn "Global variables:";
      putStr (unlines [ s | s <- reverse (nub (map (\v -> (' ':fst v)) gv)) ]) }
    when (length lv > 0) $ do {
      putStrLn "Local variables:";
      putStr (unlines [ s | s <- reverse (nub (map (\v -> (' ':fst v)) lv)) ]) }
    when (length ov > 0) $ do {
         putStrLn "Operators:";
         putStr (unlines [ s | s <- reverse (nub (map (\v -> (' ':fst v)) ov)) ]) }
    return (Just state)
  Compile c -> do
    state' <- case c of
      CompileInteractive s -> compilePhrase state s
      CompileFile        f -> compileFile (state { lfile = f, inter = False }) f
    return (Just state')
  Print s ->
    let s' = reverse (dropWhile isSpace (reverse (dropWhile isSpace s)))
    in  printPhrase s' >> return (Just state)
  Recompile -> if null lfile
    then lift $ error "No hay un archivo cargado.\n" >> return (Just state)
    else handleCommand state (Compile (CompileFile lfile))
  FindType s -> do
    x' <- parseIO "<interactive>" term_parse s
    t  <- case x' of
      Nothing -> return $ Left "Error en el parsing."
      Just x  -> return $ infer gv lv ov $ conversion $ x
    case t of
      Left  err -> lift (typeError err) >> return ()
      Right t'  -> lift $ putStrLn $ render $ printType t'
    return (Just state)

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":ls"] "" (const Ls) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  , Cmd [":print"] "<exp>" Print "Imprime un término de tabla y sus ASTs"
  , Cmd [":reload"]
        "<file>"
        (const Recompile)
        "Volver a cargar el último archivo"
  , Cmd [":quit"]       ""       (const Quit) "Salir del intérprete"
  , Cmd [":help", ":?"] ""       (const Help) "Mostrar esta lista de comandos"
  , Cmd [":type"]       "<term>" (FindType)   "Inferir el tipo de un término"
  ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs =
  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n"
    ++ "c es el primer caracter del nombre completo.\n\n"
    ++ "<expr>                             evaluar la expresión\n"
    ++ "table <var> = <expr>               definir una tabla global\n"
    ++ "import csv <file> as <var>         importar una tabla en formato csv \n"
    ++ "import database [ <conndata> ]     importar un dataset completo desde un DBMS MySQL \n"
    ++ "export csv <var> as <file>         exportar una tabla en formato csv sobre la carpeta exports/\n"
    ++ "drop table <var>                   eliminar una tabla global\n"
    ++ "drop operator <var>                eliminar un operador\n"
    ++ "T [\"<text>\"]                       imprimir texto\n"
    ++ "<var> -> <expr>                    definir una tabla local\n"
    ++ "operator <var> = (<args>)=> <expr> definir un operador\n"
    ++ unlines
         (map
           (\(Cmd c a _ d) ->
             let
               ct =
                 concat
                   (intersperse ", "
                                (map (++ if null a then "" else " " ++ a) c)
                   )
             in  ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d
           )
           cs
         )

compileFiles :: [String] -> State -> InputT IO State
compileFiles xs s =
  foldM (\s x -> compileFile (s { lfile = x, inter = False }) x) s xs

compileFile :: State -> String -> InputT IO State
compileFile state@(S inter lfile nq v lv ov) f = do
  lift $ putStrLn ("Abriendo " ++ f ++ "...")
  let f' = reverse (dropWhile isSpace (reverse f))
  x <- lift $ Control.Exception.catch
    (readFile f')
    (\e -> do
      let err = show (e :: IOException)
      hPutStr stderr
              ("No se pudo abrir el archivo " ++ f' ++ ": " ++ err ++ "\n")
      return ""
    )
  stmts <- parseIO f' (stmts_parse) x
  st <- maybe (return state { inter = True, nq = 0 }) (foldM handleStmt state) stmts
  return st { inter = True, nq = 0 }

compilePhrase :: State -> String -> InputT IO State
compilePhrase state x = do
  x' <- parseIO "<interactive>" stmt_parse x
  case x' of
    Just t -> (handleStmt state t)
    _ -> return state

printPhrase :: String -> InputT IO ()
printPhrase x = do
  x' <- parseIO "<interactive>" stmt_parse x
  maybe (return ()) (printStmt . fmap (\y -> (y, conversion y))) x'

printStmt :: Stmt (TableTerm, Term) -> InputT IO ()
printStmt stmt = lift $ do
  let outtext = case stmt of
        Eval (d, e) ->
          "TableTerm AST:\n"
            ++ show d
            ++ "\n\nTerm AST:\n"
            ++ show e
            ++ "\n\nSe muestra como:\n"
            ++ render (printTerm e)
        _ -> show stmt
  putStrLn outtext

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

handleStmt :: State -> Stmt TableTerm -> InputT IO State
handleStmt state stmt = lift $ do
  let  q = case stmt of
            Text _ -> False
            _      -> True
  _ <- when (not (inter state) && q) $ do putStrLn $ "> Query " ++ show ((nq state) + 1)  ++ "." 
  st <- case stmt of
    ImportDB d     -> checkImportDB d 
    ImportCSV f v  -> checkImportCSV f v
    ExportCSV v f  -> checkExportCSV v f
    Table x e      -> checkDefTable x (conversion e)
    Assign x e     -> if isUpper (head x) 
                      then error "Nombre de tabla invalido, debe comenzar en minusculas." >> return state
                      else checkType x (conversion e) 1
    Eval e         -> checkType it (conversion e) 0
    DropTable v    -> checkDropTable v
    DropOp v       -> checkDropOp v
    Operator v a e -> checkEvalOp v a e
    Text str       -> putStrLn ("> T: " ++ str) >> return state
  if (not (inter st)) then return st { nq = (nq st) + 1 }
                         else return st
 where
  checkDefTable i t = do
    if not (isUpper (head i))
    then error "Nombre de tabla invalido, el primer caracter debe ser en mayusculas" >> return state
    else case infer (gv state) (lv state) (ov state) t of
          Left  err -> typeError err >> return state
          Right ty  -> do
            let r = evalInferDef i (gv state) (lv state) (ov state) t
            case r of
              Left err -> typeError err >> return state
              Right (v, ty) -> 
                  if (elem i (map fst (gv state)))
                  then do msg ("Tabla " ++ i ++ " actualizada.")
                          return (state { gv = map (\(k,va) -> if k == i then (k, (v,ty)) else (k,va)) (gv state), lv = [] })
                  else do putStrLn i   
                          return (state { gv = (i,(v,ty)) : gv state, lv = [] })
            
  checkType i t a = do
    case infer (gv state) (lv state) (ov state) t of
      Left  err -> typeError err >> return state
      Right ty  -> checkEval i t ty a
  checkEval i t ty a = do
    let v = eval (gv state) (lv state) (ov state) t
    _ <- when (a /= 1) $ do
      let outtext = render (printTable v)
      putStrLn outtext 
    if a == 1  
    then if (elem i (map fst (lv state)))
         then return (state { lv = map (\(k,va) -> if k == i then (k, (v,ty)) else (k,va)) (lv state)})
         else return (state { lv = (i,(v,ty)) : lv state })
    else return (state {lv = []})
  checkImportDB d = do
    case getDBData d of
      Left  err -> error err >> return state
      Right dbData  -> do
        v <- evalImportDB dbData (gv state)
        case v of
          Right _ -> msg "Dataset cargado."
          Left ex -> exError ex
        let newst = case v of
                      Right new -> new
                      _ -> (gv state)
        return (state { gv = newst})
  checkImportCSV f n = do
    case checkNameFileImport (gv state) f n of
      Left err -> error err >> return state
      Right _ -> do
          v <- evalImportCSV f n (gv state)
          case v of
           Right ts -> msg $ "Tabla cargada: " ++ n
           Left err -> exError err
          let newst = case v of
                        Right new -> new
                        _ -> []
          return (state { gv = newst ++ gv state})
  checkExportCSV v f = do
    case checkNameFileExport (gv state) v f of
      Left err -> error err >> return state
      Right f' -> do
          v <- evalExportCSV v f' (gv state)
          case v of
           Right _ -> msg "Archivo creado con exito."
           Left ex ->  exError ex
          return state
  checkDropTable v = do
                  case evalDropTable (gv state) v of
                      Left err -> error err >> return state
                      Right st -> msg ("Tabla " ++ v ++ " eliminada.") >> return (state { gv = st })
  checkDropOp v = do
                  case evalDropOp (ov state) v of
                      Left err -> error err >> return state
                      Right st -> msg ("Operador " ++ v ++ " eliminado.") >> return (state { ov = st })
  checkEvalOp v a e = do
                  case evalOperator (ov state) (lv state) v a e of
                    Left err -> error err >> return (state { lv = [] })
                    Right st -> msg ("Operador " ++ v ++ " cargado.") >> return (state { ov = st, lv = []})

prelude :: String
prelude = "Ejemplos/Prelude.arsql"

it :: String
it = "it"


