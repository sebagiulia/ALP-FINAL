module Main where

import           Control.Exception              ( catch
                                                , IOException
                                                )
import           Control.Monad.Except
import           Data.Char
import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( print )
import           System.Console.Haskeline
import qualified Control.Monad.Catch           as MC
import           System.Environment
import           System.IO               hiding ( print )
import           Text.PrettyPrint.HughesPJ      ( render
                                                , text
                                                )

import TableOperators
import           Common
import           PrettyPrinter
import           Simplytyped
import           Parse
---------------------
--- Interpreter
---------------------

main :: IO ()
main = runInputT defaultSettings main'

main' :: InputT IO ()
main' = do
  args <- lift getArgs
  readevalprint args (S True "" 0 [] [])

iname :: String
iname = "cálculo lambda simplemente tipado"

ioExceptionCatcher :: IOException -> IO (Maybe a)
ioExceptionCatcher _ = return Nothing

data State = S
  { inter :: Bool
  ,       -- True, si estamos en modo interactivo.
    lfile :: String
  , nq :: Int -- Numero de queries.
  ,     -- Ultimo archivo cargado (para hacer "reload")
    ve    :: NameEnv Table TableType  -- Entorno con variables globales y su valor  [(Name, (Value, Type))]
  , lv    :: NameEnv Table TableType -- Entorno con variables locales y su valor [(Name, (Value, Type))]
  }

iprompt :: State -> String
iprompt st = "GV:" ++ show (length (ve st)) ++ "|LV:" ++ show (length (lv st)) ++  "> "

--  read-eval-print loop
readevalprint :: [String] -> State -> InputT IO ()
readevalprint args state@(S inter lfile nq ve lv) =
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
              | Browse
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
handleCommand state@(S inter lfile nq ve lv) cmd = case cmd of
  Quit   -> lift $ when (not inter) (putStrLn "!@#$^&*") >> return Nothing
  Noop   -> return (Just state)
  Help   -> lift $ putStr (helpTxt commands) >> return (Just state)
  Browse -> lift $ do
    putStrLn "Global variables:"
    putStr (unlines [ s | s <- reverse (nub (map (\v -> (' ':fst v)) ve)) ])
    putStrLn "Local variables:"
    putStr (unlines [ s | s <- reverse (nub (map (\v -> (' ':fst v)) lv)) ])
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
    then lift $ putStrLn "No hay un archivo cargado.\n" >> return (Just state)
    else handleCommand state (Compile (CompileFile lfile))
  FindType s -> do
    x' <- parseIO "<interactive>" term_parse s
    t  <- case x' of
      Nothing -> return $ Left "Error en el parsing."
      Just x  -> return $ infer ve lv $ conversion $ x
    case t of
      Left  err -> lift (putStrLn ("Error de tipos: " ++ err)) >> return ()
      Right t'  -> lift $ putStrLn $ render $ printType t'
    return (Just state)

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands =
  [ Cmd [":browse"] "" (const Browse) "Ver los nombres en scope"
  , Cmd [":load"]
        "<file>"
        (Compile . CompileFile)
        "Cargar un programa desde un archivo"
  , Cmd [":print"] "<exp>" Print "Imprime un término y sus ASTs"
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
    ++ "<expr>                  evaluar la expresión\n"
    ++ "def <var> = <expr>      definir una variable\n"
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
compileFile state@(S inter lfile nq v lv) f = do
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
        Def x (_, e) -> "def " ++ x ++ " = "-- ++ render (printTerm e)
        Assign x (_, e) -> x ++ " -> " ++ render (printTerm e)
        Eval (d, e) ->
          "TableTerm AST:\n"
            ++ show d
            ++ "\n\nTerm AST:\n"
            ++ show e
            ++ "\n\nSe muestra como:\n"
            ++ render (printTerm e)
  putStrLn outtext

parseIO :: String -> (String -> ParseResult a) -> String -> InputT IO (Maybe a)
parseIO f p x = lift $ case p x of
  Failed e -> do
    putStrLn (f ++ ": " ++ e)
    return Nothing
  Ok r -> return (Just r)

handleStmt :: State -> Stmt TableTerm -> InputT IO State
handleStmt state stmt = lift $ do
  _ <- when (not (inter state)) $ do putStrLn $ "> Query " ++ show ((nq state) + 1)  ++ "." 
  st <- case stmt of
    ImportDB d    -> checkTypeConn d
    ImportCSV f v -> checkTypeImpCsv f v
    ExportCSV v f -> checkTypeExpCsv v f
    Def x e       -> if isUpper (head x) then checkType x (conversion e) 0 else putStrLn "Nombre de variable invalido" >> return state
    Assign x e    -> if isUpper (head x) then putStrLn "Nombre de variable invalido" >> return state else checkType x (conversion e) 1
    Eval e        -> checkType it (conversion e) 0
  if (not (inter st)) then return st { nq = (nq st) + 1 }
                         else return st
 where
  checkType i t a = do
    case infer (ve state) (lv state) t of
      Left  err -> putStrLn ("Error de tipos: " ++ err) >> return state
      Right ty  -> checkEval i t ty a
  checkEval i t ty a = do
    let v = eval (ve state) (lv state) t
    _ <- when (a /= 1) $ do
      let outtext =
            if i == it then render (printTable v) else render (text i)
      putStrLn outtext 
    if a == 1  then return (state { lv = (i, (v, ty)) : lv state })
    else if (i /= it) then return (state { ve = (i, (v, ty)) : ve state, lv = [] })
                      else return (state {lv = []})
  checkTypeConn d = do
    case inferConn d of
      Left  err -> putStrLn ("Error: " ++ err) >> return state
      Right ty  -> checkEvalConn ty 
  checkEvalConn ty = do
    v <- evalConn ty (ve state)
    let outtext = case v of
                    Right _ -> "Dataset cargado"
                    Left ex -> show ex
    putStrLn outtext
    let newst = case v of
                  Right new -> new
                  _ -> []
    return (state { ve = newst})
  checkTypeImpCsv f s = do
    case inferFile (ve state) f s of
      Left err -> putStrLn ("Error: " ++ err) >> return state
      Right f' -> checkEvalImpCsv f' s 
  checkEvalImpCsv f n = do
    v <- evalFile f n (ve state)
    let outtext = case v of
                    Right ts -> "Tabla cargada: " ++ n
                    Left err -> show err
    putStrLn outtext
    let newst = case v of
                  Right new -> new
                  _ -> []
    return (state { ve = newst ++ ve state})
  checkTypeExpCsv v f = do
    case inferExpCsv (ve state) v f of
      Left err -> putStrLn ("Error: " ++ err) >> return state
      Right f' -> checkEvalExpCsv f' v
  checkEvalExpCsv f v = do
    v <- evalExpCsv v f (ve state)
    let outtext = case v of
                    Right _ -> "Archivo creado con exito."
                    Left err -> show err
    putStrLn outtext
    return state
    

prelude :: String
prelude = "Ejemplos/Prelude.arsql"

it :: String
it = "it"


