{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Def
%name parseStmts Defs
%name term Exp

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '|*|'    { TPNat }
    '*'      { TPCart }
    'I'      { TInt } 
    '/'      { TDiv } 
    'U'      { TUni } 
    '-'      { TDiff } 
    '='      { TEquals }
    '>='     { TGreq }
    '<='     { TLreq }
    '>'      { TGr }
    '<'      { TLr }
    '&'      { TAnd }
    '|'      { TOr }
    '.'      { TDot }
    '('      { TOpen }
    ')'      { TClose }
    '['      { TOpenB }
    ']'      { TCloseB }
    '->'     { TArrow }
    '"'      { TQuote }
    CSV      { TCsv }
    AS       { TAs }
    CONNECT  { TConnect }
    DEF      { TDef }
    STRING   { TString $$ }
    VAR      { TVar $$ }
    NUM      { TNum $$ }
    SEL      { TSelect }
    PROY     { TProy }
    REN      { TRen }
    HOST     { THost }
    DB       { TDb }
    PORT     { TPort }
    USER     { TUser }
    PW       { TPw }

%right VAR NUM
%left '->'
%left '/'
%left 'U' 'I'
%left '-'
%left '=' '&' '|' '*' '|*|' 
%right '->'
%right PROY SELECT


%% 

Def     :  Defexp                      { $1 }
        |  Assign	               { $1 }
        |  CONNECT '[' ConnWords ']'   { Connect $3 }
        |  CSV VAR AS VAR              { Csv $2 $4 }
        |  Exp	                       { Eval $1 }
Defexp  :  DEF VAR '=' Exp             { Def $2 $4 }
Assign : VAR '->' Exp  { Assign $1 $3 }
        


Exp     :: { TableTerm }
        : SEL '[' ExpCond ']' '(' Exp ')' { LSel $3 $6}
        | PROY '[' Words ']' '(' Exp ')'  { LProy $3 $6}
        | REN '[' VAR ']' '(' Exp ')'     { LRen $3 $6}
        | VAR                             { LTableVar $1 }
        | Exp '*' Exp                     { LPCart $1 $3 }
        | Exp '|*|' Exp                   { LPNat $1 $3 }
        | Exp '-' Exp                     { LDiff $1 $3 }
        | Exp 'U' Exp                     { LUni $1 $3 }
        | Exp 'I' Exp                     { LInt $1 $3 }
        | Exp '/' Exp                     { LDiv $1 $3 }
        | '(' Exp ')'                     { $2 }

ConnWords :: { ConnWords }
          : ConnWord                 { [$1] }  
          | ConnWord '|' ConnWords   { $1 : $3 }


ConnWord :: { ConnWord }
         : HOST Atom    { LHost $2 }
         | DB Atom      { LDb $2   }
         | PORT Atom    { LPort $2 }
         | USER Atom    { LUser $2 }
         | PW Atom      { LPw $2   }


Words :: { TableCols }
      : Atom                           { [$1] }
      | Atom '|' Words               { $1 : $3 }

ExpCond :: { TableCond }
        : TermCond                { $1 }
        | ExpCond '&' ExpCond     { LAnd $1 $3 }
        | ExpCond '|' ExpCond     { LOr $1 $3 }

TermCond :: { TableCond }
        : Atom '=' Atom           { LEquals $1 $3 }
        | Atom '<' Atom           { LLr $1 $3 }
        | Atom '>' Atom           { LGr $1 $3 }
        | Atom '<=' Atom          { LLrEq $1 $3 }
        | Atom '>=' Atom          { LGrEq $1 $3 }

Atom :: { TableAtom }
     : NUM         { LNum $1 }
     | STRING  { LString $1 }
     | VAR         { LVar $1 }

Defs    : Def Defs                  { $1 : $2 }
        |                              { [] }

{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)


data Token = TVar String
               | TNum String
               | TString String
               | TPNat
               | TPCart
               | TInt
               | TUni
               | TConnect
               | TCsv
               | TAs
               | TDiff
               | TDiv
               | TSelect
               | TProy
               | TRen
               | TDot
               | TQuote
               | TDef
               | TComma
               | TOpen
               | TOpenB
               | TClose 
               | TCloseB 
               | TColon
               | TArrow
               | TEquals
               | TGreq
               | TLreq
               | TGr
               | TLr
               | TAnd
               | TOr
               | TEOF
               | THost
               | TDb
               | TUser
               | TPw
               | TPort
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isDigit c -> lexNum (c:cs)
                          | isAlpha c -> lexVar (c:cs)
                    ('"':cs) -> lexString cs
                    ('-':'-':cs) -> lexer cont $ dropWhile ((/=) '\n') cs
                    ('{':'-':cs) -> consumirBK 0 0 cont cs	
                    ('-':'}':cs) -> \ line -> Failed $ "Línea "++(show line)++": Comentario no abierto"
                    ('-':'>':cs) -> cont TArrow cs
                    ('.':cs) -> cont TDot cs
                    ('(':cs) -> cont TOpen cs
                    ('[':cs) -> cont TOpenB cs
                    (')':cs) -> cont TClose cs
                    (']':cs) -> cont TCloseB cs
                    ('|':'*':'|':cs) -> cont TPNat cs
                    ('*':cs) -> cont TPCart cs
                    ('-':cs) -> cont TDiff cs
                    ('/':cs) -> cont TDiv cs
                    ('&':cs) -> cont TAnd cs
                    ('|':cs) -> cont TOr cs
                    ('<':'=':cs) -> cont TLreq cs
                    ('>':'=':cs) -> cont TGreq cs
                    ('=':cs) -> cont TEquals cs
                    ('>':cs) -> cont TGr cs
                    ('<':cs) -> cont TLr cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlpha' cs of
                              ("ht", (':':rest)) -> cont THost rest
                              ("db", (':':rest)) -> cont TDb rest
                              ("us", (':':rest)) -> cont TUser rest
                              ("pw", (':':rest)) -> cont TPw rest
                              ("pt", (':':rest)) -> cont TPort rest
                              ("as",rest) -> cont TAs rest
                              ("csv",rest) -> cont TCsv rest
                              ("connect",rest) -> cont TConnect rest
                              ("def", rest) -> cont TDef rest
                              ("S",rest)   -> cont TSelect rest
                              ("P", rest)   -> cont TProy rest
                              ("R", rest)   -> cont TRen rest
                              ("U", rest)   -> cont TUni rest
                              ("I", rest)   -> cont TInt rest
                              (var,rest)     -> cont (TVar var) rest
                          consumirBK anidado cl cont s = case s of
                              ('-':('-':cs)) -> consumirBK anidado cl cont $ dropWhile ((/=) '\n') cs
                              ('{':('-':cs)) -> consumirBK (anidado+1) cl cont cs	
                              ('-':('}':cs)) -> case anidado of
                                                  0 -> \line -> lexer cont cs (line+cl)
                                                  _ -> consumirBK (anidado-1) cl cont cs
                              ('\n':cs) -> consumirBK anidado (cl+1) cont cs
                              (_:cs) -> consumirBK anidado cl cont cs
                          lexNum cs = case span isDigit cs of
                              (num,rest)     -> cont (TNum num) rest
                          lexString cs = case span isNotQuote cs of
                                (str, ('"':rest)) -> cont (TString str) rest
                                (str, rest) -> cont (TString str) rest 
                                where isNotQuote c = c /= '"' 
                          isAlpha' c = isAlpha c || c == '.' || c == '_' || c == '-' || c == '/'

                           
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
term_parse s = term s 1
}
