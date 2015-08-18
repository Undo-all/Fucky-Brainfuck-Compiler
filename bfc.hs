data Expr = Add Int
          | Move Int
          | Print
          | Read
          | Zero
          | Open
          | Close 
          | ScanLeft
          | ScanRight deriving (Eq, Show)

parseGroup :: (Int -> Expr) -> String -> Int -> Char -> Char -> [Expr]
parseGroup f [] n o c = [f n]
parseGroup f (x:xs) n o c
    | x == o    = parseGroup f xs (n + 1) o c
    | x == c    = parseGroup f xs (n - 1) o c
    | otherwise = f n : parse (x:xs)

parse :: String -> [Expr]
parse []       = []
parse ('+':xs) = parseGroup Add xs 1 '+' '-'
parse ('-':xs) = parseGroup Add xs (-1) '+' '-'
parse ('>':xs) = parseGroup Move xs 1 '>' '<'
parse ('<':xs) = parseGroup Move xs (-1) '>' '<'
parse ('.':xs) = Print : parse xs
parse (',':xs) = Read : parse xs
-- Some optimization
parse ('[':'-':']':xs) = Zero : parse xs
parse ('[':'<':']':xs) = ScanLeft : parse xs
parse ('[':'>':']':xs) = ScanRight : parse xs
parse ('[':xs) = Open : parse xs
parse (']':xs) = Close : parse xs
parse (_:xs)   = parse xs

compileExpr :: Expr -> String
compileExpr (Add n)   = " a " ++ show n
compileExpr (Move n)  = " b " ++ show n
compileExpr Print     = " c"
compileExpr Read      = " d"
compileExpr Zero      = " e"
compileExpr Open      = " f"
compileExpr Close     = " g"
compileExpr ScanLeft  = " h"
compileExpr ScanRight = " i"

compile :: String -> String
compile xs =
    "#include <stdio.h>\n" ++
    "#include <string.h>\n" ++
    "#define a ;m[p]+=\n" ++
    "#define b ;p+=\n" ++
    "#define c ;putchar(m[p])\n" ++
    "#define d ;scanf(\"%c\", &m[p])\n" ++
    "#define e ;m[p]=0\n" ++
    "#define f ;while(m[p]){\n" ++
    "#define g ;}\n" ++
    "#define h ;p-=(long)((void*)(m+p)-memrchr(m,0,p+1))\n" ++
    "#define i ;p+=(long)(memchr(m+p,0,sizeof(m))-(void*)(m+p))\n" ++
    "main(){int p=0;int m[20000]" ++
    concat (map compileExpr (parse xs)) ++
    ";}"

main = do
    fileName <- getLine
    file <- readFile fileName
    putStr $ compile file

