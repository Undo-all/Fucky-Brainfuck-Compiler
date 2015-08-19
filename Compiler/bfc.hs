data Expr = Add Int
          | Move Int
          | Print
          | Read
          | Zero
          | Open
          | Close 
          | ScanLeft
          | ScanRight 
          | Balanced [(Int, Int)] deriving (Eq, Show)

parseGroup :: (Int -> Expr) -> String -> Int -> Char -> Char -> [Expr]
parseGroup f [] n o c = [f n]
parseGroup f (x:xs) n o c
    | x == o    = parseGroup f xs (n + 1) o c
    | x == c    = parseGroup f xs (n - 1) o c
    | otherwise = if n == 0 then parse (x:xs) else f n : parse (x:xs)

isBalanced :: String -> Bool
isBalanced xs = pure xs && foldl movement 0 xs == 0
  where pure = all (not . (`elem` ".,"))
        movement acc '>' = acc + 1
        movement acc '<' = acc - 1
        movement acc _   = acc 

--balance :: String -> Balanced
--balance =  

parse :: String -> [Expr]
parse []       = []
parse ('+':xs) = parseGroup Add xs 1 '+' '-'
parse ('-':xs) = parseGroup Add xs (-1) '+' '-'
parse ('>':xs) = parseGroup Move xs 1 '>' '<'
parse ('<':xs) = parseGroup Move xs (-1) '>' '<'
parse ('.':xs) = Print : parse xs
parse (',':xs) = Read : parse xs
-- Some simple optimization
parse ('[':'-':']':xs) = Zero : parse xs
parse ('[':'<':']':xs) = ScanLeft : parse xs
parse ('[':'>':']':xs) = ScanRight : parse xs
-- Significantly more complex
-- optimization: turn any balanced binary loop into a single iteration
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
    "#define a ;*p+=\n" ++
    "#define b ;p+=\n" ++
    "#define c ;putchar(*p)\n" ++
    "#define d ;*p=getchar()\n" ++
    "#define e ;*p=0\n" ++
    "#define f ;while(*p){\n" ++
    "#define g ;}\n" ++
    "#define h ;for(;*p!=0;--p)\n" ++
    "#define i ;for(;*p!=0;++p)\n" ++
    "main(){int m[20000];register int* p=m" ++
    concat (map compileExpr (parse xs)) ++
    ";}"

main = do
    fileName <- getLine
    file <- readFile fileName
    putStr $ compile file

