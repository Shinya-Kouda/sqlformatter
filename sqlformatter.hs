import Data.Char
--TODO: set formatSQL argument to String list (input file names, output file names)  
formatSQL :: IO () 
formatSQL = do
    str <- readFile "before.sql"
    let str2 = (modifier.lastSpaceRemove.headSpaceRemove.lister) [(bigger.spacer.flatter.headSpace.(map toLower)) str]
    writeFile "after.sql" (removeSpaceLine (former str2 0 [0]))
    
------------------------------------------------------------------------    
--functions


headSpace :: [Char] -> [Char]
headSpace x = ' ':x


flatter :: [Char] -> [Char]
flatter [] = []
flatter x@(x1:xs)
--TODO: commentout escape
    | x1 == '\n' = ' ' : (flatter xs)
    | x1 == '\t' = ' ' : (flatter xs)
    | x1 == ',' = " , " ++ (flatter xs)
    | x1 == '(' = " ( " ++ (flatter xs)
    | x1 == ')' = " ) " ++ (flatter xs)
    | x1 == '=' = " = " ++ (flatter xs)
    | otherwise = x1 : (flatter xs)


spacer :: [Char] -> [Char]
spacer [] = []
spacer x@(x1:xs)
    | take 2 x == "  " = spacer xs
    | otherwise = x1 : (spacer xs)


bigger :: [Char] -> [Char]
bigger [] = []
bigger x@(x1:xs)
--TODO: Built-in functions
    | take  8 x == " select "           = " SELECT"           ++ bigger (drop  7 x)
    | take  6 x == " from "             = " FROM"             ++ bigger (drop  5 x)
    | take  7 x == " where "            = " WHERE"            ++ bigger (drop  6 x)
    | take 10 x == " group by "         = " GROUP BY"         ++ bigger (drop  9 x)
    | take  8 x == " having "           = " HAVING"           ++ bigger (drop  7 x)
    | take 12 x == " inner join "       = " INNER JOIN"       ++ bigger (drop 11 x)
    | take 11 x == " left join "        = " LEFT JOIN"        ++ bigger (drop 10 x)
    | take 12 x == " right join "       = " RIGHT JOIN"       ++ bigger (drop 11 x)
    | take 17 x == " left outer join "  = " LEFT OUTER JOIN"  ++ bigger (drop 16 x)
    | take 18 x == " right outer join " = " RIGHT OUTER JOIN" ++ bigger (drop 17 x)
    | take 17 x == " full outer join "  = " FULL OUTER JOIN"  ++ bigger (drop 16 x)
    | take 12 x == " cross join "       = " CROSS JOIN"       ++ bigger (drop 11 x)
    | take 11 x == " case when "        = " CASE WHEN"        ++ bigger (drop 10 x)
    | take  6 x == " when "             = " WHEN"             ++ bigger (drop  5 x)
    | take  6 x == " then "             = " THEN"             ++ bigger (drop  5 x)
    | take  6 x == " else "             = " ELSE"             ++ bigger (drop  5 x)
    | take  5 x == " end "              = " END"              ++ bigger (drop  4 x)
    | take  4 x == " on "               = " ON"               ++ bigger (drop  3 x)
    | take  5 x == " and "              = " AND"              ++ bigger (drop  4 x)
    | take  4 x == " or "               = " OR"               ++ bigger (drop  3 x)
    | take  4 x == " as "               = " AS"               ++ bigger (drop  3 x)
    | otherwise = x1 : bigger xs


lister :: [[Char]] -> [[Char]]
lister [[]] = [[]]
lister [x@(x1:xs)]
    | length x <= 2 = [x]
    | x1 /= ' ' && head xs == ' ' = [[x1]] ++ (lister [tail xs])
    | otherwise = [x1:(head(lister [xs]))] ++ tail(lister [xs])


headSpaceRemove :: [[Char]] -> [[Char]]
headSpaceRemove x
    | head (head x) == ' ' = (tail (head x)) : (tail x)
    | otherwise = x


lastSpaceRemove :: [[Char]] -> [[Char]]
lastSpaceRemove x
    | last (last x) == ' ' = (init x) ++ [(init (last x))]
    | otherwise = x


modifier :: [[Char]] -> [[Char]]
modifier [] = []
modifier x@(x1:xs)
    | take 2 x == ["GROUP","BY"]           = "GROUP BY"         : modifier (drop 2 x)
    | take 2 x == ["INNER","JOIN"]         = "INNER JOIN"       : modifier (drop 2 x)
    | take 2 x == ["LEFT","JOIN"]          = "LEFT JOIN"        : modifier (drop 2 x)
    | take 2 x == ["RIGHT","JOIN"]         = "RIGHT JOIN"       : modifier (drop 2 x)
    | take 3 x == ["LEFT","OUTER","JOIN"]  = "LEFT OUTER JOIN"  : modifier (drop 3 x)
    | take 3 x == ["RIGHT","OUTER","JOIN"] = "RIGHT OUTER JOIN" : modifier (drop 3 x)
    | take 3 x == ["FULL","OUTER","JOIN"]  = "FULL OUTER JOIN"  : modifier (drop 3 x)
    | take 2 x == ["CROSS","JOIN"]         = "CROSS JOIN"       : modifier (drop 2 x)
    | otherwise = x1 : (modifier xs)


former :: [[Char]] -> Int -> [Int] -> [Char]
former [] _ _ = []
former _ _ [] = []
former x@(x1:xs) indent depths
    | x1 == "SELECT" =               "\n" ++ (space level) ++      "SELECT" ++           "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "FROM" =                 "\n" ++ (space level) ++      "FROM" ++             "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "WHERE" =                "\n" ++ (space level) ++      "WHERE" ++            "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "GROUP BY" =             "\n" ++ (space level) ++      "GROUP BY" ++         "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "CASE" =             "CASE\n" ++ (space (level+2)) ++  "WHEN " ++                                         (former (tail xs) (level+2) depths)
    | x1 == "WHEN" =                 "\n" ++ (space (level+2)) ++  "WHEN " ++                                         (former xs (level+2) depths)
    | x1 == "ELSE" =                 "\n" ++ (space (level+2)) ++  "ELSE " ++                                         (former xs (level+2) depths)
    | x1 == "END" =                  "\n" ++ (space (level+1)) ++  "END " ++                                          (former xs (level+1) depths)
    | x1 == "," =                    "\n" ++ (space level) ++      ",   " ++                                          (former xs level depths)
    | x1 == "(" =                   "(\n" ++ (space (indent+1)) ++                                                    (former xs (indent+1) (depths ++ [indent+1]))
    | x1 == ")" =                    "\n" ++ (space (level-1)) ++  ") " ++                                            (former xs (level-1) (init depths))
    | x1 == "INNER JOIN" =           "\n" ++ (space (level+1)) ++  "INNER JOIN" ++       "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "LEFT JOIN" =            "\n" ++ (space (level+1)) ++  "LEFT JOIN" ++        "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "RIGHT JOIN" =           "\n" ++ (space (level+1)) ++  "RIGHT JOIN" ++       "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "LEFT OUTER JOIN" =      "\n" ++ (space (level+1)) ++  "LEFT OUTER JOIN" ++  "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "RIGHT OUTER JOIN" =     "\n" ++ (space (level+1)) ++  "RIGHT OUTER JOIN" ++ "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "FULL OUTER JOIN" =      "\n" ++ (space (level+1)) ++  "FULL OUTER JOIN" ++  "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "CROSS JOIN" =           "\n" ++ (space (level+1)) ++  "CROSS JOIN" ++       "\n" ++ (space (level+1)) ++ (former xs (level+1) depths)
    | x1 == "ON" =                   "\n" ++ (space (level+1)) ++  "ON" ++               "\n" ++ (space (level+2)) ++ (former xs (level+2) depths)
    | x1 == "AND" =                  "\n" ++ (space indent) ++     "AND" ++              "\n" ++ (space indent)    ++ (former xs indent depths)
    | x1 == "OR" =                   "\n" ++ (space indent) ++     "OR" ++               "\n" ++ (space indent)    ++ (former xs indent depths)
    | otherwise = x1 ++ " " ++ (former xs indent depths)
    where
        level = last depths
        space n = replicate (4*n) ' '


removeSpaceLine :: [Char] -> [Char]
removeSpaceLine [] = []
removeSpaceLine x@(x1:xs)
    | x1 == '\n' && head(dropWhile (==' ') xs) == '\n' = (removeSpaceLine.(dropWhile (==' '))) xs
    | otherwise = x1 : removeSpaceLine xs