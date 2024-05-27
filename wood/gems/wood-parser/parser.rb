__END__

(+++)   :: Parser a -> Parser a -> Parser a
string  :: String -> Parser String
many    :: Parser a -> Parser [a]
many1   :: Parser a -> Parser [a]
sepby   :: Parser a -> Parser b -> Parser [a]
sepby1  :: Parser a -> Parser b -> Parser [a]
chainl  :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
space   :: Parser String
token   :: Parser a -> Parser a
symb    :: String -> Parser String
apply   :: Parser a -> String -> [(a,String)]

return a >>= f = f a
p >>= return = p
p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g
p >>= (\a -> f a ++ g a) = (p >>= f) ++ (p >>= g)

expr ::= expr addop term | term
term ::= term mulop factor | factor
factor ::= digit | ( expr )
digit ::= 0 | 1 | ... | 9
addop ::= + | -
mulop ::= * | /

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)
expr = term ‘chainl1‘ addop
term = factor ‘chainl1‘ mulop
factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}
digit = do {x <- token (sat isDigit); return (ord x - ord ’0’)}
addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}
mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}
