:r

let listParser = (:[]) <$> char 'b'

let abParser = (:) <$> char 'a' <*> listParser 

{-runParser  abParser  "abmir"-}
runParser abParser "abmir"

runParser (oneOrMore (satisfy isUpper)) "AMir"
runParser (zeroOrMore (satisfy isUpper)) "ABCamir"
runParser (oneOrMore (satisfy isUpper)) "aMir"
runParser (zeroOrMore (satisfy isUpper)) "aBCamir"

runParser ident "foobar baz"
runParser ident "foo33fA"

runParser ident "2bad"
runParser ident ""

runParser parseAtom "23"
runParser parseAtom "foo"

runParser parseSExpr "23"
runParser parseSExpr "(foo)"

runParser parseSExpr "( 23)"
runParser parseSExpr	 "(bar (foo) 3 5 874)"
