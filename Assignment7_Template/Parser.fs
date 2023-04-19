module ImpParser

    open Eval

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar" <?> "intToChar"
    let pPointValue = pstring "pointValue" <?> "pointValue"

    let pCharToInt  = pstring "charToInt" <?> "charToInt"
    let pToUpper    = pstring "toUpper" <?> "toUpper"
    let pToLower    = pstring "toLower" <?> "toLower"
    let pCharValue  = pstring "charValue" <?> "charValue"

    let pTrue       = pstring "true" <?> "true"
    let pFalse      = pstring "false" <?> "false"
    let pIsDigit    = pstring "isDigit" <?> "isDigit"
    let pIsLetter   = pstring "isLetter" <?> "isLetter"
    let pIsVowel   = pstring "isVowel" <?> "isVowel"

    let pif       = pstring "if" <?> "if"
    let pthen     = pstring "then" <?> "then"
    let pelse     = pstring "else" <?> "else"
    let pwhile    = pstring "while" <?> "while"
    let pdo       = pstring "do" <?> "do"
    let pdeclare  = pstring "declare" <?> "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) a b = (a .>> spaces) .>>. b
    let (.>*>) a b   = (a .>> spaces) .>> b
    let (>*>.) a b   = (a .>> spaces) >>. b

    let surroundedBy p s e = pchar s >*>. p .>*> pchar e
    let parenthesise p = surroundedBy p '(' ')' <?> "parenthesise"

    let innerCombine (a: Parser<char * list<char>>)  =  a |>> (fun (x, xs) -> x :: xs)
    let stringconverter (lst: list<char>) = System.String.Concat(Array.ofList(lst))
    let pid = (pletter <|> pchar '_') .>>. (many (palphanumeric <|> pchar '_')) |> innerCombine |>> stringconverter <?> "Identifier"
    
    let unop op a= op >*>. a
    let binop op p1 p2 = (p1 .>*> op) .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()
    let BoolParseTop, breft = createParserForwardedToRef<bExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    do tref := choice [AddParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    do pref := choice [MulParse; AtomParse]

    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    do aref := choice [NParse; ParParse]

    let AexpParse = TermParse 

    let CexpParse = CharParse

    let BexpParse = BoolParseTop

    let stmntParse = pstring "not implemented"

(* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    type boardProg  = {
            prog       : string;
            squares    : Map<int, squareProg>
            usedSquare : int
            center     : coord
    
            isInfinite : bool   // For pretty-printing purposes only
            ppSquare   : string // For pretty-printing purposes only
        }

    type word   = (char * int) list
    type square = Map<int, squareFun>

    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    type boardFun2 = coord -> StateMonad.Result<square option, StateMonad.Error>
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    let mkBoard (bp : boardProg) = failwith "not implemented"

