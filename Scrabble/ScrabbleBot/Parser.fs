// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

// Insert your Parser.fs file here from Assignment 7. All modules must be internal.

module internal Parser
    
    open StateMonad
    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    (*Maybe delete pIsVowel?*)
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let whitespaceChar = satisfy (fun x -> System.Char.IsWhiteSpace x) <?> "whitespace"
    let pletter        = satisfy (fun x -> System.Char.IsLetter x) <?> "letter"
    let palphanumeric  = satisfy (fun x -> System.Char.IsLetterOrDigit x) <?> "alphanumeric"

    let spaces         = many whitespaceChar <?> "spaces"
    let spaces1        = many1 whitespaceChar <?> "spaces1"

    let (.>*>.) p1 p2 = (p1 .>> spaces) .>>. p2 
    let (.>*>) p1 p2 = p1 .>> spaces .>> p2 
    let (>*>.) p1 p2 = p1 .>> spaces >>. p2 

    let parenthesise p = spaces >*>. pchar '(' >*>. p .>*> pchar ')' <?> "parenthesise"

    let charListToStr charList = List.fold (fun acc c -> acc + string c) "" charList
    let pid = (pchar '_' <|> pletter) .>>. (many (pchar '_' <|> palphanumeric)) 
                |>> fun (c, tail) -> charListToStr (c::tail)

    
    let unop op a = op >*>. a <?> "unop"
    let binop op p1 p2 = p1 .>*> op .>*>. p2 <?> "binop"

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let CharParse, cref = createParserForwardedToRef<cExp>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]
    
    let CharParParse = parenthesise CharParse <?> "char parenthesise"
    let ParParse = parenthesise TermParse <?> "parenthesise"
    let NParse   = pint32 |>> N <?> "Int"
    let VParse   = pid |>> V <?> "Variable"
    let PVParse   = pPointValue >>. ParParse |>> PV <?> "PointValue"
    let CharToIntParse = pCharToInt >>. CharParParse |>> CharToInt <?> "CharToInt"
    let NegParse = pchar '-' >>. pint32 |>> fun x -> Mul (N -1, N x)
    do aref := choice [NegParse; PVParse; CharToIntParse; ParParse; VParse; NParse]

    let CParse   = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "Char"
    let CVParse = pCharValue >>. ParParse |>> CV <?> "CharValue"
    let IntToCharParse = pIntToChar >>. ParParse |>> IntToChar <?> "IntToChar"
    let ToUpper = pToUpper >>. CharParParse |>> ToUpper <?> "ToUpper"
    let ToLower = pToLower >>. CharParParse |>> ToLower <?> "ToLower"
    do cref := choice [CParse; CharParParse; ToUpper; ToLower; CVParse; IntToCharParse]
    
    let AexpParse = TermParse 
    let CexpParse = CharParse

    let ConDisParse, cdref = createParserForwardedToRef<bExp>()
    let EqualParse, eref = createParserForwardedToRef<bExp>()
    let IsParse, iref = createParserForwardedToRef<bExp>()
    
    let ConjunctionParse = binop (pstring "/\\") EqualParse ConDisParse |>> Conj <?> "Conjunction"
    let DisjunctionParse = binop (pstring "\\/") EqualParse ConDisParse |>> (fun (x, y) -> x .||. y) <?> "Disjunction" 
    do cdref := choice [ConjunctionParse; DisjunctionParse; EqualParse]
    
    let EqualityParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "Equality" 
    let NotEqualParse = binop (pstring "<>") AexpParse AexpParse |>> AEq |>> Not <?> "Not equal" 
    let LessParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "Less than"
    let LessEqualParse = binop (pstring "<=") AexpParse AexpParse |>> (fun (x, y) -> x .<=. y) <?> "Less than or equal" 
    let GreatParse = binop (pchar '>') AexpParse AexpParse |>> (fun (x,y) -> x .>. y) <?> "Greater than" 
    let GreatEqualParse = binop (pstring ">=") AexpParse AexpParse |>> ALt |>> Not <?> "Greater than or equal"
    do eref := choice [EqualityParse; NotEqualParse; LessParse; LessEqualParse; GreatParse; GreatEqualParse; IsParse] 
    
    let TrueParse = spaces >>. pTrue .>> spaces |>> (fun _ -> TT) <?> "True"
    let FalseParse = spaces >>. pFalse .>> spaces |>> (fun _ -> FF) <?> "False"
    let BoolParParse = parenthesise ConDisParse <?> "parenthesise"
    let IsDigitParse = pIsDigit >>. CharParParse |>> IsDigit <?> "Is digit"
    let IsLetterParse = pIsLetter >>. CharParParse |>> IsLetter <?> "Is letter"
    let IsVowelParse = pIsVowel >>. CharParParse |>> IsVowel <?> "Is vowel"
    let NotParse = pchar '~' >>. ConDisParse |>> Not <?> "Not"
    do iref := choice [BoolParParse; IsDigitParse; IsLetterParse; IsVowelParse; NotParse; TrueParse; FalseParse]
    
    let BexpParse = ConDisParse
    
    let SqParse, sqref = createParserForwardedToRef<stm>()
    let StmntParse, sref = createParserForwardedToRef<stm>()
    
    let SeqParse = StmntParse .>*> pchar ';' .>*>. SqParse |>> Seq <?> "Seq" 
    do sqref := choice [SeqParse; StmntParse] 

    let AssignParse = pid .>*> pstring ":=" .>*>. AexpParse |>> Ass <?> "Assign" 
    let DeclareParse = pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"
    let ITEParse = pif >>. BoolParParse .>*> pthen .>*> pchar '{' .>*>. SqParse .>*> pchar '}' .>*> pelse .>*> pchar '{' .>*>. SqParse .>*> pchar '}' |>> (fun ((x,y),z) -> ITE(x, y, z)) <?> "If then else"
    let ITParse = pif >>. BoolParParse .>*> pthen .>*> pchar '{' .>*>. SqParse .>*> pchar '}' |>> (fun (x,y) -> ITE (x, y, Skip)) <?> "If then"
    let WhileParse = pwhile >*>. BoolParParse .>*> pdo .>*> pchar '{' .>*>. SqParse .>*> pchar '}' |>> While <?> "While"
    do sref := choice [AssignParse; DeclareParse; WhileParse; ITEParse; ITParse] 
    let stmParse = SqParse

    (* The rest of your parser goes here *)

    type word   = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>
    type square = Map<int, squareFun>
    
    (*Potentially deprecated, not part of the newest scrabble template*)
    let parseSquareFun (sqp:squareProg) = Map.map (fun _ y -> getSuccess(run stmParse y) |> stmntToSquareFun) sqp 

    type boardFun2 = coord -> Result<square option, Error>
    
    (*Potentially deprecated, not part of the newest scrabble template*)
    let parseBoardFun s (sqs : Map<int, Map<int, squareFun>>) : boardFun2 = 
        stmntToBoardFun (getSuccess (run stmParse s)) sqs 
    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun2
    }

    // Default (unusable) board in case you are not implementing a parser for the DSL.
    //let mkBoard : boardProg -> board = fun _ -> {center = (0,0); defaultSquare = Map.empty; squares = fun _ -> Success (Some Map.empty)}
    let mkBoard : boardProg -> board = fun (bp : boardProg) ->
        let m' : Map<int, square> = Map.map (fun x y -> parseSquareFun y) bp.squares
        {
            center = bp.center
            defaultSquare = Map.find bp.usedSquare m'
            squares = parseBoardFun bp.prog m'
        }

