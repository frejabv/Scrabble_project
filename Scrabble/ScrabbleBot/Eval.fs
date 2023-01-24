// Insert your updated Eval.fs file here from Assignment 7. All modules must be internal.

module internal Eval

    open StateMonad

    let add a b = 
        a >>= fun x -> 
        b >>= fun y ->
        ret ((+) x y)      
    let div a b = 
        a >>= fun x ->
        b >>= fun y ->
        if y = 0 then fail DivisionByZero else ret ((/) x y)
        
    let binop f a b = 
        a >>= fun x ->
        b >>= fun y ->
        ret (f x y)

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       (*IsLetter and IsDigit should maybe be deleted?*)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)
       
       | IsConsonant of cExp  (* check for constant*)
    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        match a with 
        | N x -> ret x
        | V x -> lookup x
        | WL -> wordLength
        | PV x -> arithEval x >>= pointValue
        | Add (a,b) -> add (arithEval a) (arithEval b)
        | Sub (a,b) -> binop (-) (arithEval a) (arithEval b)
        | Mul (a,b) -> binop (*) (arithEval a) (arithEval b)
        | Div (a,b) -> div (arithEval a) (arithEval b)
        | Mod (a,b) -> 
            arithEval a >>= fun x ->
                arithEval b >>= fun y ->
                    if y = 0 then fail DivisionByZero else ret ((%) x y)
        | CharToInt (x:cExp) -> charEval x >>= fun x' -> ret(int (x'))
    and  charEval c : SM<char> = 
        match c with
        | C x -> ret x
        | ToUpper x -> (charEval x >>= fun x' -> ret (System.Char.ToUpper x'))  
        | ToLower x -> (charEval x >>= fun x' -> ret (System.Char.ToLower x'))
        | CV x -> arithEval x >>= characterValue
        | IntToChar (x : aExp) -> arithEval x >>= fun x' -> ret(char (x'))         

    let isVowel c = List.exists (fun y -> System.Char.ToLower c = y) ['a';'e';'i';'o';'u'];; 

    let rec boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a, b) -> arithEval a >>= fun x' -> arithEval b >>= fun y' -> ret(x' = y') 
        | ALt (a, b) -> arithEval a >>= fun x' -> arithEval b >>= fun y' -> ret(x' < y') 
        | Not x -> (boolEval x >>= fun x' -> ret (not x')) 
        | Conj (a, b) -> (boolEval a >>= fun x' -> boolEval b >>= fun y' -> ret(x' && y')) 
        | IsDigit x -> (charEval x >>= fun x' -> ret (System.Char.IsDigit x'))
        | IsLetter x -> (charEval x >>= fun x' -> ret (System.Char.IsLetter x'))
        | IsVowel x -> (charEval x >>= fun x' -> ret (isVowel x'));;


    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = 
        match stmnt with
        | Declare x -> declare x
        | Ass (x, a) ->  
            arithEval a >>= fun v -> update x v
        | Skip -> ret ()
        | Seq (stm1, stm2) -> 
            stmntEval stm1 >>>= stmntEval stm2 
        | ITE (guard, stm1, stm2) ->  
            push >>>= (boolEval guard >>= fun b' -> if b' then stmntEval stm1 else stmntEval stm2) >>>= pop
        | While (guard, stm) -> 
            push >>>= (boolEval guard >>= fun b' -> if b' then stmntEval stm >>>= stmntEval (While (guard, stm)) else ret ()) >>>= pop

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> Result<int, Error>

    let stmntToSquareFun stm = fun w pos acc -> 
        let state = (mkState [("_pos_", pos); ("_acc_", acc); ("_result_", 0)] w ["_pos_"; "_acc_"; "_result_"])
        stmntEval stm >>>= lookup "_result_" |> evalSM state

    //type coord = int * int

    //type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun (stm : stm) m  = 
        fun (x, y) -> 
            let state = mkState [("_x_", x); ("_y_", y); ("_result_", 0)] [] ["_x_"; "_y_"; "_result_"]
            stmntEval stm 
            >>>= lookup "_result_" 
            >>= (fun id -> Map.tryFind id m |> ret) |> 

            evalSM state 

    (*type board = {
        center        : coord
        defaultSquare : squareFun
        squares       : boardFun
    }*)

    //let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    (*From Parser.fs
    let mkBoard : boardProg -> board = fun (bp : boardProg) ->
        let m' : Map<int, square> = Map.map (fun x y -> parseSquareFun y) bp.squares
        {
            center = bp.center
            defaultSquare = Map.find bp.usedSquare m'
            squares = parseBoardFun bp.prog m'
        }*)