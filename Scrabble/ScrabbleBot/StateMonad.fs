// Insert your StateMonad.fs from Assignment 6 here. All modules must be internal.


module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = 
        S (fun s -> Success ((), {s with vars = s.vars.[1..]}))    

    let wordLength : SM<int> = S (fun s -> Success (s.word.Length,s))     

    let characterValue (pos : int) : SM<char> = 
        S (fun s -> if pos < s.word.Length && pos >= 0 then Success (fst s.word.[pos], s) else Failure (IndexOutOfBounds pos))   

    let pointValue (pos : int) : SM<int> = 
        S (fun s -> if pos < s.word.Length && pos >= 0 then Success (snd s.word.[pos], s) else Failure (IndexOutOfBounds pos))      

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        let rec aux s =
            match s with
            | s when Seq.exists (fun x -> x = var) s.reserved -> Failure (ReservedName var)
            | s when Map.tryFind var s.vars.[0] <> None -> Failure (VarExists var)
            | s when s.vars.IsEmpty -> Failure (VarNotFound var)
            | s -> Success ((), {s with vars = s.vars.Head.Add(var,0)::s.vars.Tail})
        S (fun s -> aux s)  
    
    let update (var : string) (value : int) : SM<unit> = 
        let rec aux head s =
            function
            | [] -> Failure(VarNotFound var)
            | m :: ms ->
                match Map.tryFind var m with
                | Some v ->
                    let newList =
                        List.rev head @ (Map.add var value m :: ms)

                    Success((), { s with vars = newList })
                | None -> aux (m :: head) s ms

        S(fun s -> aux [] s s.vars)