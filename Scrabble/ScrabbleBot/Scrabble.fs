namespace Adam

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO

open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player number, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.
    type state = {
        board               : Parser.board
        placedTiles         : Map<coord, char>
        dict                : ScrabbleUtil.Dictionary.Dict
        playerNumber        : uint32
        hand                : MultiSet.MultiSet<uint32>
        numPlayers          : uint32
        playersPoints       : Map<uint32, uint32>
        turn                : uint32
        forfeitedPlayers    : List<uint32>
        tiles               : Map<uint32, tile>
        direction           : bool
        tilesLeft           : uint32
    }

    let mkState b d pn h np t = {board = b; placedTiles=Map.empty; dict=d; playerNumber=pn; hand=h
                                 numPlayers=np; playersPoints=Map.empty; turn=0u; forfeitedPlayers=[]; tiles = t;direction=false; tilesLeft=(100u - (7u*np))}
    
    let board st         = st.board
    let placedTiles st   = st.placedTiles
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let numPlayers st    = st.numPlayers
    let playersPoints st = st.playersPoints
    let turn st = st.turn
    let forfeitedPlayers st = st.forfeitedPlayers
    let direction st = st.direction
    let tilesLeft st = st.tilesLeft
    
    
    let addTilesToPlacedTiles st ms =
        List.fold (fun acc (c,(_, (cv, _))) -> (Map.add c cv acc)) st.placedTiles ms
    let removeUsedFromHand st ms =
        List.fold (fun acc (_,(x,_)) -> (MultiSet.removeSingle x acc)) st.hand ms
    let addNewToHand np hand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) hand np
    let updateChangeHand newPieces st =
        {
            st with
                hand = addNewToHand newPieces MultiSet.empty
                tilesLeft= st.tilesLeft - (uint32 newPieces.Length)
        }
        
    let updateHand st ms np= removeUsedFromHand st ms |> addNewToHand np
    let updateTurn st =
        let rec incrementor num = 
            let increase = (num + 1u) % st.numPlayers
            if(List.contains increase st.forfeitedPlayers)
            then
                incrementor increase
            else
                increase
        incrementor st.turn
    
    let updateChange st numberOfTiles = {
        st with
            tilesLeft= st.tilesLeft - numberOfTiles
            turn = updateTurn st
        }
            
    let updatePlayersPoints st pid points =
        if(Map.tryFind pid st.playersPoints = None)
        then
            Map.add pid points st.playersPoints
        else 
            let existingPoints = Map.find pid st.playersPoints
            Map.add pid (existingPoints+points) st.playersPoints
    
    let updatePlaySuccess st ms np points =
        {
        st with
            hand= (updateHand st ms np)
            playersPoints=updatePlayersPoints st st.playerNumber points
            turn=updateTurn st
            placedTiles=addTilesToPlacedTiles st ms
            tilesLeft= st.tilesLeft - (uint32 ms.Length)
        }
        
    let updatePlayed st ms pid points =
        {
            st with
                playersPoints=updatePlayersPoints st pid points
                turn=updateTurn st
                placedTiles=addTilesToPlacedTiles st ms
                tilesLeft= st.tilesLeft - (uint32 ms.Length)
        }
    let updateIncreaseTurn st =
        {
            st with
                turn = updateTurn st
        }
    let updateForfeit st pid =
        {
            st with
                forfeitedPlayers = st.forfeitedPlayers@[pid]
        }
    
    let updateDirection st =
        {
            st with
                direction = not st.direction
        }
        
     
     
module Algorithm =
    
    open State
        
    //Go through placedtiles and for each see if we can place a word to the right or down
    let findPossibleWordPlacements placedTiles =
        let placedTilesList = Map.fold (fun l k _ -> l@[k]) List.empty placedTiles
        let rec aux remainingPlacedTiles accRight accDown =
            match remainingPlacedTiles with
            | (x,y)::xs ->
                //for every tile see if there is a tile to the left and above.
                let possibleWordStartRight = Map.tryFind ((x - 1), y) placedTiles = None
                let possibleWordStartDown = Map.tryFind (x, (y - 1)) placedTiles = None
                aux xs (if possibleWordStartRight then accRight@[(x,y)] else accRight) (if possibleWordStartDown then accDown@[(x,y)] else accDown)
            | _ ->
                if List.isEmpty accRight 
                then ([(0,0)],[(0,0)])
                else
                    (accRight, accDown)
        aux placedTilesList [] []
        
    
    type placedWord = (coord * (uint32 * (char * int))) list
    
    let incDownFunc (c:coord) : coord = (fst c, snd c + 1)
        
    let incRightFunc (c:coord) : coord = (fst c + 1, snd c)
    
    let downAdjacents (c:coord) : coord * coord = ((fst c - 1, snd c) , (fst c + 1, snd c))
    
    let rightAdjacents (c:coord) : coord * coord = ((fst c, snd c - 1) , (fst c, snd c + 1))
    
    let isAdjacentsTaken adjacentsFunc (c:coord) placedTiles =
        let (a,b) = adjacentsFunc c
        let aPotentialTileOnCoord = (Map.tryFind a placedTiles) <> None
        let bPotentialTileOnCoord = (Map.tryFind b placedTiles) <> None
        aPotentialTileOnCoord || bPotentialTileOnCoord
        
    let afterWordTaken (incrementedC:coord) placedTiles =
        (Map.tryFind incrementedC placedTiles) <> None
    
    let rec tryPathWithHandTile id tiles dict restOfHand placedTiles builtWord builtWordArray coord coordIncrementor usedLetters direction =
                    //Is there something at this coordinate?
                    let potentialTileOnCoord = Map.tryFind coord placedTiles
                    
                    let tileOnCoord = potentialTileOnCoord <> None
                    
                    let actualTileOnCoord =
                        match potentialTileOnCoord with
                        | Some x ->
                            x
                        | None -> '_'
                    
                    let tilesAround =
                        if direction = "right"
                        then
                            isAdjacentsTaken rightAdjacents coord placedTiles
                        else
                            isAdjacentsTaken downAdjacents coord placedTiles
                    
                    if(tilesAround)
                    then
                        []
                    else
                        let tryEveryTileInSet cv pv =
                            let goDownTrie dict' =
                                //Look further down the trie to see if it eventually is a valid word (with your yet unused tiles on hand)
                                //remove id/cv from existing hand
                                let mutable newRestOfHand = restOfHand
                                if not tileOnCoord
                                then 
                                    newRestOfHand <- MultiSet.removeSingle id restOfHand
                                //take the new first and send it as id
                                //send along the rest
                                MultiSet.fold (fun acc2 id' n ->
                                    let newWordArray = builtWordArray@[(coord, (id,(cv,pv)))]
                                    let resultWords = tryPathWithHandTile id' tiles dict' newRestOfHand placedTiles (builtWord+string(cv))
                                                          newWordArray (coordIncrementor coord) coordIncrementor (usedLetters@[id']) direction
                                    acc2@resultWords
                                ) [] newRestOfHand
                                
                            //Step through the dictionary to see if we can create a word
                            let stepResult = Dictionary.step (if tileOnCoord then actualTileOnCoord else cv) dict 
                            match stepResult with
                            | Some (bool, dict') ->
                                //if tile not on coord then you can check if it is a valid word, else if there is a tile on coord
                                //then go down the dict with this one
                                
                                if bool
                                then
                                    if tileOnCoord
                                    then
                                        //build upon existing word
                                        goDownTrie dict'
                                    else
                                        let newWordArray = builtWordArray@[(coord, (id,(cv,pv)))]
                                        //check if there is anything after this
                                        if (afterWordTaken (coordIncrementor coord) placedTiles)
                                        then
                                            []
                                        else
                                            [newWordArray]
                                else
                                    goDownTrie dict'
                            | None ->
                                //This letter is not a child of the current dict. Check another letter on the hand.
                                []
                        
                        let inputLetterSet = Map.find id tiles
                        Set.fold (fun acc (cv, pv) -> acc@(tryEveryTileInSet cv pv)) [] inputLetterSet
                        
                        
    
    let goThroughHand st dict builtWord builtWordArray coord coordIncrementor direction =
        MultiSet.fold (fun acc id n ->
            let possibleResult = tryPathWithHandTile id st.tiles dict st.hand st.placedTiles builtWord builtWordArray coord coordIncrementor [id] direction
            acc@possibleResult
        ) [] st.hand
    
    let rec aux newCurrentCoord dict st coordIncrementor direction = 
            
            match Map.tryFind newCurrentCoord st.placedTiles with
            | None ->
                // no tile on coord proceed by folding through the entire hand
                let theResult = goThroughHand st dict "" [] newCurrentCoord coordIncrementor direction
                
                // Here we have all the words. For now we take the longest
                let rec takeLongestWord wordLength index indexOfLongestWord (restOfList : (coord * (uint32 * (char * int))) list list) =
                    match restOfList with
                    | x::xs ->
                        if(x.Length > wordLength || indexOfLongestWord = -1)
                        then
                            takeLongestWord x.Length (index+1) index xs
                        else
                            takeLongestWord wordLength (index+1) indexOfLongestWord xs
                    | [] ->
                        indexOfLongestWord
                let index = (takeLongestWord 0 0 -1 theResult)
                if theResult = List.empty || index = -1
                then
                    []
                else
                    theResult[index]
            | Some cv ->
                //There is a tile on the coord, step into the dictionary with this
                let childNode = Dictionary.step cv dict
                match childNode with
                | Some (_,dict') ->
                    //if it is some we don't care because it means there is still something on the board
                    aux (coordIncrementor newCurrentCoord) dict' st coordIncrementor direction
                | None ->
                    //could not go down in dictionary with this letter
                    let theResult = goThroughHand st dict "" [] newCurrentCoord coordIncrementor direction
                    theResult[0]
                    
    let findWordFromCoord st (coordList: (int*int) list) coordIncrementor direction : placedWord =
        let everyCoord = List.fold (
            fun acc (currentCoord : coord) ->
                let result = (aux currentCoord st.dict st coordIncrementor) direction
                acc@[result]) [] coordList

        let rec longestWordFromResults listLength longestList (resultOfEveryCoordinate : (coord * (uint32 * (char * int))) list list) =
           match resultOfEveryCoordinate with
                    | x::xs ->
                        if(x.Length > listLength)
                        then
                            longestWordFromResults x.Length x xs
                        else
                            longestWordFromResults listLength longestList xs
                    | [] ->
                        longestList
        longestWordFromResults -1 [] everyCoord
        
module Scrabble =
    open System.Threading

    let playGame cstream pieces (st : State.state) =
        
        debugPrint 
            (sprintf "Player #%d: ADAM:
                      Starting game!
                      number of players = %d
                      player id = %d
                      hand =  %A
                      points = %A
                      turn = %d\n\n" st.playerNumber st.numPlayers st.playerNumber st.hand st.playersPoints st.turn)
        
        let rec aux (st : State.state) directionCounter =
            //direction counter moved from here
            
            let consoleInput = false
            let consoleInputFunc consoleInput =
                let input =  System.Console.ReadLine()
                RegEx.parseMove input
                
            Print.printHand pieces (State.hand st) 
            
            let updatedDirectionCounter =
                if (st.turn+1u) = st.playerNumber
                then
                    //TODO: We update DirectionCounter twice.
                    let updatedDirectionCounter = directionCounter + 1
                    let move =
                        if false
                        then
                            consoleInputFunc consoleInput
                        else
                            let (right, down) = Algorithm.findPossibleWordPlacements st.placedTiles 
                            let newSt = State.updateDirection st
                            let (direction,possibleStartCoordinates,directionString) =
                                if(updatedDirectionCounter % 2 = 0)
                                then
                                    (Algorithm.incRightFunc,right,"right")
                                else
                                    (Algorithm.incDownFunc,down,"down")
                            Algorithm.findWordFromCoord newSt possibleStartCoordinates direction directionString 
                   
                    if move <> List.empty
                    then
                        debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) 
                        send cstream (SMPlay move)
                    else
                        //change hand
                        let getNumberOfKeys key value =
                                let rec aux key newValue acc =
                                    match newValue with
                                    | 1u -> acc@[key]
                                    | x -> aux key (x-1u) acc@[key]
                                    
                                aux key value []
                        let convertedHand = MultiSet.fold(fun keys key value -> keys@(getNumberOfKeys key value)) [] st.hand
                        if st.tilesLeft < 7u
                        then
                            send cstream (SMChange convertedHand[..(int st.tilesLeft-1)])
                        else
                            send cstream (SMChange convertedHand)
                            
                    
                    debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move)
                    
                    updatedDirectionCounter
                else
                    //TODO: Multiplayer-Issue: as it seems like we need to send something to receive something.
                    //send cstream (SMPass)
                    directionCounter
            
            //TODO: Multiplayer-Issue: breaks if we have more than one bot and don't send a message to server
            let msg = recv cstream
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                let st' = State.updatePlaySuccess st ms newPieces (uint32 points)
                aux st' updatedDirectionCounter
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = State.updatePlayed st ms pid (uint32 points)
                aux st' updatedDirectionCounter
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = State.updateIncreaseTurn st
                aux st' updatedDirectionCounter
            | RCM (CMPassed (pid)) ->
                let st' = State.updateIncreaseTurn st
                aux st' updatedDirectionCounter
            | RCM (CMForfeit (pid)) ->
                let st' = State.updateForfeit st pid
                aux st' updatedDirectionCounter
            | RCM (CMChange (pid, numberOfTiles)) ->
                let st' = State.updateChange st numberOfTiles
                aux st' updatedDirectionCounter
            | RCM (CMChangeSuccess (newTiles)) ->
                let st' = State.updateChangeHand newTiles st
                aux st' updatedDirectionCounter
            | RCM (CMGameOver (playerScoreList)) ->
                let rec printFinalScore playerScoreList =
                    match playerScoreList with
                    | x::xs -> debugPrint (sprintf "Player #%d: %d" (fst x) (snd x)); printFinalScore xs
                    | _ -> ()
                printFinalScore playerScoreList
            | RCM a -> failwith (sprintf "Player #%d: not implemented: %A" st.playerNumber a)
            | RGPE err ->
                let rec auxi errorList =
                    match errorList with
                    | x::xs ->
                        match x with
                        | GPENotEnoughPieces (_, availableTiles) -> printf "Not enough pieces, there are only: %d left" availableTiles
                        auxi xs
                    | [] -> ""
                auxi err
                printfn "Gameplay Error:\n%A" err; aux st updatedDirectionCounter

        aux st 0

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)
            
        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        
        let board = Parser.mkBoard boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet numPlayers tiles)