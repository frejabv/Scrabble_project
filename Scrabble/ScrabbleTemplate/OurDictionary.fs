module Dictionary
    type Dictionary = 
        | Node of bool * Map<char, Dictionary>
    
    let empty () = Node(false, Map.empty);;

    let rec insert (s : string) (Node(b,map)) = 
        match s with 
        | "" -> Node (true,map)  
        | s -> 
            let c = s.[0]
            match Map.tryFind c map with
            | Some x -> 
                let newMap = Map.add c (insert (s.[1..]) x) map
                Node(b, newMap)
            | None -> 
                let newMap = Map.add c (insert (s.[1..]) (empty())) map
                Node(b, newMap)  
    
    let rec lookup (s :string) (Node(b,map)) = 
        match s with
        | "" -> b 
        | s -> 
            match Map.tryFind s.[0] map with
            | Some x -> lookup s.[1..] x 
            | None -> false 
    
    let step (c:char) (Node(b,map)) = 
        match Map.tryFind c map with
        | Some (Node(bo,dict')) -> Some (bo, Node(bo,dict')) 
        | None -> None