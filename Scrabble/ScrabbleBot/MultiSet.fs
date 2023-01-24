// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32>

    let empty = M Map.empty ;;
    let isEmpty (M s) = Map.isEmpty s;;
    let size (M s) = s |> Map.fold (fun state key value -> state + value)  0u;; 
    let contains a (M s) = Map.containsKey a s;;
    let numItems a (M s) = if Map.tryFind a s <> None then Map.find a s else 0u;;
    let add a (n:uint32) (M s) = 
                                    if Map.tryFind a s <> None 
                                    then 
                                        let curVal = Map.find a s  
                                        M (Map.add a ((uint32 curVal) + (uint32 n)) s)
                                    else M (Map.add a (uint32 n) s);;
    let addSingle a s = add a 1u s;;
    let remove a (n:uint32) (M s) = if Map.tryFind a s <> None 
                                                then 
                                                    let curVal = Map.find a s
                                                    if (curVal <= (uint32 n)) 
                                                    then M (Map.remove a s)
                                                    else M (Map.add a ((uint32 curVal)-(uint32 n)) s)
                                                else M s;; 
    let removeSingle a s = remove a 1u s;;
    let fold f acc (M s) = Map.fold f acc s;;
    let foldBack f (M s) acc = Map.foldBack f s acc;;
