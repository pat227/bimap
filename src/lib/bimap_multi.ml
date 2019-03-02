module Bimap_multi(MapModule1 : Map.S)(MapModule2 : Map.S) = struct 
  let forward_map = ref MapModule1.empty;;
  let reverse_map = ref MapModule2.empty;;
  (*empty, union, merge, map, etc, all mutate the maps, ie, the functions 
    do not return maps unlike counterparts in map.mli*)
  let get_forward_map () =
    let newmap = ref MapModule1.empty in 
    let () = MapModule1.iter
               (fun key values -> newmap := MapModule1.add key values !newmap) !forward_map in
    !newmap;;
  let get_reverse_map () =
    let newmap = ref MapModule2.empty in 
    let () = MapModule2.iter
               (fun key values -> newmap := MapModule2.add key values !newmap) !reverse_map in
    !newmap;;
  let empty () =
    let () = forward_map := MapModule1.empty in
    reverse_map := MapModule2.empty
  let is_empty () =
    MapModule1.is_empty !forward_map
  let is_empty_reverse () =
    MapModule2.is_empty !reverse_map
  let mem ~key =
    MapModule1.mem key !forward_map
  let mem_reverse ~key =
    MapModule2.mem key !reverse_map
  (*keep private*)
  let mem_data_of_fwd_map ~key ~data =
    let forward_list = MapModule1.find key !forward_map in
    if List.length forward_list > 0 then
      if List.mem data forward_list then true else false
    else false
  (*keep private*)
  let mem_data_of_rev_map ~key ~data =
    let rev_list = MapModule2.find key !reverse_map in
    if List.length rev_list > 0 then
      if List.mem data rev_list then true else false
    else false
  let add ~key ~data =
    if mem_data_of_fwd_map ~key ~data then ()
    else
      let forward_list = MapModule1.find key !forward_map in
      let newlist = data::forward_list in
      let () = forward_map := MapModule1.add key newlist !forward_map in
      if List.length forward_list > 0 then
        List.iter
          (fun x ->
            let reverse_list = MapModule2.find x !reverse_map in
            let new_rev_list = key::reverse_list in
            reverse_map := MapModule2.add x new_rev_list !reverse_map
          ) forward_list
      else
        reverse_map := MapModule2.add data [key] !reverse_map;;

  let add_reverse ~key ~data =
    if mem_data_of_rev_map ~key ~data then ()
    else
      let rev_list = MapModule2.find key !reverse_map in
      let newlist = data::rev_list in
      let () = reverse_map := MapModule2.add key newlist !reverse_map in
      if List.length rev_list > 0 then
        List.iter
          (fun x ->
            let fwd_list = MapModule1.find x !forward_map in
            let new_fwd_list = key::fwd_list in
            forward_map := MapModule1.add data new_fwd_list !forward_map
          ) rev_list
      else
        forward_map := MapModule1.add data [key] !forward_map;;
    
  let singleton ~key ~data =
    let () = empty () in
    add ~key ~data
  let singleton_reverse ~key ~data =
    let () = empty () in
    add_reverse ~key ~data
                
  let create_reverse_map_from_forward_map () =
    let () = reverse_map := MapModule2.empty in
    MapModule1.iter
      (fun k v ->
        List.iter
          (fun x ->
            if MapModule2.mem x !reverse_map then 
              let rev_list = MapModule2.find x !reverse_map in
              reverse_map := MapModule2.add x (k::rev_list) !reverse_map
            else
              reverse_map := MapModule2.add x [k] !reverse_map
          ) v)
      !forward_map
      
  let create_forward_map_from_reverse_map () =
    let () = forward_map := MapModule1.empty in
    MapModule2.iter
      (fun k v ->
        List.iter
          (fun x ->
            if MapModule1.mem x !forward_map then 
              let fwd_list = MapModule1.find x !forward_map in
              forward_map := MapModule1.add x (k::fwd_list) !forward_map
            else
              forward_map := MapModule1.add x [k] !forward_map
          ) v)
      !reverse_map

  let remove ~key =
    if MapModule1.mem key !forward_map then 
      let values = MapModule1.find key !forward_map in 
      let () = forward_map := MapModule1.remove key !forward_map in
      List.iter
        (fun x ->
          let revlist = MapModule2.find x !reverse_map in
          (*remove only the key from the list*)
          let newlist = List.filter (fun y -> y != key) revlist in
          reverse_map := MapModule2.add x newlist !reverse_map
        )
        values
    else ()
      
    
  let remove_reverse ~key =
    if MapModule2.mem key !reverse_map then 
      let rev_list = MapModule2.find key !reverse_map in 
      let () = reverse_map := MapModule2.remove key !reverse_map in
      List.iter
        (fun x ->
          let fwd_list = MapModule1.find x !forward_map in
          let newlist = List.filter (fun y -> y != key) fwd_list in 
          forward_map := MapModule1.add x newlist !forward_map
        ) rev_list
    else ()      
  
  let merge ~f ~othermap =
    let () = forward_map := MapModule1.merge f !forward_map othermap in
    create_reverse_map_from_forward_map ()
  let merge_reverse ~f ~othermap =
    let () = reverse_map := MapModule2.merge f !reverse_map othermap in
    create_forward_map_from_reverse_map ()
  let union ~f ~othermap =
    let () = forward_map := MapModule1.union f !forward_map othermap in
    create_reverse_map_from_forward_map ()
  let union_reverse ~f ~othermap =
    let () = reverse_map := MapModule2.union f !reverse_map othermap in
    create_forward_map_from_reverse_map ()
  let compare ~f ~othermap =
    MapModule1.compare f !forward_map othermap
  let compare_reverse ~f ~othermap =
    MapModule2.compare f !reverse_map othermap
  let equal ~f ~othermap =
    MapModule1.equal f !forward_map othermap
  let equal_reverse ~f ~othermap =
    MapModule2.equal f !reverse_map othermap
  let filter ~f =
    let () = forward_map := (MapModule1.filter f !forward_map) in
    create_reverse_map_from_forward_map ()
  let filter_reverse ~f =
    let () = reverse_map := (MapModule2.filter f !reverse_map) in
    create_forward_map_from_reverse_map ()
  let iter ~f =
    let () = MapModule1.iter f !forward_map in
    create_reverse_map_from_forward_map ()
  let iter_reverse ~f =
    let () = MapModule2.iter f !reverse_map in
    create_forward_map_from_reverse_map ()    
  let fold ~f = 
    MapModule1.fold f !forward_map
  let fold_reverse ~f =
    MapModule2.fold f !reverse_map
  let for_all ~f =
    MapModule1.for_all f !forward_map
  let for_all_reverse ~f =
    MapModule2.for_all f !reverse_map
  let exists ~f =
    MapModule1.exists f !forward_map
  let exists_reverse ~f =
    MapModule2.exists f !reverse_map
  let partition ~f =
    MapModule1.partition f !forward_map
  let partition_reverse ~f =
    MapModule2.partition f !reverse_map
  let cardinal () =
    MapModule1.cardinal !forward_map
  let cardinal_reverse () =
    MapModule2.cardinal !reverse_map
  let bindings () =
    MapModule1.bindings !forward_map
  let bindings_reverse () =
    MapModule2.bindings !reverse_map
  let min_binding () =
    MapModule1.min_binding !forward_map
  let min_binding_reverse () =
    MapModule2.min_binding !reverse_map
  let max_binding () =
    MapModule1.max_binding !forward_map
  let max_binding_reverse () =
    MapModule2.max_binding !reverse_map
  let choose () =
    MapModule1.choose !forward_map
  let choose_reverse () =
    MapModule2.choose !reverse_map
  let split ~key =
    MapModule1.split key !forward_map
  let split_reverse ~key =
    MapModule2.split key !reverse_map
  let find_exn ~key =
    MapModule1.find key !forward_map
  let find_reverse_exn ~key =
    MapModule2.find key !reverse_map
  let find ~key =
    try
      Some (MapModule1.find key !forward_map)
    with _ -> None
  let find_reverse ~key =
    try
      Some (MapModule2.find key !reverse_map)
    with _ -> None 
  let map ~f =
    let () = forward_map := MapModule1.map f !forward_map in
    create_reverse_map_from_forward_map ()
  let map_reverse ~f =
    let () = reverse_map := MapModule2.map f !reverse_map in
    create_forward_map_from_reverse_map ()
  let mapi ~f =
    let () = forward_map := MapModule1.mapi f !forward_map in
    create_reverse_map_from_forward_map ()
  let mapi_reverse ~f =
    let () = reverse_map := MapModule2.mapi f !reverse_map in
    create_forward_map_from_reverse_map ()
  (*need a way to quickly set up bimap in an other than empty state by supplying 
    a map already populated with values*)
  let set_forward_map ~map =
    let () = empty () in
    let () = forward_map := map in
    create_reverse_map_from_forward_map ()
end;;
