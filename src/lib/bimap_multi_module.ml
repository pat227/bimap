module Bimap_multi_module(MapModule1 : Map.S)(MapModule2 : Map.S) = struct 

  type t = { fwdmap:MapModule2.key list MapModule1.t; revmap:MapModule1.key list MapModule2.t}
             
  let get_forward_map t = t.fwdmap
  let get_reverse_map t = t.revmap
  let empty () = { fwdmap=MapModule1.empty; revmap=MapModule2.empty }
  let is_empty t = MapModule1.is_empty t.fwdmap
  let is_empty_reverse t = MapModule2.is_empty t.revmap
  let mem t ~key = MapModule1.mem key t.fwdmap
  let mem_reverse t ~key = MapModule2.mem key t.revmap
  (*keep private*)
  let mem_data_of_fwd_map forward_map ~key ~data =
    if MapModule1.mem key forward_map then
      let forward_list = MapModule1.find key forward_map in
      if List.length forward_list > 0 then
        if List.mem data forward_list then true else false
      else false
    else false
  (*keep private*)
  let mem_data_of_rev_map reverse_map ~key ~data =
    if MapModule2.mem key reverse_map then
      let rev_list = MapModule2.find key reverse_map in
      if List.length rev_list > 0 then
        if List.mem data rev_list then true else false
      else false
    else false

  (*
        if List.length forward_list > 0 then
          List.iter
            (fun x ->
              let reverse_list = MapModule2.find x !reverse_map in
              let new_rev_list = key::reverse_list in
              reverse_map := MapModule2.add x new_rev_list !reverse_map
            ) forward_list
        else
          reverse_map := MapModule2.add data [key] !reverse_map
*)
    (*let update_all_fwd_keys ~fwdkey ~rev_list =
      if List.length rev_list > 0 then
        List.iter
          (fun x ->
            let fwd_list = MapModule1.find x !forward_map in
            let new_fwd_list = data::fwd_list in
            forward_map := MapModule1.add data new_fwd_list !forward_map
          ) rev_list else () in *)       
  let rec add t ~key ~data =
    if MapModule1.mem key t.fwdmap then
      if mem_data_of_fwd_map t.fwdmap ~key ~data then t
      else
        let forward_list = MapModule1.find key t.fwdmap in
        let newlist = data::forward_list in
        let new_forward_map = MapModule1.add key newlist t.fwdmap in
        let newt = { fwdmap=new_forward_map ; revmap=t.revmap } in
        add_reverse newt ~key:data ~data:key
    else
      let new_forward_map = MapModule1.add key [data] t.fwdmap in
      let newt = { fwdmap=new_forward_map ; revmap=t.revmap } in
      add_reverse newt ~key:data ~data:key
  and add_reverse t ~key ~data =
    if MapModule2.mem key t.revmap then
      if mem_data_of_rev_map t.revmap ~key ~data then t
      else
        let rev_list = MapModule2.find key t.revmap in
        let newlist = data::rev_list in
        let new_reverse_map = MapModule2.add key newlist t.revmap in
        let newt = { fwdmap=t.fwdmap ; revmap=new_reverse_map } in
        add newt ~key:data ~data:key
    else
      let new_reverse_map = MapModule2.add key [data] t.revmap in
      let newt = { fwdmap=t.fwdmap ; revmap=new_reverse_map } in
      add newt ~key:data ~data:key
(*
    else
      if MapModule2.mem key !reverse_map then 
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
          forward_map := MapModule1.add data [key] !forward_map
      else
        let () = reverse_map := MapModule2.add key [data] !reverse_map in
        forward_map := MapModule1.add data [key] !forward_map 

 *)
              
  let singleton ~key ~data =
    let t = empty () in
    add t ~key ~data
  let singleton_reverse ~key ~data =
    let t = empty () in
    add_reverse t ~key ~data
  let create_reverse_map_from_forward_map forward_map =
    let new_reverse_map = ref MapModule2.empty in
    let () = MapModule1.iter
               (fun k v ->
                 List.iter
                   (fun x ->
                     if MapModule2.mem x !new_reverse_map then 
                       let rev_list = MapModule2.find x !new_reverse_map in
                       new_reverse_map := MapModule2.add x (k::rev_list) !new_reverse_map
                     else
                       new_reverse_map := MapModule2.add x [k] !new_reverse_map
                   ) v
               ) forward_map in
    !new_reverse_map
      
  let create_forward_map_from_reverse_map reverse_map =
    let new_forward_map = ref MapModule1.empty in
    let () = MapModule2.iter
               (fun k v ->
                 List.iter
                   (fun x ->
                     if MapModule1.mem x !new_forward_map then 
                       let fwd_list = MapModule1.find x !new_forward_map in
                       new_forward_map := MapModule1.add x (k::fwd_list) !new_forward_map
                     else
                       new_forward_map := MapModule1.add x [k] !new_forward_map
                   ) v)
               reverse_map in
    !new_forward_map

  let remove t ~key =
    if MapModule1.mem key t.fwdmap then 
      let values = MapModule1.find key t.fwdmap in 
      let new_forward_map = MapModule1.remove key t.fwdmap in
      let new_revmap =
        List.fold_left
          (fun m x ->
            MapModule2.update x
              (fun revlist_opt ->
                match revlist_opt with
                | Some revlist -> 
                   Some (List.filter (fun y -> y != key) revlist)
                | None -> (*should be impossible or evidence of a bug*)
                   raise (Failure "A value in the fwdmap was not found as key in revmap!")
              ) m
          ) t.revmap values in 
      { fwdmap=new_forward_map ; revmap=new_revmap }
    else t

  let remove_reverse t ~key =
    if MapModule2.mem key t.revmap then 
      let rev_list = MapModule2.find key t.revmap in 
      let new_reverse_map = MapModule2.remove key t.revmap in
      let new_fwdmap =
        List.fold_left
          (fun m x ->
            MapModule1.update x
              (fun fwdlist_opt ->
                match fwdlist_opt with
                | Some fwdlist ->
                   Some (List.filter (fun y -> y != key) fwdlist)
                | None -> (*should be impossible or evidence of a bug*)
                   raise (Failure "A value in the revmap was not found as key in fwdmap!")
              ) m
          ) t.fwdmap rev_list in
      { fwdmap=new_fwdmap; revmap=new_reverse_map }
    else t
  
  let merge t ~f ~othermap =
    let new_forward_map = MapModule1.merge f t.fwdmap othermap in
    let new_revmap = create_reverse_map_from_forward_map new_forward_map in
    { fwdmap=new_forward_map; revmap=new_revmap }
  let merge_reverse t ~f ~othermap =
    let new_reverse_map = MapModule2.merge f t.revmap othermap in
    let new_fwdmap = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_fwdmap; revmap=new_reverse_map }
  let union t ~f ~othermap =
    let new_forward_map = MapModule1.union f t.fwdmap othermap in
    let new_revmap = create_reverse_map_from_forward_map new_forward_map in
    { fwdmap=new_forward_map; revmap=new_revmap }
  let union_reverse t ~f ~othermap =
    let new_reverse_map = MapModule2.union f t.revmap othermap in
    let new_fwdmap = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_fwdmap; revmap=new_reverse_map }
  let compare t ~f ~othermap =
    MapModule1.compare f t.fwdmap othermap
  let compare_reverse t ~f ~othermap =
    MapModule2.compare f t.revmap othermap
  let equal t ~f ~othermap =
    MapModule1.equal f t.fwdmap othermap
  let equal_reverse t ~f ~othermap =
    MapModule2.equal f t.revmap othermap
  let filter t ~f =
    let new_forward_map = (MapModule1.filter f t.fwdmap) in
    let new_revmap = create_reverse_map_from_forward_map t.fwdmap in
    { fwdmap=new_forward_map; revmap=new_revmap }    
  let filter_reverse t ~f =
    let new_reverse_map = (MapModule2.filter f t.revmap) in
    let new_fwdmap = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_fwdmap; revmap=new_reverse_map }
  (*iter returns unit, should not mutuate map, that is what the map function is for*)
  let iter t ~f =
    MapModule1.iter f t.fwdmap
  let iter_reverse t ~f =
    MapModule2.iter f t.revmap
  let fold t ~f = 
    MapModule1.fold f t.fwdmap
  let fold_reverse t ~f =
    MapModule2.fold f t.revmap
  let for_all t ~f =
    MapModule1.for_all f t.fwdmap
  let for_all_reverse t ~f =
    MapModule2.for_all f t.revmap
  let exists t ~f =
    MapModule1.exists f t.fwdmap
  let exists_reverse t ~f =
    MapModule2.exists f t.revmap
  let partition t ~f =
    MapModule1.partition f t.fwdmap
  let partition_reverse t ~f =
    MapModule2.partition f t.revmap
  let cardinal t =
    MapModule1.cardinal t.fwdmap
  let cardinal_reverse t =
    MapModule2.cardinal t.revmap
  let bindings t =
    MapModule1.bindings t.fwdmap
  let bindings_reverse t =
    MapModule2.bindings t.revmap
  let min_binding t =
    MapModule1.min_binding t.fwdmap
  let min_binding_reverse t =
    MapModule2.min_binding t.revmap
  let max_binding t =
    MapModule1.max_binding t.fwdmap
  let max_binding_reverse t =
    MapModule2.max_binding t.revmap
  let choose t =
    MapModule1.choose t.fwdmap
  let choose_reverse t =
    MapModule2.choose t.revmap
  let split t ~key =
    MapModule1.split key t.fwdmap
  let split_reverse t ~key =
    MapModule2.split key t.revmap
  let find_exn t ~key =
    MapModule1.find key t.fwdmap
  let find_reverse_exn t ~key =
    MapModule2.find key t.revmap
  let find t ~key =
    try
      Some (MapModule1.find key t.fwdmap)
    with _ -> None
  let find_reverse t ~key =
    try
      Some (MapModule2.find key t.revmap)
    with _ -> None 
  let map t ~f =
    let new_forward_map = MapModule1.map f t.fwdmap in
    let new_revmap = create_reverse_map_from_forward_map new_forward_map in
    { fwdmap=new_forward_map; revmap=new_revmap }    
  let map_reverse t ~f =
    let new_reverse_map = MapModule2.map f t.revmap in
    let new_fwdmap = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_fwdmap; revmap=new_reverse_map }
      
  let mapi t ~f =
    let new_forward_map = MapModule1.mapi f t.fwdmap in
    let new_reverse_map = create_reverse_map_from_forward_map new_forward_map in
    { fwdmap=new_forward_map; revmap=new_reverse_map }    
  let mapi_reverse t ~f =
    let new_reverse_map = MapModule2.mapi f t.revmap in
    let new_forward_map = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_forward_map; revmap=new_reverse_map }
  (*need a way to quickly set up bimap in an other than empty state by supplying 
    a map already populated with values*)
  let set_forward_map t ~map =
    let new_reverse_map = create_reverse_map_from_forward_map map in
    { fwdmap=map; revmap=new_reverse_map }
end;;
