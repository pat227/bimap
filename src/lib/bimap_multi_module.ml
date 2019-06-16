module Bimap_multi_module(MapModule1 : Map.S)(MapModule2 : Map.S) = struct 

  type t = { fwdmap:MapModule2.key list MapModule1.t; revmap:MapModule1.key list MapModule2.t}
  let create_t ~fwdmap ~revmap = { fwdmap=fwdmap; revmap=revmap } 
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

  let remove_fwd_key_from_reverse_map revmap ~fwd_values_list ~key =
    let rec foo m k l =
      (match l with
       | [] -> m
       | h :: tl ->
          let old_reverse_bindings_opt = MapModule2.find_opt h m in
          (match old_reverse_bindings_opt with
           | Some old_reverse_bindings -> 
              if ((List.length old_reverse_bindings) > 1) then
                let newvs = List.filter (fun v -> not (v = key)) old_reverse_bindings in
                let m2 = MapModule2.add h newvs m in
                foo m2 k tl
              else
                let m2 = MapModule2.remove h m in 
                foo m2 k tl
           | None -> foo m k tl)
      ) in
    foo revmap key fwd_values_list

    let remove_rev_key_from_forward_map fwdmap ~rev_values_list ~key =
      let rec foo m k l =
        (match l with
         | [] -> m
         | h :: tl ->
            let old_fwd_bindings_opt = MapModule1.find_opt h m in
            (match old_fwd_bindings_opt with
             | Some old_fwd_bindings -> 
                if ((List.length old_fwd_bindings) > 1) then
                  let newvs = List.filter (fun v -> not (v = key)) old_fwd_bindings in
                  let m2 = MapModule1.add h newvs m in
                  foo m2 k tl
                else
                  let m2 = MapModule1.remove h m in 
                  foo m2 k tl
             | None -> foo m k tl)
        ) in
      foo fwdmap key rev_values_list

              
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

  let bindings t =
    MapModule1.bindings t.fwdmap
  let bindings_reverse t =
    MapModule2.bindings t.revmap
  let cardinal t =
    MapModule1.cardinal t.fwdmap
  let cardinal_reverse t =
    MapModule2.cardinal t.revmap
  let choose t =
    MapModule1.choose t.fwdmap
  let choose_reverse t =
    MapModule2.choose t.revmap
  let compare t ~f ~othermap =
    MapModule1.compare f t.fwdmap othermap
  let compare_reverse t ~f ~othermap =
    MapModule2.compare f t.revmap othermap
  let equal t ~f ~othermap =
    MapModule1.equal f t.fwdmap othermap
  let equal_reverse t ~f ~othermap =
    MapModule2.equal f t.revmap othermap
  let exists t ~f =
    MapModule1.exists f t.fwdmap
  let exists_reverse t ~f =
    MapModule2.exists f t.revmap
  let filter t ~f =
    let new_forward_map = (MapModule1.filter f t.fwdmap) in
    let new_revmap = create_reverse_map_from_forward_map t.fwdmap in
    { fwdmap=new_forward_map; revmap=new_revmap }    
  let filter_reverse t ~f =
    let new_reverse_map = (MapModule2.filter f t.revmap) in
    let new_fwdmap = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_fwdmap; revmap=new_reverse_map }
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
  let fold t ~f = 
    MapModule1.fold f t.fwdmap
  let fold_reverse t ~f =
    MapModule2.fold f t.revmap
  let for_all t ~f =
    MapModule1.for_all f t.fwdmap
  let for_all_reverse t ~f =
    MapModule2.for_all f t.revmap
  (*iter returns unit, should not mutuate map, that is what the map function is for*)
  let iter t ~f =
    MapModule1.iter f t.fwdmap
  let iter_reverse t ~f =
    MapModule2.iter f t.revmap
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
  let max_binding t =
    MapModule1.max_binding t.fwdmap
  let max_binding_reverse t =
    MapModule2.max_binding t.revmap
  let min_binding t =
    MapModule1.min_binding t.fwdmap
  let min_binding_reverse t =
    MapModule2.min_binding t.revmap
  let merge t ~f ~othermap =
    let new_forward_map = MapModule1.merge f t.fwdmap othermap in
    let new_revmap = create_reverse_map_from_forward_map new_forward_map in
    { fwdmap=new_forward_map; revmap=new_revmap }
  let merge_reverse t ~f ~othermap =
    let new_reverse_map = MapModule2.merge f t.revmap othermap in
    let new_fwdmap = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_fwdmap; revmap=new_reverse_map }
  let partition t ~f =
    MapModule1.partition f t.fwdmap
  let partition_reverse t ~f =
    MapModule2.partition f t.revmap
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

  (**Remove the head of the list of values under the key, and return it too alongside the bimap*)
  let remove_multi t ~key =
    if MapModule1.mem key t.fwdmap then 
      let values = MapModule1.find key t.fwdmap in
      let new_values = List.tl values in
      let value = List.hd values in 
      let new_forward_map = MapModule1.add key new_values t.fwdmap in
      let rev_values = MapModule2.find value t.revmap in
      let new_rev_values = List.filter (fun x -> x != key) rev_values in
      let new_revmap = MapModule2.add value new_rev_values t.revmap in 
      let newt = { fwdmap=new_forward_map ; revmap=new_revmap } in
      (Some value, newt)
    else
      (None, t)

          (**Remove the head of the list of values under the key, and return it too alongside the bimap*)
  let remove_multi_reverse t ~key =
    if MapModule2.mem key t.revmap then 
      let values = MapModule2.find key t.revmap in
      let new_values = List.tl values in
      let value = List.hd values in 
      let new_forward_map = MapModule2.add key new_values t.revmap  in
      let rev_values = MapModule1.find value t.fwdmap in
      let new_rev_values = List.filter (fun x -> x != key) rev_values in
      let new_revmap = MapModule1.add value new_rev_values t.fwdmap in 
      let newt = { revmap=new_forward_map ; fwdmap=new_revmap } in
      (Some value, newt)
    else
      (None, t)

  (*need a way to quickly set up bimap in an other than empty state by supplying 
    a map already populated with values*)
  let set_forward_map t ~map =
    let new_reverse_map = create_reverse_map_from_forward_map map in
    { fwdmap=map; revmap=new_reverse_map }
  let singleton ~key ~data =
    let t = empty () in
    add t ~key ~data
  let singleton_reverse ~key ~data =
    let t = empty () in
    add_reverse t ~key ~data
  let split t ~key =
    MapModule1.split key t.fwdmap
  let split_reverse t ~key =
    MapModule2.split key t.revmap
  let union t ~f ~othermap =
    let new_forward_map = MapModule1.union f t.fwdmap othermap in
    let new_revmap = create_reverse_map_from_forward_map new_forward_map in
    { fwdmap=new_forward_map; revmap=new_revmap }
  let union_reverse t ~f ~othermap =
    let new_reverse_map = MapModule2.union f t.revmap othermap in
    let new_fwdmap = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_fwdmap; revmap=new_reverse_map }
  let update t ~key ~f =
    let rec insert_updated_reverse_values t l =
      match l with
      | [] -> t
      | h :: tl ->
         let newt = add_reverse t ~key:h ~data:key in
         insert_updated_reverse_values newt tl in 
    if MapModule1.mem key t.fwdmap then
      let oldvalues = MapModule1.find key t.fwdmap in
      let newfwdmap = MapModule1.update key f t.fwdmap in
      let newvalues = MapModule1.find key t.fwdmap in
      let newrevmap = remove_fwd_key_from_reverse_map t.revmap ~fwd_values_list:oldvalues ~key in
      let newt = { fwdmap=newfwdmap; revmap=newrevmap } in
      insert_updated_reverse_values newt newvalues
    else
      t

  let update_reverse t ~key ~f =
    let rec insert_updated_forward_values t l =
      match l with
      | [] -> t
      | h :: tl ->
         let newt = add t ~key:h ~data:key in
         insert_updated_forward_values newt tl in 
    if MapModule2.mem key t.revmap then
      let oldvalues = MapModule2.find key t.revmap in
      let newrevmap = MapModule2.update key f t.revmap in
      let newvalues = MapModule2.find key t.revmap in
      let newfwdmap = remove_rev_key_from_forward_map t.fwdmap ~rev_values_list:oldvalues ~key in
      let newt = { fwdmap=newfwdmap; revmap=newrevmap } in
      insert_updated_forward_values newt newvalues
    else
      t

end;;
