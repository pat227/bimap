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
    MapModule1.iter
      (fun k v ->
        List.iter
          (fun x ->
            if MapModule2.mem x !new_reverse_map then 
              let rev_list = MapModule2.find x !new_reverse_map in
              new_reverse_map := MapModule2.add x (k::rev_list) !new_reverse_map
            else
              new_reverse_map := MapModule2.add x [k] !new_reverse_map
          ) v
      ) forward_map
      
  let create_forward_map_from_reverse_map reverse_map =
    let new_forward_map = ref MapModule1.empty in
    MapModule2.iter
      (fun k v ->
        List.iter
          (fun x ->
            if MapModule1.mem x !new_forward_map then 
              let fwd_list = MapModule1.find x !new_forward_map in
              new_forward_map := MapModule1.add x (k::fwd_list) !new_forward_map
            else
              new_forward_map := MapModule1.add x [k] !new_forward_map
          ) v)
      !reverse_map

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
                   Some (List.filter (fun y -> y != key) fwd_list)
                | None -> (*should be impossible or evidence of a bug*)
                   raise (Failure "A value in the revmap was not found as key in fwdmap!")
              ) m
          ) t.fwdmap rev_list in
      { fwdmap=new_fwdmap; revmap=new_reverse_map }
    else t
(*  
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
 *)
end;;
