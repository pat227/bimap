module Bimap_single_module(MapModule1 : Map.S)(MapModule2 : Map.S) = struct

  type t = { fwdmap:MapModule2.key MapModule1.t; revmap:MapModule1.key MapModule2.t}

  let empty () =
    { fwdmap=MapModule1.empty; revmap=MapModule2.empty }
  let is_empty t =
    MapModule1.is_empty t.fwdmap
  let is_empty_reverse t =
    MapModule2.is_empty t.revmap
  let mem t ~key =
    MapModule1.mem key t.fwdmap
  let mem_reverse t ~key =
    MapModule2.mem key t.revmap;;
  let add t ~key ~data =
    if MapModule1.mem key t.fwdmap then
      let value = MapModule1.find key t.fwdmap in
      let new_reverse_map = MapModule2.remove value t.revmap in 
      let new_forward_map = MapModule1.add key data t.fwdmap in 
      { fwdmap=new_forward_map; revmap=new_reverse_map }
    else
      (*enforce only one key <-> value pair; do not permit, for example, 
        key -> value1 | key -> value2 while in reverse mapping only have 
        value1 -> key at same time.*)
      if MapModule2.mem data t.revmap then
        let new_reverse_map = MapModule2.remove data t.revmap in
        let new_forward_map = MapModule1.add key data t.fwdmap in 
        { fwdmap=new_forward_map; revmap=new_reverse_map }
      else 
        let new_forward_map = MapModule1.add key data t.fwdmap in 
        let new_reverse_map = MapModule2.add data key t.revmap in
        { fwdmap=new_forward_map; revmap=new_reverse_map }
  let add_reverse t ~key ~data =
    if MapModule2.mem key t.revmap then
      let value = MapModule2.find key t.revmap in
      let new_forward_map = MapModule1.remove value t.fwdmap in
      let new_reverse_map = MapModule2.add key data t.revmap in
      { fwdmap=new_forward_map; revmap=new_reverse_map }
    else if MapModule1.mem data t.fwdmap then
      let new_forward_map = MapModule1.remove data t.fwdmap in
      let new_reverse_map = MapModule2.add key data t.revmap in 
      let new_forward_map = MapModule1.add data key new_forward_map in
      { fwdmap=new_forward_map; revmap=new_reverse_map }
    else
      let new_reverse_map = MapModule2.add key data t.revmap in
      let new_forward_map = MapModule1.add data key t.fwdmap in
      { fwdmap=new_forward_map; revmap=new_reverse_map };;
  let singleton t ~key ~data =
    let empty_t = empty () in
    add empty_t ~key ~data
  let singleton_reverse t ~key ~data =
    let empty_t = empty () in
    add_reverse empty_t ~key ~data
  let create_reverse_map_from_forward_map ~forward_map =
    let newrmap = MapModule2.empty in
    let newrmapref = ref newrmap in 
    let () = MapModule1.iter 
	       (fun key data ->
	         newrmapref :=
                   (MapModule2.add data key !newrmapref))
               forward_map in
    !newrmapref
  let create_forward_map_from_reverse_map ~reverse_map =
    let newfmap = MapModule1.empty in
    let newfmapref = ref newfmap in 
    let () = MapModule2.iter 
	       (fun key data ->
		 newfmapref :=
                   MapModule1.add data key !newfmapref) reverse_map in
    !newfmapref

  let remove t ~key =
    let value = MapModule1.find key t.fwdmap in 
    let new_forward_map = MapModule1.remove key t.fwdmap in
    let new_reverse_map = MapModule2.remove value t.revmap in 
    { fwdmap=new_forward_map ; revmap=new_reverse_map}
  let remove_reverse t ~key =
    let fwd_value = MapModule2.find key t.revmap in 
    let new_reverse_map = MapModule2.remove key t.revmap in
    let new_forward_map = MapModule1.remove fwd_value t.fwdmap in
    { fwdmap=new_forward_map ; revmap=new_reverse_map }
  let get_forward_map t = t.fwdmap
  let get_reverse_map t = t.revmap
  let merge t ~f ~othermap =
    let new_forward_map = MapModule1.merge f t.fwdmap othermap in
    let new_rev_map = create_reverse_map_from_forward_map new_forward_map in
    { fwdmap=new_forward_map; revmap=new_rev_map }
  let merge_reverse t ~f ~othermap =
    let new_reverse_map = MapModule2.merge f t.revmap othermap in
    let new_fwd_map = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in
    { fwdmap=new_fwd_map; revmap=new_reverse_map }
(*  let union ~f ~othermap =
    let () = forward_map := MapModule1.union f !forward_map othermap in
    create_reverse_map_from_forward_map ()
  let union_reverse ~f ~othermap =
    let () = reverse_map := MapModule2.union f !reverse_map othermap in
    create_forward_map_from_reverse_map ()*)
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
    let new_rev_map = create_reverse_map_from_forward_map new_forward_map in
    { fwdmap=new_forward_map; revmap=new_rev_map }
  let filter_reverse t ~f =
    let new_reverse_map = (MapModule2.filter f t.revmap) in
    let new_fwd_map = create_forward_map_from_reverse_map new_reverse_map in
    { fwdmap=new_fwd_map; revmap=new_reverse_map }
  (*FIX THIS -- iter mutates the map...function should return unit...modify the revmap*)
  let iter t ~f =
    let () = MapModule1.iter f t.fwdmap in
    let new_rev_map = create_reverse_map_from_forward_map t.fwdmap in
    { fwdmap=t.fwdmap; revmap=new_rev_map }
  let iter_reverse t ~f =
    let () = MapModule2.iter f t.revmap in
    let new_fwd_map = create_forward_map_from_reverse_map t.revmap in
    { fwdmap=new_fwd_map; revmap=t.revmap }
(*  let fold ~f = 
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
  let min_binding_exn () =
    MapModule1.min_binding !forward_map
  let min_binding_reverse_exn () =
    MapModule2.min_binding !reverse_map
  let max_binding_exn () =
    MapModule1.max_binding !forward_map
  let max_binding_reverse_exn () =
    MapModule2.max_binding !reverse_map
  let min_binding () =
    try
      Some (MapModule1.min_binding !forward_map)
    with _ -> None 
  let min_binding_reverse () =
    try 
      Some (MapModule2.min_binding !reverse_map)
    with _ -> None
  let max_binding () =
    try 
      Some (MapModule1.max_binding !forward_map)
    with _ -> None
  let max_binding_reverse () =
    try 
      Some (MapModule2.max_binding !reverse_map)
    with _ -> None
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
  (*need a way to quickly set up bimap in an other than empty state by supplying a map already populated with values*)
  let set_forward_map ~map =
    let () = empty () in
    let () = forward_map := map in
    create_reverse_map_from_forward_map ()*)
end;;
