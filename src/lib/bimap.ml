(*Not using core have to use a functor. A class expects a type, which Core Map does,
but map.mli only provides a module type ... wait maybe this is possible using the (+a) t type in Map.mli????!!! At any rate, use functor.*)
module Bimap(MapModule1 : Map.S)(MapModule2 : Map.S) = struct
  let forward_map = ref MapModule1.empty;; 
  let reverse_map = ref MapModule2.empty;;   
  (*empty, union, merge, map, etc, all mutate the maps, ie, the functions do not return maps unlike counterparts in map.mli*)
  let empty () =
    let () = forward_map := MapModule1.empty in
    reverse_map := MapModule2.empty
  let is_empty () =
    MapModule1.is_empty !forward_map
  let is_empty_reverse () =
    MapModule2.is_empty !reverse_map
  let mem k =
    MapModule1.mem k !forward_map
  let mem_reverse k =
    MapModule2.mem k !reverse_map
  let add ~key ~data =
    let () = forward_map := MapModule1.add key data !forward_map in 
    reverse_map := MapModule2.add data key !reverse_map
  let add_reverse ~key ~data =
    let () = reverse_map := (MapModule2.add key data !reverse_map) in
    forward_map := MapModule1.add data key !forward_map
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
	       reverse_map :=
		 MapModule2.add v k !reverse_map) !forward_map
  let create_forward_map_from_reverse_map () =
    let () = forward_map := MapModule1.empty in 
    MapModule2.iter
      (fun k v ->
	forward_map :=
	  MapModule1.add v k !forward_map) !reverse_map
  let remove ~key =
    let value = MapModule1.find key !forward_map in 
    let () = forward_map := MapModule1.remove key !forward_map in
    reverse_map := MapModule2.remove value !reverse_map
  let remove_reverse ~key =
    let fwd_value = MapModule2.find key !reverse_map in 
    let () = reverse_map := MapModule2.remove key !reverse_map in
    forward_map := MapModule1.remove fwd_value !forward_map
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
  let fold f = 
    MapModule1.fold f !forward_map
  let fold_reverse f =
    MapModule2.fold f !reverse_map
  let for_all f =
    MapModule1.for_all f !forward_map
  let for_all_reverse f =
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
  let find ~key =
    MapModule1.find key !forward_map
  let find_reverse ~key =
    MapModule2.find key !reverse_map
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
    create_reverse_map_from_forward_map ()
end;;
