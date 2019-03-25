module Bimap_multi_module (ModuleA : Core.Comparable.S)(ModuleB : Core.Comparable.S) = struct

  type t = {
      fwdmap : ModuleB.Map.Key.t list ModuleA.Map.t;
      revmap : ModuleA.Map.Key.t list ModuleB.Map.t
    }
             
  let empty =
      {fwdmap=ModuleA.Map.empty; revmap=ModuleB.Map.empty}
        
  let rec add_multi t ~key ~data =
    let update_maps t ~key ~data =
      let new_forward_map =
        (ModuleA.Map.add_multi t.fwdmap ~key ~data) in
      let newt = { fwdmap=new_forward_map; revmap=t.revmap } in 
      add_multi_reverse newt ~key:data ~data:key in
    if ModuleA.Map.mem t.fwdmap key then
      if Core.List.mem
           (ModuleA.Map.find_exn t.fwdmap key) data
           ~equal:(ModuleB.equal) then t
      else 
        update_maps t ~key ~data
    else
      update_maps t ~key ~data
  and add_multi_reverse t ~key ~data =
    let update_maps t ~key ~data =
      let new_reverse_map = (ModuleB.Map.add_multi t.revmap ~key ~data) in
      let newt = { fwdmap=t.fwdmap; revmap=new_reverse_map } in 
      add_multi newt ~key:data ~data:key in
      if ModuleB.Map.mem t.revmap key then
        if Core.List.mem
             (ModuleB.Map.find_exn t.revmap key) data
             ~equal:(ModuleA.equal) then t
        else 
          update_maps t ~key ~data
      else
        update_maps t ~key ~data

    let rec remove_fwd_key_from_reverse_map t ~fwd_values_list ~key =
      match fwd_values_list with
      | [] -> t
      | h :: tl -> 
         let old_reverse_bindings = ModuleB.Map.find_exn t.revmap h in
         if (Core.List.count old_reverse_bindings ~f:(fun _v -> true)) > 1 then
           let new_reverse_bindings = Core.List.filter old_reverse_bindings ~f:(fun v -> not (v = key)) in
           let new_reverse_map = ModuleB.Map.remove t.revmap h in
           let new_reverse_map = ModuleB.Map.add_exn new_reverse_map ~key:h ~data:new_reverse_bindings in
           let newt = { fwdmap=t.fwdmap; revmap=new_reverse_map} in 
           remove_fwd_key_from_reverse_map newt ~fwd_values_list:tl ~key
         else
           let newrevmap = ModuleB.Map.remove t.revmap h in
           let newt = { fwdmap=t.fwdmap; revmap=newrevmap} in
           remove_fwd_key_from_reverse_map newt ~fwd_values_list:tl ~key

    let rec remove_rev_key_from_forward_map t ~rev_values_list ~key =
      match rev_values_list with
      | [] -> t
      | h :: tl -> 
         let old_fwd_bindings = ModuleA.Map.find_exn t.fwdmap h in
         if (Core.List.count old_fwd_bindings ~f:(fun _v -> true)) > 1 then
           let new_fwd_bindings = Core.List.filter old_fwd_bindings ~f:(fun v -> not (v = key)) in
           let new_forward_map = ModuleA.Map.remove t.fwdmap h in
           let newfwdmap = ModuleA.Map.add_exn new_forward_map ~key:h ~data:new_fwd_bindings in
           let newt = { fwdmap=newfwdmap; revmap=t.revmap} in 
           remove_rev_key_from_forward_map newt ~rev_values_list:tl ~key
         else
           let newfwdmap = ModuleA.Map.remove t.fwdmap h in
           let newt = { fwdmap=newfwdmap; revmap=t.revmap} in
           remove_rev_key_from_forward_map newt ~rev_values_list:tl ~key

    let change t ~key ~f =
      (* A -> 1,2,3 | B-> 4,5,6 | C -> 7,8,9 
         1->A|2->A|3->A|4->B|5->B|6->B ... etc
         but after some change could end up with
         A -> 2,4,6 | B-> 4,5,6 | C -> 7,8,9
         2->A|4->A,B|5->B|6->A,B| ... etc   *)
      let old_value_list = ModuleA.Map.find_exn t.fwdmap key in 
      let new_forward_map = ModuleA.Map.change t.fwdmap key ~f in
      let new_values = ModuleA.Map.find_exn new_forward_map key in
    (*--- now only need to fixup the reverse mapping ---
       For each old value (key in reverse mapping):
         a) if the only binding is to ~key provided to this function then remove the old value (key in reverse mapping)
         b) else filter the list of values to remove the ~key provided to this function
       And then for each new value, add them to reverse mapping  *)
      let tempt = { fwdmap=new_forward_map; revmap=t.revmap } in 
      let newt = remove_fwd_key_from_reverse_map tempt ~fwd_values_list:old_value_list ~key in
      let rec add_new_rev_mappings t2 ~new_values =
        match new_values with
        | [] -> t2
        | h :: tl ->
           let new_reverse_map = ModuleB.Map.add_multi t2.revmap ~key:h ~data:key in
           let tempt = { fwdmap = t2.fwdmap; revmap = new_reverse_map } in
           add_new_rev_mappings tempt ~new_values:tl
      in
      add_new_rev_mappings newt ~new_values

    let change_reverse t ~key ~f =
      let old_value_list = ModuleB.Map.find_exn t.revmap key in 
      let new_reverse_map = ModuleB.Map.change t.revmap key ~f in
      let new_values = ModuleB.Map.find_exn new_reverse_map key in
      let tempt = { fwdmap=t.fwdmap; revmap=new_reverse_map } in 
      let newt = remove_rev_key_from_forward_map tempt ~rev_values_list:old_value_list ~key in
      let rec add_new_fwd_mappings t2 ~new_values =
        match new_values with
        | [] -> t2
        | h :: tl ->
           let new_forward_map = ModuleA.Map.add_multi t2.fwdmap ~key:h ~data:key in
           let tempt = { fwdmap = new_forward_map; revmap = t2.revmap } in
           add_new_fwd_mappings tempt ~new_values:tl
      in
      add_new_fwd_mappings newt ~new_values

    let create_reverse_map_from_forward_map ~forward_map =
      let newrevmap = ref ModuleB.Map.empty in 
      let () = ModuleA.Map.iter_keys forward_map
	         ~f:(fun k ->
                   let values = ModuleA.Map.find_exn forward_map k in
                   Core.List.iter values ~f:(fun v -> newrevmap := ModuleB.Map.add_multi !newrevmap ~key:v ~data:k)
	         ) in
      !newrevmap

    let create_forward_map_from_reverse_map ~reverse_map =
      let newfwdmap = ref ModuleA.Map.empty in 
      let () = ModuleB.Map.iter_keys reverse_map
	         ~f:(fun k ->
	           let values = ModuleB.Map.find_exn reverse_map k in
                   Core.List.iter values ~f:(fun v -> newfwdmap := ModuleA.Map.add_multi !newfwdmap ~key:v ~data:k)
	         ) in
      !newfwdmap

    let count t ~f =
      ModuleA.Map.count t.fwdmap ~f
    let count_reverse t ~f  =
      ModuleB.Map.count t.revmap ~f
    let counti t ~f =
      ModuleA.Map.counti t.fwdmap ~f
    let data t =
      ModuleA.Map.data t.fwdmap
    let data_reverse t =
      ModuleB.Map.data t.revmap
    let empty = { fwdmap = ModuleA.Map.empty ; revmap = ModuleB.Map.empty }
    let exists t ~f =
      ModuleA.Map.exists t.fwdmap ~f
    let exists_reverse t ~f =
      ModuleB.Map.exists t.revmap ~f
    let existsi t ~f =
      ModuleA.Map.existsi t.fwdmap ~f
    let existsi_reverse t ~f =
      ModuleB.Map.existsi t.revmap ~f
    let filter t ~f =
      let new_forward_map = ModuleA.Map.filter t.fwdmap ~f in
      (*Since each map is a multi map of 'a list or 'b list, with 'b or 'a as keys, respectively, the ~f used to 
        filter keys in one map cannot be used to filter values in the other.*)
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
      { fwdmap = new_forward_map; revmap = newrevmap }
    let filter_keys t ~f =
      let new_forward_map = ModuleA.Map.filter_keys t.fwdmap ~f in
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
      { fwdmap = new_forward_map; revmap = newrevmap }
    let filter_reverse t ~f =
      let new_reverse_map = ModuleB.Map.filter t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in 
      { fwdmap = newfwdmap; revmap = new_reverse_map }
    let filter_keys_reverse t ~f =
      let new_reverse_map = ModuleB.Map.filter_keys t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in 
      { fwdmap = newfwdmap; revmap = new_reverse_map }
    let filter_map t ~f =
      let new_forward_map = ModuleA.Map.filter_map t.fwdmap ~f in
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
      { fwdmap = new_forward_map; revmap = newrevmap }
    let filter_map_reverse t ~f =
      let new_reverse_map = ModuleB.Map.filter_map t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in 
      { fwdmap = newfwdmap; revmap = new_reverse_map }
    let filteri t ~f =
      let new_forward_map = ModuleA.Map.filteri t.fwdmap ~f in
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:new_forward_map in
      { fwdmap = new_forward_map; revmap = newrevmap }
    let filteri_reverse t ~f =
      let new_reverse_map = ModuleB.Map.filteri t.revmap ~f in
      let newfwdmap = create_forward_map_from_reverse_map ~reverse_map:new_reverse_map in 
      { fwdmap = newfwdmap; revmap = new_reverse_map }
    let find t ~key =
      ModuleA.Map.find t.fwdmap key
    let find_reverse t ~key =
      ModuleB.Map.find t.revmap key 
    let find_exn t ~key =
      ModuleA.Map.find_exn t.fwdmap key
    let find_exn_reverse t ~key =
      ModuleB.Map.find_exn t.revmap key
    let fold : 'e. t -> init:'e -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> 'e -> 'e) -> 'e = 
      (fun t ~init ~f -> ModuleA.Map.fold t.fwdmap ~init ~f)
    let fold_reverse : 'e. t -> init:'e -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> 'e -> 'e) -> 'e =
      (fun t ~init ~f -> ModuleB.Map.fold t.revmap ~init ~f)
(*  method fold_range_inclusive ~min ~max ~init ~f =
      Core.Map.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    let fold_right : 'e. t -> init:'e -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> 'e -> 'e) -> 'e =
      (fun t ~init ~f -> ModuleA.Map.fold_right t.fwdmap ~init ~f)
    let fold_right_reverse : 'e. t -> init:'e -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> 'e -> 'e) -> 'e =
      (fun t ~init ~f -> ModuleB.Map.fold_right t.revmap ~init ~f)
    let for_all t ~f = ModuleA.Map.for_all t.fwdmap ~f
    let for_all_reverse t ~f = ModuleB.Map.for_all t.revmap ~f
    let is_empty t = ModuleA.Map.is_empty t.fwdmap
    let iter t ~f = ModuleA.Map.iter t.fwdmap ~f
    let iter_keys t ~f = ModuleA.Map.iter_keys t.fwdmap ~f
    let iter_keys_reverse t ~f = ModuleB.Map.iter_keys t.revmap ~f
    let iter_reverse t ~f = ModuleB.Map.iter t.revmap ~f
    let iteri t ~f = ModuleA.Map.iteri t.fwdmap ~f
    let iteri_reverse t ~f = ModuleB.Map.iteri t.revmap ~f
    let keys t = ModuleA.Map.keys t.fwdmap
    let keys_reverse t = ModuleB.Map.keys t.revmap
    let length t = ModuleA.Map.length t.fwdmap
    let map t ~f =
      let newfwdmap = ModuleA.Map.map t.fwdmap ~f in
      let newrevmap = create_reverse_map_from_forward_map ~forward_map:newfwdmap in
      { fwdmap=newfwdmap; revmap = newrevmap }
(*    method map_reverse ~f =
      let () = reverse_map := (ModuleB.Map.map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map () 
    method mapi ~f =
      let () = forward_map := (Core.Map.mapi !forward_map ~f) in
      self#create_reverse_map_from_forward_map ()
    method mapi_reverse ~f =
      let () = reverse_map := (Core.Map.mapi !reverse_map ~f) in
      self#create_forward_map_from_reverse_map ()     
    method mem key =
      Core.Map.mem !forward_map key
    method mem_reverse key =
      Core.Map.mem !reverse_map key
    method min_elt =
      Core.Map.min_elt !forward_map
    method min_elt_exn =
      Core.Map.min_elt_exn !forward_map
    method min_elt_reverse =
      Core.Map.min_elt !reverse_map
    method min_elt_exn_reverse =
      Core.Map.min_elt_exn !reverse_map
    method max_elt =
      Core.Map.max_elt !forward_map
    method max_elt_exn =
      Core.Map.max_elt_exn !forward_map
    method max_elt_reverse =
      Core.Map.max_elt !reverse_map
    method max_elt_exn_reverse =
      Core.Map.max_elt_exn !reverse_map
    method nth int =
      Core.Map.nth !forward_map int
    method nth_reverse int =
      Core.Map.nth !reverse_map int
    method remove ~key =
      let reverse_keys = Core.Map.find_exn !forward_map key in 
      let () = forward_map := (Core.Map.remove !forward_map key) in
      self#remove_reverse_keys reverse_keys
    method private remove_reverse_keys klist = 
      let rec remove_keys k =
	match k with
	| h :: t ->
	   let () = reverse_map := (Core.Map.remove !reverse_map h) in
	   remove_keys t
	| [] -> () in
      remove_keys klist
    method remove_reverse ~key =
      let fwd_keys = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.remove !reverse_map key) in
      Core.List.iter fwd_keys
        ~f:(fun k ->
          let fwd_values = self#find_exn ~key:k in
          let new_fwd_values = (Core.List.filter fwd_values ~f:(fun x -> not (x = key))) in
          let () = forward_map := (Core.Map.remove !forward_map k) in
          forward_map := (Core.Map.set !forward_map  ~key:k ~data:new_fwd_values)
        )
    method remove_multi ~key =
      try
	let values = ModuleA.Map.find_exn !forward_map key in
	let head_element = Core.List.nth_exn values 0 in 
	let () = forward_map := (ModuleA.Map.remove_multi !forward_map key) in
	(*using head_element: if reverse_map binds head_element only to key then remove it else filter out key*)
        self#remove_fwd_key_from_reverse_map ~fwd_values_list:[head_element] ~key
        (*--TODO--improve exception handling*)
      with _e -> raise (Failure "bimap_multi::remove_multi() failed")
    method remove_reverse_multi ~key =
      try
	let values = ModuleB.Map.find_exn !reverse_map key in
	let head_element = Core.List.nth_exn values 0 in 
	let () = reverse_map := (ModuleB.Map.remove_multi !reverse_map key) in
	(*using head_element: if reverse_map binds head_element only to key then remove it else filter out key*)
        self#remove_rev_key_from_forward_map ~rev_values_list:[head_element] ~key
        (*--TODO--improve exception handling*)
      with _e -> raise (Failure "bimap_multi::remove_reverse_multi() failed")
    method to_alist ?key_order () =
      match Core.Option.is_some key_order with
      | false -> Core.Map.to_alist !forward_map
      | true -> Core.Map.to_alist ~key_order:(Core.Option.value_exn key_order) !forward_map
    (*update and change are identical except that the function f must be of a different type; see Core.Map documentation.*)
    method update ~key ~f =
      let oldvalues = ModuleA.Map.find_exn !forward_map key in
      let () = forward_map := (ModuleA.Map.update !forward_map key ~f) in
      let newvalues = ModuleA.Map.find_exn !forward_map key in
      (*remove or filter mappings in reverse map for oldvalues and then add_multi newvalues to reverse map*)
      let () = self#remove_fwd_key_from_reverse_map ~fwd_values_list:oldvalues ~key in
      Core.List.iter newvalues
        ~f:(fun v -> 
          reverse_map := ModuleB.Map.add_multi !reverse_map ~key:v ~data:key
        )
 *)
end

(*
    method equal f ~other_fwd_map =
      Core.Map.equal f !forward_map !other_fwd_map *)

(*  UNBOUND 'e --- how to write these?
    method fold ~init ~f =
      Core.Map.fold !forward_map ~init ~f
    method fold_reverse ~init ~f =
      Core.Map.fold !reverse_map ~init ~f*)
(*
Cannot do these right now--type 'c the compraator is unbound
    method comparator () =
      Core.Map.comparator !forward_map
    method comparator_reverse () =
      Core.Map.comparator !reverse_map
*)
