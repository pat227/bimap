module Bimap_multi_class (ModuleA : Map.S)(ModuleB : Map.S) = struct
  class bimap_multi_class = object(self)
    val mutable forward_map = ref ModuleA.empty
    val mutable reverse_map = ref ModuleB.empty
    method private empty_forward_map () =
      forward_map := ModuleA.empty
    method private empty_reverse_map () =
      reverse_map := ModuleB.empty

    method add_multi ~key ~data =
      let update_maps () =
        if ModuleA.mem key !forward_map then
          let oldvs = ModuleA.find key !forward_map in
          let newvs = (data::oldvs) in 
          let () = forward_map :=
                     (ModuleA.add key newvs !forward_map) in
          self#add_multi_reverse ~key:data ~data:key
        else
          let () = forward_map :=
                     (ModuleA.add key [data] !forward_map) in
          self#add_multi_reverse ~key:data ~data:key in 
      if ModuleA.mem key !forward_map then
        if List.mem data (ModuleA.find key !forward_map) then ()
        else 
          update_maps ()
      else
        update_maps ()

    method add_multi_reverse ~key ~data =
      let update_maps () =
        if ModuleB.mem key !reverse_map then
          let oldvs = ModuleB.find key !reverse_map in
          let newvs = (data::oldvs) in 
          let () = reverse_map :=
                     (ModuleB.add key newvs !reverse_map) in
          self#add_multi ~key:data ~data:key 
        else
          let () = reverse_map :=
                     (ModuleB.add key [data] !reverse_map) in
          self#add_multi ~key:data ~data:key in 
      if ModuleB.mem key !reverse_map then
        if List.mem data
             (ModuleB.find key !reverse_map)
             then ()
        else 
          update_maps ()
      else
        update_maps ()

    method private remove_fwd_key_from_reverse_map ~fwd_values_list ~key =
      List.iter
        (fun k ->
          let old_reverse_bindings_opt = ModuleB.find_opt k !reverse_map in
          match old_reverse_bindings_opt with
          | Some old_reverse_bindings -> 
             if ((List.length old_reverse_bindings) > 1) then
               let new_reverse_bindings = List.filter (fun v -> not (v = key)) old_reverse_bindings in
               let () = reverse_map := ModuleB.remove k !reverse_map in
               reverse_map :=
                 ModuleB.add k new_reverse_bindings !reverse_map
             else
               reverse_map := ModuleB.remove k !reverse_map
          | None -> ()
        ) fwd_values_list

    method private remove_rev_key_from_forward_map ~rev_values_list ~key =
      List.iter
        (fun k ->
          let old_fwd_bindings_opt = ModuleA.find_opt k !forward_map in
          match old_fwd_bindings_opt with
          | Some old_fwd_bindings -> 
             if (List.length old_fwd_bindings) > 1 then
               let new_fwd_bindings = List.filter (fun v -> not (v = key)) old_fwd_bindings in
               let () = forward_map := ModuleA.remove k !forward_map in
               forward_map :=
                 ModuleA.add k new_fwd_bindings !forward_map
             else
               forward_map := ModuleA.remove k !forward_map
          | None -> ()             
        ) rev_values_list

    method private create_reverse_map_from_forward_map () =
      let () = self#empty_reverse_map () in 
      ModuleA.iter
	(fun k v ->
          List.iter (fun v -> self#add_multi_reverse ~key:v ~data:k) v
	) !forward_map

    method private create_forward_map_from_reverse_map () =
      let () = self#empty_forward_map () in 
      ModuleB.iter
	(fun k v ->
          List.iter (fun v -> self#add_multi ~key:v ~data:k) v
	) !reverse_map

    method cardinal () =
      ModuleA.cardinal !forward_map
    method cardinal_reverse () =
      ModuleB.cardinal !reverse_map
(*
    method counti ~f =
      ModuleA.counti !forward_map ~f
    method data =
      ModuleA.data !forward_map
    method data_reverse =
      ModuleB.data !reverse_map
    method empty () =
      (*let () = forward_map := (Core.empty ~comparator:(Core.comparator !forward_map)) in
      reverse_map := (Core.empty ~comparator:(Core.comparator !reverse_map))*)
      let () = self#empty_forward_map () in
      self#empty_reverse_map ()
    method exists ~f =
      ModuleA.exists !forward_map ~f
    method exists_reverse ~f =
      ModuleB.exists !reverse_map ~f
    method existsi ~f =
      ModuleA.existsi !forward_map ~f
    method existsi_reverse ~f =
      ModuleB.existsi !reverse_map ~f
    method find ~key =
      ModuleA.find !forward_map key
    method find_reverse ~key =
      ModuleB.find !reverse_map key 
    method find_exn ~(key:ModuleA.Key.t) =
      ModuleA.find_exn !forward_map key
    method find_exn_reverse ~key =
      Core.find_exn !reverse_map key
    method filter ~f =
      let () = forward_map := (ModuleA.filter !forward_map ~f) in
      (*Since each map is a multi map of 'a list or 'b list, with 'b or 'a as keys, respectively, the ~f used to 
        filter keys in one map cannot be used to filter values in the other.*)
      self#create_reverse_map_from_forward_map
    method filter_reverse ~f =
      let () = reverse_map := (ModuleB.filter !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_keys ~f =
      let () = forward_map := (ModuleA.filter_keys !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filter_keys_reverse ~f =
      let () = reverse_map := (ModuleB.filter_keys !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filteri ~f =
      let () = forward_map := (ModuleA.filteri !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filteri_reverse ~f =
      let () = reverse_map := (ModuleB.filteri !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_map ~f =
      let () = forward_map := (ModuleA.filter_map !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filter_map_reverse ~f =
      let () = reverse_map := (ModuleB.filter_map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method fold : 'e. init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> ModuleA.fold !forward_map ~init ~f)
    method fold_reverse : 'e. init:'e -> f:(key:'b -> data:'a list -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.fold !reverse_map ~init ~f)
(*    method fold_range_inclusive ~min ~max ~init ~f =
      Core.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    method fold_right : 'e. init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleA.fold_right !forward_map ~init ~f)
    method fold_right_reverse : 'e. init:'e -> f:(key:'b -> data:'a list -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.fold_right !reverse_map ~init ~f)
    method for_all ~f =
      ModuleA.for_all !forward_map ~f
    method for_all_reverse ~f =
      ModuleB.for_all !reverse_map ~f
    method is_empty =
      ModuleA.is_empty !forward_map
    method iter_keys ~f =
      ModuleA.iter_keys !forward_map ~f
    method iter_keys_reverse ~f =
      ModuleB.iter_keys !reverse_map ~f
    method iter ~f =
      ModuleA.iter !forward_map ~f
    method iter_reverse ~f =
      ModuleB.iter !reverse_map ~f
    method iteri ~f =
      ModuleA.iteri !forward_map ~f
    method iteri_reverse ~f =
      ModuleB.iteri !reverse_map ~f
    method keys =
      ModuleA.keys !forward_map
    method keys_reverse =
      ModuleB.keys !reverse_map
    method length =
      ModuleA.length !forward_map
    method map ~f =
      let () = forward_map := (ModuleA.map !forward_map ~f) in
      self#create_reverse_map_from_forward_map ()
    method map_reverse ~f =
      let () = reverse_map := (ModuleB.map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map () 
    method mapi ~f =
      let () = forward_map := (Core.mapi !forward_map ~f) in
      self#create_reverse_map_from_forward_map ()
    method mapi_reverse ~f =
      let () = reverse_map := (Core.mapi !reverse_map ~f) in
      self#create_forward_map_from_reverse_map ()     
    method mem key =
      Core.mem !forward_map key
    method mem_reverse key =
      Core.mem !reverse_map key
    method min_elt =
      Core.min_elt !forward_map
    method min_elt_exn =
      Core.min_elt_exn !forward_map
    method min_elt_reverse =
      Core.min_elt !reverse_map
    method min_elt_exn_reverse =
      Core.min_elt_exn !reverse_map
    method max_elt =
      Core.max_elt !forward_map
    method max_elt_exn =
      Core.max_elt_exn !forward_map
    method max_elt_reverse =
      Core.max_elt !reverse_map
    method max_elt_exn_reverse =
      Core.max_elt_exn !reverse_map
    method nth int =
      Core.nth !forward_map int
    method nth_reverse int =
      Core.nth !reverse_map int
    method remove ~key =
      let reverse_keys = Core.find_exn !forward_map key in 
      let () = forward_map := (Core.remove !forward_map key) in
      self#remove_reverse_keys reverse_keys
    method private remove_reverse_keys klist = 
      let rec remove_keys k =
	match k with
	| h :: t ->
	   let () = reverse_map := (Core.remove !reverse_map h) in
	   remove_keys t
	| [] -> () in
      remove_keys klist
    method remove_reverse ~key =
      let fwd_keys = Core.find_exn !reverse_map key in 
      let () = reverse_map := (Core.remove !reverse_map key) in
      Core.List.iter fwd_keys
        ~f:(fun k ->
          let fwd_values = self#find_exn ~key:k in
          let new_fwd_values = (Core.List.filter fwd_values ~f:(fun x -> not (x = key))) in
          let () = forward_map := (Core.remove !forward_map k) in
          forward_map := (Core.set !forward_map  ~key:k ~data:new_fwd_values)
        )
    method remove_multi ~key =
      try
	let values = ModuleA.find_exn !forward_map key in
	let head_element = Core.List.nth_exn values 0 in 
	let () = forward_map := (ModuleA.remove_multi !forward_map key) in
	(*using head_element: if reverse_map binds head_element only to key then remove it else filter out key*)
        self#remove_fwd_key_from_reverse_map ~fwd_values_list:[head_element] ~key
        (*--TODO--improve exception handling*)
      with _e -> raise (Failure "bimap_multi::remove_multi() failed")
    method remove_reverse_multi ~key =
      try
	let values = ModuleB.find_exn !reverse_map key in
	let head_element = Core.List.nth_exn values 0 in 
	let () = reverse_map := (ModuleB.remove_multi !reverse_map key) in
	(*using head_element: if reverse_map binds head_element only to key then remove it else filter out key*)
        self#remove_rev_key_from_forward_map ~rev_values_list:[head_element] ~key
        (*--TODO--improve exception handling*)
      with _e -> raise (Failure "bimap_multi::remove_reverse_multi() failed")
    method to_alist ?key_order () =
      match Core.Option.is_some key_order with
      | false -> Core.to_alist !forward_map
      | true -> Core.to_alist ~key_order:(Core.Option.value_exn key_order) !forward_map
    (*update and change are identical except that the function f must be of a different type; see Core.Map documentation.*)
    method update ~key ~f =
      let oldvalues = ModuleA.find_exn !forward_map key in
      let () = forward_map := (ModuleA.update !forward_map key ~f) in
      let newvalues = ModuleA.find_exn !forward_map key in
      (*remove or filter mappings in reverse map for oldvalues and then add_multi newvalues to reverse map*)
      let () = self#remove_fwd_key_from_reverse_map ~fwd_values_list:oldvalues ~key in
      Core.List.iter newvalues
        ~f:(fun v -> 
          reverse_map := ModuleB.add_multi !reverse_map ~key:v ~data:key
        )
      *)
  end
end
