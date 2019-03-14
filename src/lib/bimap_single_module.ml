(*This functor generates a module, not a class, and has no mutable member, more closely 
  resembling the Map interface*)
module Bimap_single (ModuleA : Core.Comparable.S)(ModuleB : Core.Comparable.S) = struct
    type t = {
      fwdmap : ModuleB.Map.Key.t ModuleA.Map.t;
      revmap : ModuleA.Map.Key.t ModuleB.Map.t
    }
(*    let empty_forward_map () = ModuleA.Map.empty
    let empty_reverse_map () = ModuleB.Map.empty*)
    let set t ~key ~data  =
      let newfmap = ModuleA.Map.set t.fwdmap ~key ~data in
      let newrmap = ModuleB.Map.set t.revmap ~key:data ~data:key in
      {fwdmap=newfmap; revmap=newrmap}
    let set_reverse t ~key ~data =
      let newrmap = ModuleB.Map.set t.revmap ~key ~data in
      let newfmap = ModuleA.Map.set t.fwdmap ~key:data ~data:key in
      {fwdmap=newfmap; revmap=newrmap}
    let change t ~key ~f =
      let old_value = ModuleA.Map.find_exn t.fwdmap key in 
      let newfmap = ModuleA.Map.change t.fwdmap key ~f in
      let new_value = ModuleA.Map.find_exn newfmap key in 
      let newrmap = ModuleB.Map.set t.revmap ~key:new_value ~data:key in
      let newrmap = ModuleB.Map.remove newrmap old_value in
      {fwdmap=newfmap; revmap=newrmap}
    let change_reverse t ~key ~f =
      let old_key = ModuleB.Map.find_exn t.revmap key in 
      let newrmap = ModuleB.Map.change t.revmap key ~f in
      let new_value = ModuleB.Map.find_exn newrmap key in 
      let newfmap = ModuleA.Map.set t.fwdmap ~key:new_value ~data:key in
      let newfmap = ModuleA.Map.remove newfmap old_key in
      {fwdmap=newfmap; revmap=newrmap}
    let create_reverse_map_from_forward_map ~forward_map =
      let newrmap = ModuleB.Map.empty in
      let newrmapref = ref newrmap in 
      (*iteri fits the bill better than iter keys*)
      let () = ModuleA.Map.iteri
	         ~f:(fun ~key ~data ->
	           newrmapref :=
                     (ModuleB.Map.set !newrmapref ~key:data ~data:key))
                 forward_map in
      !newrmapref
    let create_forward_map_from_reverse_map ~reverse_map =
      let newfmap = ModuleA.Map.empty in
      let newfmapref = ref newfmap in 
      let () = ModuleB.Map.iteri 
	         ~f:(fun ~key ~data ->
		   newfmapref :=
                     ModuleA.Map.set !newfmapref ~key:data ~data:key)
                 reverse_map in
      !newfmapref

    let count t ~f =
      ModuleA.Map.count t.fwdmap ~f
    let count_reverse t ~f =
      ModuleB.Map.count t.revmap ~f
    let counti t ~f =
      ModuleA.Map.counti t.fwdmap ~f
    let data t =
      ModuleA.Map.data t.fwdmap
    let data_reverse t =
      ModuleB.Map.data t.revmap
    let empty () =
      {fwdmap=ModuleA.Map.empty; revmap=ModuleB.Map.empty}
    let exists ~forward_map ~f =
      ModuleA.Map.exists forward_map ~f
    let exists_reverse ~reverse_map ~f =
      ModuleB.Map.exists reverse_map ~f
    let existsi ~forward_map ~f =
      ModuleA.Map.existsi forward_map ~f
    let existsi_reverse ~reverse_map ~f =
      ModuleB.Map.existsi reverse_map ~f
    let find ~forward_map ~key =
      ModuleA.Map.find forward_map key
    let find_reverse ~reverse_map ~key =
      ModuleB.Map.find reverse_map key
(*    let find_exn ~key =
      ModuleA.Map.find_exn !forward_map key
    let find_exn_reverse ~key =
      ModuleB.Map.find_exn !reverse_map key
    let filter ~f =
      let () = forward_map := (ModuleA.Map.filter !forward_map ~f) in
      reverse_map := (ModuleB.Map.filter_keys !reverse_map ~f)
    let filter_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter !reverse_map ~f) in
      forward_map := (ModuleA.Map.filter_keys !forward_map ~f)
    let filter_keys ~f =
      let () = forward_map := (ModuleA.Map.filter_keys !forward_map ~f) in
      reverse_map := (ModuleB.Map.filter !reverse_map ~f)
    let filter_keys_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter_keys !reverse_map ~f) in
      forward_map := (ModuleA.Map.filter !forward_map ~f)
    let filteri ~f =
      let () = forward_map := (ModuleA.Map.filteri !forward_map ~f) in
    self#create_reverse_map_from_forward_map
    let filteri_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filteri !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    let filter_map ~f =
      let () = forward_map := (ModuleA.Map.filter_map !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    let filter_map_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter_map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    (*    let forward_map = !forward_map*)
    let fold : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> ModuleA.Map.fold !forward_map ~init ~f)
    let fold_reverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.Map.fold !reverse_map ~init ~f)
(*    let fold_range_inclusive ~min ~max ~init ~f =
      Core.Map.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    let fold_right : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleA.Map.fold_right !forward_map ~init ~f)
    let fold_right_reverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.Map.fold_right !reverse_map ~init ~f)
    let for_all ~f =
      ModuleA.Map.for_all !forward_map ~f
    let for_all_reverse ~f =
      ModuleB.Map.for_all !reverse_map ~f
    let is_empty =
      ModuleA.Map.is_empty !forward_map
 
    let iter_keys ~f =
      ModuleA.Map.iter_keys !forward_map ~f
    let iter_keys_reverse ~f =
      ModuleB.Map.iter_keys !reverse_map ~f
    let iter ~f =
      ModuleA.Map.iter !forward_map ~f
    let iter_reverse ~f =
      ModuleB.Map.iter !reverse_map ~f
    let iteri ~f =
      ModuleA.Map.iteri !forward_map ~f
    let iteri_reverse ~f =
      ModuleB.Map.iteri !reverse_map ~f

    let keys =
      ModuleA.Map.keys !forward_map
    let keys_reverse =
      ModuleB.Map.keys !reverse_map
    let length =
      ModuleA.Map.length !forward_map
    let map ~f =
      let () = forward_map := (ModuleA.Map.map !forward_map ~f) in
      (*===could never get this to work===
       let () = reverse_map :=
		 Core.Map.empty (Core.Map.comparator !reverse_map) in*)
      let () = self#empty_reverse_map () in
      ModuleA.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       ModuleB.Map.set !reverse_map
				    ~key:(ModuleA.Map.find_exn !forward_map k) ~data:k)
    let map_reverse ~f =
      let () = reverse_map := (ModuleB.Map.map !reverse_map ~f) in
      (*let () = forward_map :=
		 Core.Map.empty (Core.Map.comparator !forward_map) in*)
      let () = self#empty_forward_map () in 
      ModuleB.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       ModuleA.Map.set !forward_map
				    ~key:(ModuleB.Map.find_exn !reverse_map k) ~data:k)
    let mapi ~f =
      let () = forward_map := (ModuleA.Map.mapi !forward_map ~f) in
      (*let () = reverse_map :=
		 Core.Map.empty (Core.Map.comparator !reverse_map) in*)
      let () = self#empty_reverse_map () in
      ModuleA.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       ModuleB.Map.set !reverse_map
				    ~key:(ModuleA.Map.find_exn !forward_map k) ~data:k)
    let mapi_reverse ~f =
      let () = reverse_map := (ModuleB.Map.mapi !reverse_map ~f) in
      (*let () = forward_map :=
		 Core.Map.empty (Core.Map.comparator !forward_map) in*)
      let () = self#empty_forward_map () in
      ModuleB.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       ModuleA.Map.set !forward_map
				    ~key:(ModuleB.Map.find_exn !reverse_map k) ~data:k)
    let mem key =
      ModuleA.Map.mem !forward_map key
    let mem_reverse key =
      ModuleB.Map.mem !reverse_map key
    let min_elt =
      ModuleA.Map.min_elt !forward_map
    let min_elt_exn =
      ModuleA.Map.min_elt_exn !forward_map
    let min_elt_reverse =
      ModuleB.Map.min_elt !reverse_map
    let min_elt_exn_reverse =
      ModuleB.Map.min_elt_exn !reverse_map
    let max_elt =
      ModuleA.Map.max_elt !forward_map
    let max_elt_exn =
      ModuleA.Map.max_elt_exn !forward_map
    let max_elt_reverse =
      ModuleB.Map.max_elt !reverse_map
    let max_elt_exn_reverse =
      ModuleB.Map.max_elt_exn !reverse_map
    let nth int =
      ModuleA.Map.nth !forward_map int
    let nth_reverse int =
      ModuleB.Map.nth !reverse_map int
    let remove ~key =
      let reverse_key = ModuleA.Map.find_exn !forward_map key in 
      let () = forward_map := (ModuleA.Map.remove !forward_map key) in
      reverse_map := (ModuleB.Map.remove !reverse_map reverse_key)
    let remove_reverse ~key =
      let fwd_key = ModuleB.Map.find_exn !reverse_map key in 
      let () = reverse_map := (ModuleB.Map.remove !reverse_map key) in
      forward_map := (ModuleA.Map.remove !forward_map fwd_key)
(*    let reverse_map = !reverse_map*)
    let to_alist ?key_order () =
      match Core.Option.is_some key_order with
      | false -> ModuleA.Map.to_alist !forward_map
      | true -> ModuleA.Map.to_alist ~key_order:(Core.Option.value_exn key_order) !forward_map
    let update ~key ~f =
      let oldvalue = ModuleA.Map.find_exn !forward_map key in
      let () = forward_map := (ModuleA.Map.update !forward_map key ~f) in
      let newvalue = ModuleA.Map.find_exn !forward_map key in 
      let () = reverse_map := (ModuleB.Map.set !reverse_map ~key:newvalue ~data:key) in
      reverse_map := (ModuleB.Map.remove !reverse_map oldvalue)
 *)
end
