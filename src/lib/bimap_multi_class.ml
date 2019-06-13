module Bimap_pure = Bimap_multi_module.Bimap_multi_module
module Bimap_multi_class (ModuleA : Map.S)(ModuleB : Map.S) = struct
  module Bimap_p = Bimap_pure(ModuleA)(ModuleB)
  class bimap_multi_class = object(self)
    val mutable forward_map = ref ModuleA.empty
    val mutable reverse_map = ref ModuleB.empty
    method private empty_forward_map () =
      forward_map := ModuleA.empty
    method private empty_reverse_map () =
      reverse_map := ModuleB.empty

    method add_multi ~key ~data =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.add newt ~key ~data in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method add_multi_reverse ~key ~data =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.add_reverse newt ~key ~data in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt

    method private create_reverse_map_from_forward_map () =
      let newrevmap = Bimap_p.create_reverse_map_from_forward_map !forward_map in
      reverse_map := newrevmap

    method private create_forward_map_from_reverse_map () =
      let newfwdmap = Bimap_p.create_forward_map_from_reverse_map !reverse_map in
      forward_map := newfwdmap

    method cardinal () =
      ModuleA.cardinal !forward_map
    method cardinal_reverse () =
      ModuleB.cardinal !reverse_map
    method empty () =
      let () = self#empty_forward_map () in
      self#empty_reverse_map ()
    method mem key =
      ModuleA.mem key !forward_map
    method mem_reverse key =
      ModuleB.mem key !reverse_map
    method update ~key ~f =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.update newt ~key ~f in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method update_reverse ~key ~f =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.update_reverse newt ~key ~f in 
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method singleton ~key ~data =
      let () = self#empty_forward_map () in
      let () = self#empty_reverse_map () in
      self#add_multi ~key ~data
    method singleton_reverse ~key ~data =
      let () = self#empty_forward_map () in
      let () = self#empty_reverse_map () in
      self#add_multi_reverse ~key ~data
                             
    method remove ~key =
      if ModuleA.mem key !forward_map then 
        let fwd_values_list = ModuleA.find key !forward_map in 
        let () = forward_map := (ModuleA.remove key !forward_map) in
        let newrevmap = Bimap_p.remove_fwd_key_from_reverse_map !reverse_map ~fwd_values_list ~key in
        reverse_map := newrevmap
      else ()

    method remove_reverse ~key =
      if ModuleB.mem key !reverse_map then 
        let rev_values_list = ModuleB.find key !reverse_map in 
        let () = reverse_map := (ModuleB.remove key !reverse_map) in
        let newfwdmap = Bimap_p.remove_rev_key_from_forward_map !forward_map ~rev_values_list ~key in
        forward_map := newfwdmap
      else ()

    method merge f ~(othermap:ModuleB.key list ModuleA.t) =
      let () = forward_map := ModuleA.merge f !forward_map othermap in
      self#create_reverse_map_from_forward_map ()
    method merge_reverse f ~(othermap:ModuleA.key list ModuleB.t) =
      let () = reverse_map := ModuleB.merge f !reverse_map othermap in
      self#create_forward_map_from_reverse_map ()
    method union f ~(othermap:ModuleB.key list ModuleA.t) =
      let () = forward_map := ModuleA.union f !forward_map othermap in
      self#create_reverse_map_from_forward_map ()
    method union_reverse f ~(othermap:ModuleA.key list ModuleB.t) =
      let () = reverse_map := ModuleB.union f !reverse_map othermap in
      self#create_forward_map_from_reverse_map ()
    method compare f ~othermap =
      ModuleA.compare f !forward_map othermap
    method compare_reverse f ~othermap =
      ModuleB.compare f !reverse_map othermap
    method equal f ~othermap =
      ModuleA.equal f !forward_map othermap
    method equal_reverse f ~othermap =
      ModuleB.equal f !reverse_map othermap
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

      *)
  end
end
