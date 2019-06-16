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
    method exists ~f =
      ModuleA.exists f !forward_map
    method exists_reverse ~f =
      ModuleB.exists f !reverse_map
    method find ~key =
      ModuleA.find key !forward_map
    method find_reverse ~key =
      ModuleB.find key !reverse_map 
(*    method find_exn ~key =
      ModuleA.find_exn key !forward_map
    method find_exn_reverse ~key =
      ModuleB.find_exn key !reverse_map*)
    (*Since each map is a multi map of 'a list or 'b list, with 'b or 'a as keys, respectively, the ~f used to 
        filter keys in one map cannot be used to filter values in the other.*)
    method filter ~f =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.filter newt ~f in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method filter_reverse ~f =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.filter_reverse newt ~f in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method fold ~f (*: 'e. init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e*) =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.fold ~f newt in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method fold_reverse ~f (*: 'e. init:'e -> f:(key:'b -> data:'a list -> 'e -> 'e) -> 'e*) =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.fold_reverse ~f newt in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method for_all ~f =
      ModuleA.for_all !forward_map ~f
    method for_all_reverse ~f =
      ModuleB.for_all !reverse_map ~f
    method is_empty =
      ModuleA.is_empty !forward_map
    method iter ~f =
      ModuleA.iter !forward_map ~f
    method iter_reverse ~f =
      ModuleB.iter !reverse_map ~f
    method map ~f =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.map ~f newt in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method map_reverse ~f =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.map_reverse ~f newt in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method mapi ~f =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.mapi ~f newt in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method mapi_reverse ~f =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.mapi_reverse ~f newt in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method min_binding =
      ModuleA.min_binding !forward_map
    method min_binding_reverse =
      ModuleB.min_binding !reverse_map
    method max_binding =
      ModuleA.max_binding !forward_map
    method max_binding_reverse =
      ModuleB.max_binding !reverse_map
    (**Remove the head element*)
    method remove_multi ~key =
      try
	let values = ModuleA.find_exn !forward_map key in
	let head_element = List.nth 0 values in
        let new_values = List.tl values in 
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

  end
end
