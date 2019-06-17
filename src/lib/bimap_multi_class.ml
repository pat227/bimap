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
    method bindings () =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      Bimap_p.bindings newt
    method bindings_reverse () =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      Bimap_p.bindings_reverse newt
                       
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
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.remove newt ~key in 
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt

    method remove_reverse ~key =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.remove_reverse newt ~key in 
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt

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
    method find_reverse_exn ~key =
      ModuleB.find_exn !reverse_map key*)
    method filter ~f =
      let () = forward_map := (ModuleA.filter f !forward_map) in
      (*Since each map is a multi map of 'a list or 'b list, with 'b or 'a as keys, respectively, the ~f used to 
        filter keys in one map cannot be used to filter values in the other.*)
      self#create_reverse_map_from_forward_map
    method filter_reverse ~f =
      let () = reverse_map := (ModuleB.filter f !reverse_map) in
      self#create_forward_map_from_reverse_map

    method fold : 'e. init:'e -> f:(ModuleA.key -> ModuleB.key list -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> ModuleA.fold f !forward_map init)
    method fold_reverse : 'e. init:'e -> f:(ModuleB.key -> ModuleA.key list -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> ModuleB.fold f !reverse_map init)
    method for_all ~f =
      ModuleA.for_all f !forward_map
    method for_all_reverse ~f =
      ModuleB.for_all f !reverse_map
    method is_empty =
      ModuleA.is_empty !forward_map
    method iter ~f =
      ModuleA.iter f !forward_map
    method iter_reverse ~f =
      ModuleB.iter f !reverse_map
    method map ~f =
      let () = forward_map := (ModuleA.map f !forward_map) in
      self#create_reverse_map_from_forward_map ()
    method map_reverse ~f =
      let () = reverse_map := (ModuleB.map f !reverse_map) in
      self#create_forward_map_from_reverse_map () 
    method mapi ~f =
      let () = forward_map := (ModuleA.mapi f !forward_map) in
      self#create_reverse_map_from_forward_map ()
    method mapi_reverse ~f =
      let () = reverse_map := (ModuleB.mapi f !reverse_map) in
      self#create_forward_map_from_reverse_map ()
  end
end
