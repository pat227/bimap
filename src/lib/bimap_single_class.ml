module Bimap_pure = Bimap_single_module.Bimap_single_module
module Bimap_single_class(MapModule1 : Map.S)(MapModule2 : Map.S) = struct
  module Bimap_p = Bimap_pure(MapModule1)(MapModule2)
  class bimap_single_class = object(self)
    val mutable forward_map = ref MapModule1.empty
    val mutable reverse_map = ref MapModule2.empty
    method private empty_forward_map () =
      forward_map := MapModule1.empty
    method private empty_reverse_map () =
      reverse_map := MapModule2.empty
    method empty () =
      let () = self#empty_forward_map () in
      self#empty_reverse_map ()
    method is_empty () =
      MapModule1.is_empty !forward_map
    method is_empty_reverse () =
      MapModule2.is_empty !reverse_map
    method mem k =
      MapModule1.mem k !forward_map
    method mem_reverse k =
      MapModule2.mem k !reverse_map
    method add ~key ~data =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.add newt ~key ~data in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt
    method add_reverse ~key ~data =
      let newt = Bimap_p.create_t ~fwdmap:!forward_map ~revmap:!reverse_map in
      let newt = Bimap_p.add_reverse newt ~key ~data in
      let () = forward_map := Bimap_p.get_forward_map newt in
      reverse_map := Bimap_p.get_reverse_map newt      
    method singleton ~key ~data =
      let () = self#empty () in
      self#add ~key ~data
    method singleton_reverse ~key ~data =
      let () = self#empty () in
      self#add_reverse ~key ~data
    method private create_reverse_map_from_forward_map () =
      let new_reverse_map = Bimap_p.create_reverse_map_from_forward_map ~forward_map:(!forward_map) in
      reverse_map := new_reverse_map
    method private create_forward_map_from_reverse_map () =
      let new_forward_map = Bimap_p.create_forward_map_from_reverse_map ~reverse_map:(!reverse_map) in
      forward_map := new_forward_map
    method remove ~key =
      let value = MapModule1.find key !forward_map in 
      let () = forward_map := MapModule1.remove key !forward_map in
      reverse_map := MapModule2.remove value !reverse_map
    method remove_reverse ~key =
      let fwd_value = MapModule2.find key !reverse_map in 
      let () = reverse_map := MapModule2.remove key !reverse_map in
      forward_map := MapModule1.remove fwd_value !forward_map
    method merge ~(f:MapModule1.key ->
                   MapModule2.key option -> MapModule2.key option -> MapModule2.key option) ~othermap =
      let () = forward_map := MapModule1.merge f !forward_map othermap in
      self#create_reverse_map_from_forward_map ()
    method merge_reverse ~(f:MapModule2.key ->
                           MapModule1.key option -> MapModule1.key option -> MapModule1.key option) ~othermap =
      let () = reverse_map := MapModule2.merge f !reverse_map othermap in
      self#create_forward_map_from_reverse_map ()
    method union ~f ~othermap =
      let () = forward_map := MapModule1.union f !forward_map othermap in
      self#create_reverse_map_from_forward_map ()
    method union_reverse ~f ~othermap =
      let () = reverse_map := MapModule2.union f !reverse_map othermap in
      self#create_forward_map_from_reverse_map ()
    method compare ~f ~othermap =
      MapModule1.compare f !forward_map othermap
    method compare_reverse ~f ~othermap =
      MapModule2.compare f !reverse_map othermap
    method equal ~f ~othermap =
      MapModule1.equal f !forward_map othermap
    method equal_reverse ~f ~othermap =
      MapModule2.equal f !reverse_map othermap
    method iter ~f =
      MapModule1.iter f !forward_map
    method iter_reverse ~f =
      MapModule2.iter f !reverse_map
    method fold ~f ~(init:MapModule2.key) =
      MapModule1.fold f !forward_map init
    method fold_reverse ~f ~(init:MapModule1.key) =
      MapModule2.fold f !reverse_map init
    method for_all ~f =
      MapModule1.for_all f !forward_map
    method for_all_reverse ~f =
      MapModule2.for_all f !reverse_map
    method exists ~f =
      MapModule1.exists f !forward_map
    method exists_reverse ~f =
      MapModule2.exists f !reverse_map
    method filter ~f =
      MapModule1.filter f !forward_map
    method filter_reverse ~f =
      MapModule2.filter f !reverse_map
    method partition ~f =
      MapModule1.partition f !forward_map
    method partition_reverse ~f =
      MapModule2.partition f !reverse_map
    method cardinal () =
      MapModule1.cardinal !forward_map
    method cardinal_reverse () =
      MapModule2.cardinal !reverse_map
    method bindings () =
      MapModule1.bindings !forward_map
    method bindings_reverse () =
      MapModule2.bindings !reverse_map
    method min_binding () =
      MapModule1.min_binding !forward_map
    method min_binding_reverse () =
      MapModule2.min_binding !reverse_map
    method max_binding () =
      MapModule1.max_binding !forward_map
    method max_binding_reverse () =
      MapModule2.max_binding !reverse_map
    method choose () =
      MapModule1.choose !forward_map
    method choose_reverse () =
      MapModule2.choose !reverse_map
    method split ~key =
      MapModule1.split key !forward_map
    method split_reverse ~key =
      MapModule2.split key !reverse_map
    method find ~key =
      MapModule1.find key !forward_map
    method find_reverse ~key =
      MapModule2.find key !reverse_map
    method map ~(f:MapModule2.key -> MapModule2.key) =
      MapModule1.map f !forward_map
    method map_reverse ~(f:MapModule1.key -> MapModule1.key) =
      MapModule2.map f !reverse_map
    method mapi ~(f:MapModule2.key -> MapModule2.key) =
      MapModule1.map f !forward_map
    method mapi_reverse ~(f:MapModule1.key -> MapModule1.key) =
      MapModule2.map f !reverse_map
  end
end 
