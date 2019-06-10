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

(*
====================================================================
  class ['a,'b] bimap_class m1 m2 = object(self)
    val mutable forward_map = ref m1
    val mutable reverse_map = ref m2
    method private empty_forward_map () =
      forward_map := (!forward_map).empty
    method private empty_reverse_map () =
      reverse_map := Map.empty
    method add ~key ~data =
      let () = forward_map := Map.add ~key ~data !forward_map in 
      reverse_map := Map.add ~key:data ~data:key !reverse_map
    method set_inverse ~key ~data =
      let () = reverse_map := (Map.set !reverse_map ~key ~data) in
      forward_map := Map.set !forward_map ~key:data ~data:key
    method private create_inverse_map_from_forward_map =
      let open Sexplib.Std in
      let open Sexplib in
      (*===could never get this to work===
      let comp = Map.comparator !reverse_map in
      let () = reverse_map := (Map.empty comp) in*)
      let () = self#empty_reverse_map () in
      self#iter_keys
	     ~f:(fun k ->
		 reverse_map :=
		   Map.set !reverse_map ~key:(self#find_exn ~key:k) ~data:k)
    method private create_forward_map_from_reverse_map =
      (* ==== could never get this to work ===
      let () = forward_map :=
		 (Map.empty (Map.comparator !forward_map)) in*)
      let () = self#empty_forward_map () in 
      self#iter_keys_inverse
	     ~f:(fun k ->
		 forward_map :=
		   Map.set !forward_map
				~key:(self#find_exn_inverse ~key:k) ~data:k)
    method cardinal () =
      Map.cardinal !forward_map
    method cardinal_inverse () =
      Map.cardinal !reverse_map
    method counti ~f =
      Map.counti !forward_map ~f
    method data =
      Map.data !forward_map
    method data_inverse =
      Map.data !reverse_map
    method empty () =
      (*===could never get this to work===
      let () = forward_map :=
		 (Map.empty (Map.comparator !forward_map)) in
      reverse_map :=
	(Map.empty (Map.comparator !reverse_map)) *)
      let () = self#empty_forward_map () in 
      self#empty_reverse_map () 
(*    method equal f ~other_bimap =
      Map.equal f !forward_map !other_fwd_map*)
    method exists ~f =
      Map.exists !forward_map ~f
    method exists_inverse ~f =
      Map.exists !reverse_map ~f
    method existsi ~f =
      Map.existsi !forward_map ~f
    method existsi_inverse ~f =
      Map.existsi !reverse_map ~f
    method find ~key =
      Map.find !forward_map key
    method find_inverse ~key =
      Map.find !reverse_map key
    method find_exn ~key =
      Map.find_exn !forward_map key
    method find_exn_inverse ~key =
      Map.find_exn !reverse_map key
    method filter ~f =
      let () = forward_map := (Map.filter !forward_map ~f) in
      reverse_map := (Map.filter_keys !reverse_map ~f)
    method filter_inverse ~f =
      let () = reverse_map := (Map.filter !reverse_map ~f) in
      forward_map := (Map.filter_keys !forward_map ~f)
    method filter_keys ~f =
      let () = forward_map := (Map.filter_keys !forward_map ~f) in
      reverse_map := (Map.filter !reverse_map ~f)
    method filter_keys_inverse ~f =
      let () = reverse_map := (Map.filter_keys !reverse_map ~f) in
      forward_map := (Map.filter !forward_map ~f)
    method filteri ~f =
      let () = forward_map := (Map.filteri !forward_map ~f) in
      self#create_inverse_map_from_forward_map
    method filteri_inverse ~f =
      let () = reverse_map := (Map.filteri !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_map ~f =
      let () = forward_map := (Map.filter_map !forward_map ~f) in
      self#create_inverse_map_from_forward_map
    method filter_map_inverse ~f =
      let () = reverse_map := (Map.filter_map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    (*    method forward_map = !forward_map*)
    method fold : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> Map.fold !forward_map ~init ~f)
    method fold_inverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> Map.fold !reverse_map ~init ~f)
(*    method fold_range_inclusive ~min ~max ~init ~f =
      Map.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    method fold_right : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> Map.fold_right !forward_map ~init ~f)
    method fold_right_inverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> Map.fold_right !reverse_map ~init ~f)
    method for_all ~f =
      Map.for_all !forward_map ~f
    method for_all_inverse ~f =
      Map.for_all !reverse_map ~f
    method is_empty =
      Map.is_empty !forward_map
    method iter_keys ~f =
      Map.iter_keys !forward_map ~f
    method iter_keys_inverse ~f =
      Map.iter_keys !reverse_map ~f
    method iter ~f =
      Map.iter !forward_map ~f
    method iter_inverse ~f =
      Map.iter !reverse_map ~f
    method iteri ~f =
      Map.iteri !forward_map ~f
    method iteri_inverse ~f =
      Map.iteri !reverse_map ~f
    method keys =
      Map.keys !forward_map
    method keys_inverse =
      Map.keys !reverse_map
    method length =
      Map.length !forward_map
    method map ~f =
      let () = forward_map := (Map.map !forward_map ~f) in
      (*===could never get this to work===
       let () = reverse_map :=
		 Map.empty (Map.comparator !reverse_map) in*)
      let () = self#empty_reverse_map () in 
      Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       Map.set !reverse_map
				    ~key:(Map.find_exn !forward_map k) ~data:k)
    method map_inverse ~f =
      let () = reverse_map := (Map.map !reverse_map ~f) in
      (*let () = forward_map :=
		 Map.empty (Map.comparator !forward_map) in*)
      let () = self#empty_forward_map () in 
      Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Map.set !forward_map
				    ~key:(Map.find_exn !reverse_map k) ~data:k)
    method mapi ~f =
      let () = forward_map := (Map.mapi !forward_map ~f) in
      (*let () = reverse_map :=
		 Map.empty (Map.comparator !reverse_map) in*)
      let () = self#empty_reverse_map () in 
      Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       Map.set !reverse_map
				    ~key:(Map.find_exn !forward_map k) ~data:k)
    method mapi_inverse ~f =
      let () = reverse_map := (Map.mapi !reverse_map ~f) in
      (*let () = forward_map :=
		 Map.empty (Map.comparator !forward_map) in*)
      let () = self#empty_forward_map () in
      Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Map.set !forward_map
				    ~key:(Map.find_exn !reverse_map k) ~data:k)
    method mem key =
      Map.mem !forward_map key
    method mem_inverse key =
      Map.mem !reverse_map key
    method min_elt =
      Map.min_elt !forward_map
    method min_elt_exn =
      Map.min_elt_exn !forward_map
    method min_elt_inverse =
      Map.min_elt !reverse_map
    method min_elt_exn_inverse =
      Map.min_elt_exn !reverse_map
    method max_elt =
      Map.max_elt !forward_map
    method max_elt_exn =
      Map.max_elt_exn !forward_map
    method max_elt_inverse =
      Map.max_elt !reverse_map
    method max_elt_exn_inverse =
      Map.max_elt_exn !reverse_map
    method nth int =
      Map.nth !forward_map int
    method nth_inverse int =
      Map.nth !reverse_map int
    method remove ~key =
      let reverse_key = Map.find_exn !forward_map key in 
      let () = forward_map := (Map.remove !forward_map key) in
      reverse_map := (Map.remove !reverse_map reverse_key)
    method remove_inverse ~key =
      let fwd_key = Map.find_exn !reverse_map key in 
      let () = reverse_map := (Map.remove !reverse_map key) in
      forward_map := (Map.remove !forward_map fwd_key)
(*    method reverse_map = !reverse_map*)
    method update key ~f =
      let oldvalue = Map.find_exn !forward_map key in
      let () = forward_map := (Map.update !forward_map key ~f) in
      let newvalue = Map.find_exn !forward_map key in 
      let () = reverse_map := (Map.set !reverse_map newvalue key) in
      reverse_map := (Map.remove !reverse_map oldvalue)
    method to_alist ?key_order () =
      match Option.is_some key_order with
      | false -> Map.to_alist !forward_map
      | true -> Map.to_alist ~key_order:(Option.value_exn key_order) !forward_map
  end
*)

(*  type ('a, 'b) t = ('a, 'b) bimap_class
  let make mapa mapb = new bimap_class mapa mapb*)

  (*=====CANNOT YET FIGURE OUT HOW TO GET THIS TO WORK OUTSIDE THIS MODULE============
   ACTUALLY THIS IS IMPOSSIBLE EVEN HERE BECAUSE multi map requires 'b list not 'b
   AND SO WE CANNOT EXPRESS the FOLD METHOD because we can only override the method,
   but we need to redefine the type of the fold method itself, which appears to not
   be permitted. 
   ==================================================================================
   
  class ['a,'b] bimap_multi_class m1 m2 = object(self)
    inherit ['a, 'b] bimap_class m1 m2 as super
    method add_multi ~(key:'a) ~(data:'b) =
      let () = forward_map := (Core.Map.add_multi !forward_map ~key ~data) in
      reverse_map := Core.Map.add !reverse_map ~key:data ~data:key
    method add_multi_inverse ~(key:'b) ~(data:'a) =
      let () = forward_map := (Core.Map.add_multi !forward_map ~key:data ~data:key) in
      reverse_map := Core.Map.add !reverse_map ~key ~data
    method private add_inverse_values klist v =
      let rec add_values k v =
	match k with
	| h :: t ->
	   let () = reverse_map := (Core.Map.add !reverse_map ~key:h ~data:v) in
	   add_values t v
	| [] -> () in
      add_values klist v
    method change ~key ~f =
      let old_value_list = Core.Map.find_exn !forward_map key in 
      let () = forward_map := (Core.Map.change !forward_map key ~f) in
      let new_values = Core.Map.find_exn !forward_map key in
      let () = self#add_inverse_values new_values key in 
      self#remove_inverse_keys old_value_list		  
    method change_inverse ~key ~f =
      let old_key = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.change !reverse_map key ~f) in
      let new_value = Core.Map.find_exn !reverse_map key in 
      let () = forward_map := (Core.Map.add_multi !forward_map ~key:new_value ~data:key) in
      forward_map := (Core.Map.remove !forward_map old_key)
		       
    method private create_inverse_map_from_forward_map =
      let () = reverse_map :=
		 (Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map)) in
      self#iter_keys
	     ~f:(fun k ->
		 let values = self#find_exn ~key:k in
		 let rec helper v2bkeys =
		   match v2bkeys with
		   | [] -> ()
		   | h::t ->
		      let () = reverse_map :=
				 Core.Map.add !reverse_map ~key:h ~data:k in
		      helper t in
		 helper values)
    method private create_forward_map_from_reverse_map =
      let () = forward_map :=
		 (Core.Map.empty ~comparator:(Core.Map.comparator !forward_map)) in
      self#iter_keys_inverse
	     ~f:(fun k ->
		 forward_map :=
		   Core.Map.add_multi !forward_map
				      ~key:(self#find_exn_inverse ~key:k) ~data:k)
    method filter ~f =
      let () = forward_map := (Core.Map.filter !forward_map ~f) in
      (*in bimap_multi, unlike bimap, the filter function cannot 
        be applied to the inverse map through filter_keys function*)
      self#create_inverse_map_from_forward_map
    method filter_inverse ~f =
      let () = reverse_map := (Core.Map.filter !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_keys ~f =
      let () = forward_map := (Core.Map.filter_keys !forward_map ~f) in
      self#create_inverse_map_from_forward_map
    method filter_keys_inverse ~f =
      let () = reverse_map := (Core.Map.filter_keys !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filteri ~f =
      let () = forward_map := (Core.Map.filteri !forward_map ~f) in
      self#create_inverse_map_from_forward_map
    method filteri_inverse ~f =
      let () = reverse_map := (Core.Map.filteri !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_map ~f =
      let () = forward_map := (Core.Map.filter_map !forward_map ~f) in
      self#create_inverse_map_from_forward_map
    method filter_map_inverse ~f =
      let () = reverse_map := (Core.Map.filter_map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method fold : 'e. init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> Core.Map.fold !forward_map ~init ~f)
    method fold_right : 'e. init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> Core.Map.fold_right !forward_map ~init ~f)
    method map ~f =
      let () = forward_map := (Core.Map.map !forward_map ~f) in
      let () = reverse_map := Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map) in
      Core.Map.iter_keys
	!forward_map
	~f:(fun k ->
	    self#add_inverse_values (Core.Map.find_exn !forward_map k) k)
    method map_inverse ~f =
      let () = reverse_map := (Core.Map.map !reverse_map ~f) in
      let () = forward_map := Core.Map.empty ~comparator:(Core.Map.comparator !forward_map) in
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.add_multi !forward_map ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method mapi ~f =
      let () = forward_map := (Core.Map.mapi !forward_map ~f) in
      let () = reverse_map := Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map) in
      Core.Map.iter_keys
	!forward_map
	~f:(fun k ->
	    self#add_inverse_values (Core.Map.find_exn !forward_map k) k)
    method mapi_inverse ~f =
      let () = reverse_map := (Core.Map.mapi !reverse_map ~f) in
      let () = forward_map := Core.Map.empty ~comparator:(Core.Map.comparator !forward_map) in
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.add_multi !forward_map ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method private remove_inverse_keys klist = 
      let rec remove_keys k =
	match k with
	| h :: t ->
	   let () = reverse_map := (Core.Map.remove !reverse_map h) in
	   remove_keys t
	| [] -> () in
      remove_keys klist
    method remove ~key =
      let reverse_keys = Core.Map.find_exn !forward_map key in 
      let () = forward_map := (Core.Map.remove !forward_map key) in
      self#remove_inverse_keys reverse_keys
    method remove_inverse ~key =
      let fwd_key = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.remove !reverse_map key) in
      let fwd_values = self#find_exn ~key:fwd_key in
      let new_fwd_values = (Core.List.filter fwd_values ~f:(fun x -> not (x = key))) in 
      let () = forward_map := (Core.Map.remove !forward_map fwd_key) in
      forward_map := (Core.Map.add !forward_map  ~key:fwd_key ~data:new_fwd_values)
    method remove_multi ~key =
      try
	let values = Core.Map.find_exn !forward_map key in
	let head_element = Core.List.nth_exn values 0 in 
	let () = forward_map := (Core.Map.remove_multi !forward_map key) in
	reverse_map := (Core.Map.remove !reverse_map head_element)
      with e -> ()
    method update key ~f =
      let oldvalues = Core.Map.find_exn !forward_map key in
      let () = forward_map := (Core.Map.update !forward_map key ~f) in
      let newvalues = Core.Map.find_exn !forward_map key in 
      let () = self#add_inverse_values newvalues key in
      self#remove_inverse_keys oldvalues
  end
end 
*)
(*  
    
    method fold_inverse ~init ~f =
      Core.Map.fold !reverse_map ~init ~f

 *)
(*Cannot do these right now--type 'c the compraator is unbound
    method comparator () =
      Core.Map.comparator !forward_map
    method comparator_inverse () =
      Core.Map.comparator !reverse_map
*)
