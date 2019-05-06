(*do this twice; once with objects/classes and once with modules
  --m1 and m2 are empty maps that need to be provided by client code-- *)
(*Should made this a functor building on Comparable.S ... next version todo*)
module Bimap_single_class (ModuleA : Core.Comparable.S)(ModuleB : Core.Comparable.S) = struct
  class ['a, 'b] bimap_single_class (m1 : 'b ModuleA.Map.t) (m2 : 'a ModuleB.Map.t) = object(self)
    val mutable forward_map : ('b ModuleA.Map.t ref) = ref (m1 : 'b ModuleA.Map.t)
    val mutable reverse_map : ('a ModuleB.Map.t ref) = ref (m2 : 'a ModuleB.Map.t)

    method private empty_forward_map () =
      forward_map := ModuleA.Map.empty
    method private empty_reverse_map () =
      reverse_map := ModuleB.Map.empty
    method set ~key ~data =
      let () = forward_map := ModuleA.Map.set !forward_map ~key ~data in
      reverse_map := ModuleB.Map.set !reverse_map ~key:data ~data:key
    method set_reverse ~key ~data =
      let () = reverse_map := (ModuleB.Map.set !reverse_map ~key ~data) in
      forward_map := ModuleA.Map.set !forward_map ~key:data ~data:key
    method change ~key ~f =
      let old_value = ModuleA.Map.find_exn !forward_map key in 
      let () = forward_map := (ModuleA.Map.change !forward_map key ~f) in
      let new_value = ModuleA.Map.find_exn !forward_map key in 
      let () = reverse_map :=
		 (ModuleB.Map.set !reverse_map ~key:new_value ~data:key) in
      reverse_map := (ModuleB.Map.remove !reverse_map old_value)
    method change_reverse ~key ~f =
      let old_key = ModuleB.Map.find_exn !reverse_map key in 
      let () = reverse_map := (ModuleB.Map.change !reverse_map key ~f) in
      let new_value = ModuleB.Map.find_exn !reverse_map key in 
      let () = forward_map :=
		 (ModuleA.Map.set !forward_map ~key:new_value ~data:key) in
      forward_map := (ModuleA.Map.remove !forward_map old_key)

    method private create_reverse_map_from_forward_map =
      (*===could never get this to work===
      let comp = Core.Map.comparator !reverse_map in
      let () = reverse_map := (Core.Map.empty comp) in*)
      let () = self#empty_reverse_map () in
      self#iter_keys
	     ~f:(fun k ->
		 reverse_map :=
		   ModuleB.Map.set !reverse_map ~key:(self#find_exn ~key:k) ~data:k)
    method private create_forward_map_from_reverse_map =
      (* ==== could never get this to work ===
      let () = forward_map :=
		 (Core.Map.empty (Core.Map.comparator !forward_map)) in*)
      let () = self#empty_forward_map () in 
      self#iter_keys_reverse
	     ~f:(fun k ->
		 forward_map :=
		   ModuleA.Map.set !forward_map
		     ~key:(self#find_exn_reverse ~key:k) ~data:k)

    method count ~f =
      ModuleA.Map.count !forward_map ~f
    method count_reverse ~f =
      ModuleB.Map.count !reverse_map ~f
    method counti ~f =
      ModuleA.Map.counti !forward_map ~f
    method data =
      ModuleA.Map.data !forward_map
    method data_reverse =
      ModuleB.Map.data !reverse_map
    method empty () =
      (*===could never get this to work===
      let () = forward_map :=
		 (Core.Map.empty (Core.Map.comparator !forward_map)) in
      reverse_map :=
	(Core.Map.empty (Core.Map.comparator !reverse_map)) *)
      let () = self#empty_forward_map () in 
      self#empty_reverse_map () 
(*    method equal f ~other_bimap =
      Core.Map.equal f !forward_map !other_fwd_map*)
    method exists ~f =
      ModuleA.Map.exists !forward_map ~f
    method exists_reverse ~f =
      ModuleB.Map.exists !reverse_map ~f
    method existsi ~f =
      ModuleA.Map.existsi !forward_map ~f
    method existsi_reverse ~f =
      ModuleB.Map.existsi !reverse_map ~f
    method find ~key =
      ModuleA.Map.find !forward_map key
    method find_reverse ~key =
      ModuleB.Map.find !reverse_map key
    method find_exn ~key =
      ModuleA.Map.find_exn !forward_map key
    method find_exn_reverse ~key =
      ModuleB.Map.find_exn !reverse_map key
    method filter ~f =
      let () = forward_map := (ModuleA.Map.filter !forward_map ~f) in
      reverse_map := (ModuleB.Map.filter_keys !reverse_map ~f)
    method filter_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter !reverse_map ~f) in
      forward_map := (ModuleA.Map.filter_keys !forward_map ~f)
    method filter_keys ~f =
      let () = forward_map := (ModuleA.Map.filter_keys !forward_map ~f) in
      reverse_map := (ModuleB.Map.filter !reverse_map ~f)
    method filter_keys_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter_keys !reverse_map ~f) in
      forward_map := (ModuleA.Map.filter !forward_map ~f)
    method filteri ~f =
      let () = forward_map := (ModuleA.Map.filteri !forward_map ~f) in
    self#create_reverse_map_from_forward_map
    method filteri_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filteri !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_map ~f =
      let () = forward_map := (ModuleA.Map.filter_map !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filter_map_reverse ~f =
      let () = reverse_map := (ModuleB.Map.filter_map !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    (*    method forward_map = !forward_map*)
    method fold : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> ModuleA.Map.fold !forward_map ~init ~f)
    method fold_reverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.Map.fold !reverse_map ~init ~f)
(*    method fold_range_inclusive ~min ~max ~init ~f =
      Core.Map.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    method fold_right : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleA.Map.fold_right !forward_map ~init ~f)
    method fold_right_reverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> ModuleB.Map.fold_right !reverse_map ~init ~f)
    method for_all ~f =
      ModuleA.Map.for_all !forward_map ~f
    method for_all_reverse ~f =
      ModuleB.Map.for_all !reverse_map ~f
    method is_empty =
      ModuleA.Map.is_empty !forward_map
 
    method iter_keys ~f =
      ModuleA.Map.iter_keys !forward_map ~f
    method iter_keys_reverse ~f =
      ModuleB.Map.iter_keys !reverse_map ~f
    method iter ~f =
      ModuleA.Map.iter !forward_map ~f
    method iter_reverse ~f =
      ModuleB.Map.iter !reverse_map ~f
    method iteri ~f =
      ModuleA.Map.iteri !forward_map ~f
    method iteri_reverse ~f =
      ModuleB.Map.iteri !reverse_map ~f

    method keys =
      ModuleA.Map.keys !forward_map
    method keys_reverse =
      ModuleB.Map.keys !reverse_map
    method length =
      ModuleA.Map.length !forward_map
    method map ~f =
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
    method map_reverse ~f =
      let () = reverse_map := (ModuleB.Map.map !reverse_map ~f) in
      (*let () = forward_map :=
		 Core.Map.empty (Core.Map.comparator !forward_map) in*)
      let () = self#empty_forward_map () in 
      ModuleB.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       ModuleA.Map.set !forward_map
				    ~key:(ModuleB.Map.find_exn !reverse_map k) ~data:k)
    method mapi ~f =
      let () = forward_map := (ModuleA.Map.mapi !forward_map ~f) in
      (*let () = reverse_map :=
		 Core.Map.empty (Core.Map.comparator !reverse_map) in*)
      let () = self#empty_reverse_map () in
      ModuleA.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       ModuleB.Map.set !reverse_map
				    ~key:(ModuleA.Map.find_exn !forward_map k) ~data:k)
    method mapi_reverse ~f =
      let () = reverse_map := (ModuleB.Map.mapi !reverse_map ~f) in
      (*let () = forward_map :=
		 Core.Map.empty (Core.Map.comparator !forward_map) in*)
      let () = self#empty_forward_map () in
      ModuleB.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       ModuleA.Map.set !forward_map
				    ~key:(ModuleB.Map.find_exn !reverse_map k) ~data:k)
    method mem key =
      ModuleA.Map.mem !forward_map key
    method mem_reverse key =
      ModuleB.Map.mem !reverse_map key
    method min_elt =
      ModuleA.Map.min_elt !forward_map
    method min_elt_exn =
      ModuleA.Map.min_elt_exn !forward_map
    method min_elt_reverse =
      ModuleB.Map.min_elt !reverse_map
    method min_elt_exn_reverse =
      ModuleB.Map.min_elt_exn !reverse_map
    method max_elt =
      ModuleA.Map.max_elt !forward_map
    method max_elt_exn =
      ModuleA.Map.max_elt_exn !forward_map
    method max_elt_reverse =
      ModuleB.Map.max_elt !reverse_map
    method max_elt_exn_reverse =
      ModuleB.Map.max_elt_exn !reverse_map
    method nth int =
      ModuleA.Map.nth !forward_map int
    method nth_reverse int =
      ModuleB.Map.nth !reverse_map int
    method remove ~key =
      let reverse_key = ModuleA.Map.find_exn !forward_map key in 
      let () = forward_map := (ModuleA.Map.remove !forward_map key) in
      reverse_map := (ModuleB.Map.remove !reverse_map reverse_key)
    method remove_reverse ~key =
      let fwd_key = ModuleB.Map.find_exn !reverse_map key in 
      let () = reverse_map := (ModuleB.Map.remove !reverse_map key) in
      forward_map := (ModuleA.Map.remove !forward_map fwd_key)
(*    method reverse_map = !reverse_map*)
    method to_alist ?key_order () =
      match Core.Option.is_some key_order with
      | false -> ModuleA.Map.to_alist !forward_map
      | true -> ModuleA.Map.to_alist ~key_order:(Core.Option.value_exn key_order) !forward_map
    method update ~key ~f =
      let oldvalue = ModuleA.Map.find_exn !forward_map key in
      let () = forward_map := (ModuleA.Map.update !forward_map key ~f) in
      let newvalue = ModuleA.Map.find_exn !forward_map key in 
      let () = reverse_map := (ModuleB.Map.set !reverse_map ~key:newvalue ~data:key) in
      reverse_map := (ModuleB.Map.remove !reverse_map oldvalue)
  end
end

(*  type ('a, 'b) t = ('a, 'b) bimap_class
  let make mapa mapb = new bimap_class mapa mapb*)

  (*=====CANNOT YET FIGURE OUT HOW TO GET THIS TO WORK OUTSIDE THIS MODULE============
   ACTUALLY THIS IS IMPOSSIBLE EVEN HERE BECAUSE multi map requires 'b list not 'b
   AND SO WE CANNOT EXPRESS the FOLD METHOD because we can only override the method,
   but we need to redefine the type of the fold method itself, which appears to not
   be permitted. 
   ==================================================================================
   
  class ['a,'b] bimap_multi_class forward_map reverse_map = object(self)
    inherit ['a, 'b] bimap_class forward_map reverse_map as super
    method add_multi ~(key:'a) ~(data:'b) =
      let () = forward_map := (Core.Map.add_multi !forward_map ~key ~data) in
      reverse_map := Core.Map.add !reverse_map ~key:data ~data:key
    method add_multi_reverse ~(key:'b) ~(data:'a) =
      let () = forward_map := (Core.Map.add_multi !forward_map ~key:data ~data:key) in
      reverse_map := Core.Map.add !reverse_map ~key ~data
    method private add_reverse_values klist v =
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
      let () = self#add_reverse_values new_values key in 
      self#remove_reverse_keys old_value_list		  
    method change_reverse ~key ~f =
      let old_key = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.change !reverse_map key ~f) in
      let new_value = Core.Map.find_exn !reverse_map key in 
      let () = forward_map := (Core.Map.add_multi !forward_map ~key:new_value ~data:key) in
      forward_map := (Core.Map.remove !forward_map old_key)
		       
    method private create_reverse_map_from_forward_map =
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
      self#iter_keys_reverse
	     ~f:(fun k ->
		 forward_map :=
		   Core.Map.add_multi !forward_map
				      ~key:(self#find_exn_reverse ~key:k) ~data:k)
    method filter ~f =
      let () = forward_map := (Core.Map.filter !forward_map ~f) in
      (*in bimap_multi, unlike bimap, the filter function cannot 
        be applied to the reverse map through filter_keys function*)
      self#create_reverse_map_from_forward_map
    method filter_reverse ~f =
      let () = reverse_map := (Core.Map.filter !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_keys ~f =
      let () = forward_map := (Core.Map.filter_keys !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filter_keys_reverse ~f =
      let () = reverse_map := (Core.Map.filter_keys !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filteri ~f =
      let () = forward_map := (Core.Map.filteri !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filteri_reverse ~f =
      let () = reverse_map := (Core.Map.filteri !reverse_map ~f) in
      self#create_forward_map_from_reverse_map
    method filter_map ~f =
      let () = forward_map := (Core.Map.filter_map !forward_map ~f) in
      self#create_reverse_map_from_forward_map
    method filter_map_reverse ~f =
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
	    self#add_reverse_values (Core.Map.find_exn !forward_map k) k)
    method map_reverse ~f =
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
	    self#add_reverse_values (Core.Map.find_exn !forward_map k) k)
    method mapi_reverse ~f =
      let () = reverse_map := (Core.Map.mapi !reverse_map ~f) in
      let () = forward_map := Core.Map.empty ~comparator:(Core.Map.comparator !forward_map) in
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.add_multi !forward_map ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method private remove_reverse_keys klist = 
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
      self#remove_reverse_keys reverse_keys
    method remove_reverse ~key =
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
      let () = self#add_reverse_values newvalues key in
      self#remove_reverse_keys oldvalues
  end
end 
   *)
(*  
    
    method fold_reverse ~init ~f =
      Core.Map.fold !reverse_map ~init ~f

 *)
(*Cannot do these right now--type 'c the compraator is unbound
    method comparator () =
      Core.Map.comparator !forward_map
    method comparator_reverse () =
      Core.Map.comparator !reverse_map
*)
