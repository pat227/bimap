(*do this twice; once with objects/classes and once with modules
  --m1 and m2 are empty maps that need to be provided by client code-- *)
module Bimap = struct
  class ['a,'b] bimap_class m1 m2 = object(self)
    val mutable forward_map = ref m1
    val mutable reverse_map = ref m2
    method private empty_forward_map () =
      self#iter_keys
        ~f:(fun k -> forward_map := (Core.Map.remove !forward_map k))
    method private empty_reverse_map () =
      self#iter_keys_inverse 
        ~f:(fun k -> reverse_map := (Core.Map.remove !reverse_map k))
    method set ~(key:'a) ~(data:'b) =
      let () = forward_map := Core.Map.set !forward_map ~key ~data in
      reverse_map := Core.Map.set !reverse_map ~key:data ~data:key
    method set_inverse ~key ~data =
      let () = reverse_map := (Core.Map.set !reverse_map ~key ~data) in
      forward_map := Core.Map.set !forward_map ~key:data ~data:key
    method change ~key ~f =
      let old_value = Core.Map.find_exn !forward_map key in 
      let () = forward_map := (Core.Map.change !forward_map key ~f) in
      let new_value = Core.Map.find_exn !forward_map key in 
      let () = reverse_map :=
		 (Core.Map.set !reverse_map ~key:new_value ~data:key) in
      reverse_map := (Core.Map.remove !reverse_map old_value)
    method change_inverse ~key ~f =
      let old_key = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.change !reverse_map key ~f) in
      let new_value = Core.Map.find_exn !reverse_map key in 
      let () = forward_map :=
		 (Core.Map.set !forward_map ~key:new_value ~data:key) in
      forward_map := (Core.Map.remove !forward_map old_key)
    method private create_inverse_map_from_forward_map =
      let open Sexplib.Std in
      let open Sexplib in
      (*===could never get this to work===
      let comp = Core.Map.comparator !reverse_map in
      let () = reverse_map := (Core.Map.empty comp) in*)
      let () = self#empty_reverse_map () in
      self#iter_keys
	     ~f:(fun k ->
		 reverse_map :=
		   Core.Map.set !reverse_map ~key:(self#find_exn ~key:k) ~data:k)
    method private create_forward_map_from_reverse_map =
      (* ==== could never get this to work ===
      let () = forward_map :=
		 (Core.Map.empty (Core.Map.comparator !forward_map)) in*)
      let () = self#empty_forward_map () in 
      self#iter_keys_inverse
	     ~f:(fun k ->
		 forward_map :=
		   Core.Map.set !forward_map
				~key:(self#find_exn_inverse ~key:k) ~data:k)
    method count ~f =
      Core.Map.count !forward_map ~f
    method count_inverse ~f =
      Core.Map.count !reverse_map ~f
    method counti ~f =
      Core.Map.counti !forward_map ~f
    method data =
      Core.Map.data !forward_map
    method data_inverse =
      Core.Map.data !reverse_map
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
      Core.Map.exists !forward_map ~f
    method exists_inverse ~f =
      Core.Map.exists !reverse_map ~f
    method existsi ~f =
      Core.Map.existsi !forward_map ~f
    method existsi_inverse ~f =
      Core.Map.existsi !reverse_map ~f
    method find ~key =
      Core.Map.find !forward_map key
    method find_inverse ~key =
      Core.Map.find !reverse_map key
    method find_exn ~key =
      Core.Map.find_exn !forward_map key
    method find_exn_inverse ~key =
      Core.Map.find_exn !reverse_map key
    method filter ~f =
      let () = forward_map := (Core.Map.filter !forward_map ~f) in
      reverse_map := (Core.Map.filter_keys !reverse_map ~f)
    method filter_inverse ~f =
      let () = reverse_map := (Core.Map.filter !reverse_map ~f) in
      forward_map := (Core.Map.filter_keys !forward_map ~f)
    method filter_keys ~f =
      let () = forward_map := (Core.Map.filter_keys !forward_map ~f) in
      reverse_map := (Core.Map.filter !reverse_map ~f)
    method filter_keys_inverse ~f =
      let () = reverse_map := (Core.Map.filter_keys !reverse_map ~f) in
      forward_map := (Core.Map.filter !forward_map ~f)
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
    (*    method forward_map = !forward_map*)
    method fold : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e = 
      (fun ~init ~f -> Core.Map.fold !forward_map ~init ~f)
    method fold_inverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> Core.Map.fold !reverse_map ~init ~f)
(*    method fold_range_inclusive ~min ~max ~init ~f =
      Core.Map.fold_range_inclusive !forward_map ~min ~max ~init ~f*)
    method fold_right : 'e. init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> Core.Map.fold_right !forward_map ~init ~f)
    method fold_right_inverse : 'e. init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e =
      (fun ~init ~f -> Core.Map.fold_right !reverse_map ~init ~f)
    method for_all ~f =
      Core.Map.for_all !forward_map ~f
    method for_all_inverse ~f =
      Core.Map.for_all !reverse_map ~f
    method is_empty =
      Core.Map.is_empty !forward_map
    method iter_keys ~f =
      Core.Map.iter_keys !forward_map ~f
    method iter_keys_inverse ~f =
      Core.Map.iter_keys !reverse_map ~f
    method iter ~f =
      Core.Map.iter !forward_map ~f
    method iter_inverse ~f =
      Core.Map.iter !reverse_map ~f
    method iteri ~f =
      Core.Map.iteri !forward_map ~f
    method iteri_inverse ~f =
      Core.Map.iteri !reverse_map ~f
    method keys =
      Core.Map.keys !forward_map
    method keys_inverse =
      Core.Map.keys !reverse_map
    method length =
      Core.Map.length !forward_map
    method map ~f =
      let () = forward_map := (Core.Map.map !forward_map ~f) in
      (*===could never get this to work===
       let () = reverse_map :=
		 Core.Map.empty (Core.Map.comparator !reverse_map) in*)
      let () = self#empty_reverse_map () in 
      Core.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       Core.Map.set !reverse_map
				    ~key:(Core.Map.find_exn !forward_map k) ~data:k)
    method map_inverse ~f =
      let () = reverse_map := (Core.Map.map !reverse_map ~f) in
      (*let () = forward_map :=
		 Core.Map.empty (Core.Map.comparator !forward_map) in*)
      let () = self#empty_forward_map () in 
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.set !forward_map
				    ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method mapi ~f =
      let () = forward_map := (Core.Map.mapi !forward_map ~f) in
      (*let () = reverse_map :=
		 Core.Map.empty (Core.Map.comparator !reverse_map) in*)
      let () = self#empty_reverse_map () in 
      Core.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       Core.Map.set !reverse_map
				    ~key:(Core.Map.find_exn !forward_map k) ~data:k)
    method mapi_inverse ~f =
      let () = reverse_map := (Core.Map.mapi !reverse_map ~f) in
      (*let () = forward_map :=
		 Core.Map.empty (Core.Map.comparator !forward_map) in*)
      let () = self#empty_forward_map () in
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.set !forward_map
				    ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method mem key =
      Core.Map.mem !forward_map key
    method mem_inverse key =
      Core.Map.mem !reverse_map key
    method min_elt =
      Core.Map.min_elt !forward_map
    method min_elt_exn =
      Core.Map.min_elt_exn !forward_map
    method min_elt_inverse =
      Core.Map.min_elt !reverse_map
    method min_elt_exn_inverse =
      Core.Map.min_elt_exn !reverse_map
    method max_elt =
      Core.Map.max_elt !forward_map
    method max_elt_exn =
      Core.Map.max_elt_exn !forward_map
    method max_elt_inverse =
      Core.Map.max_elt !reverse_map
    method max_elt_exn_inverse =
      Core.Map.max_elt_exn !reverse_map
    method nth int =
      Core.Map.nth !forward_map int
    method nth_inverse int =
      Core.Map.nth !reverse_map int
    method remove ~key =
      let reverse_key = Core.Map.find_exn !forward_map key in 
      let () = forward_map := (Core.Map.remove !forward_map key) in
      reverse_map := (Core.Map.remove !reverse_map reverse_key)
    method remove_inverse ~key =
      let fwd_key = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.remove !reverse_map key) in
      forward_map := (Core.Map.remove !forward_map fwd_key)
(*    method reverse_map = !reverse_map*)
    method update key ~f =
      let oldvalue = Core.Map.find_exn !forward_map key in
      let () = forward_map := (Core.Map.update !forward_map key ~f) in
      let newvalue = Core.Map.find_exn !forward_map key in 
      let () = reverse_map := (Core.Map.set !reverse_map newvalue key) in
      reverse_map := (Core.Map.remove !reverse_map oldvalue)
    method to_alist ?key_order () =
      match Core.Option.is_some key_order with
      | false -> Core.Map.to_alist !forward_map
      | true -> Core.Map.to_alist ~key_order:(Core.Option.value_exn key_order) !forward_map
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
