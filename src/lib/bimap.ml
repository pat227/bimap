(*do this twice; once with objects/classes and once with modules
  --m1 and m2 are empty maps that need to be provided by client code-- *)
module Bimap = struct
  class ['a,'b] bimap_class m1 m2 = object(self)
    val mutable forward_map = ref m1
    val mutable reverse_map = ref m2 
    method add ~(key:'a) ~(data:'b) = 
      let () = forward_map := (Core.Map.add !forward_map ~key ~data) in 
      reverse_map := Core.Map.add !reverse_map ~key:data ~data:key
    method add_inverse ~key ~data =
      let () = reverse_map := (Core.Map.add !reverse_map ~key ~data) in
      forward_map := Core.Map.add !forward_map ~key:data ~data:key
    method change ~key ~f =
      let old_value = Core.Map.find_exn !forward_map key in 
      let () = forward_map := (Core.Map.change !forward_map key ~f) in
      let new_value = Core.Map.find_exn !forward_map key in 
      let () = reverse_map :=
		 (Core.Map.add !reverse_map ~key:new_value ~data:key) in
      reverse_map := (Core.Map.remove !reverse_map old_value)
    method change_inverse ~key ~f =
      let old_key = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.change !reverse_map key ~f) in
      let new_value = Core.Map.find_exn !reverse_map key in 
      let () = forward_map :=
		 (Core.Map.add !forward_map ~key:new_value ~data:key) in
      forward_map := (Core.Map.remove !forward_map old_key)
    method private create_inverse_map_from_forward_map =
      let () = reverse_map :=
		 (Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map)) in
      self#iter_keys
	     ~f:(fun k ->
		 reverse_map :=
		   Core.Map.add !reverse_map ~key:(self#find_exn ~key:k) ~data:k)
    method private create_forward_map_from_reverse_map =
      let () = forward_map :=
		 (Core.Map.empty ~comparator:(Core.Map.comparator !forward_map)) in
      self#iter_keys_inverse
	     ~f:(fun k ->
		 forward_map :=
		   Core.Map.add !forward_map
				~key:(self#find_exn_inverse ~key:k) ~data:k)
    method data =
      Core.Map.data !forward_map
    method data_inverse =
      Core.Map.data !reverse_map
    method empty () =
      let () = forward_map :=
		 (Core.Map.empty ~comparator:(Core.Map.comparator !forward_map)) in
      reverse_map :=
	(Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map))
(*    method equal f ~other_bimap =
      Core.Map.equal f !forward_map !other_fwd_map *)
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
    (*    method forward_map = !forward_map*)
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
      let () = reverse_map :=
		 Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map) in
      Core.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       Core.Map.add !reverse_map
				    ~key:(Core.Map.find_exn !forward_map k) ~data:k)
    method map_inverse ~f =
      let () = reverse_map := (Core.Map.map !reverse_map ~f) in
      let () = forward_map :=
		 Core.Map.empty ~comparator:(Core.Map.comparator !forward_map) in
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.add !forward_map
				    ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method mapi ~f =
      let () = forward_map := (Core.Map.mapi !forward_map ~f) in
      let () = reverse_map :=
		 Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map) in
      Core.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       Core.Map.add !reverse_map
				    ~key:(Core.Map.find_exn !forward_map k) ~data:k)
    method mapi_inverse ~f =
      let () = reverse_map := (Core.Map.mapi !reverse_map ~f) in
      let () = forward_map :=
		 Core.Map.empty ~comparator:(Core.Map.comparator !forward_map) in
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.add !forward_map
				    ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method mem key =
      Core.Map.mem !forward_map key
    method mem_inverse key =
      Core.Map.mem !reverse_map key
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
      let () = reverse_map := (Core.Map.add !reverse_map newvalue key) in
      reverse_map := (Core.Map.remove !reverse_map oldvalue)
  end
end 

(*
    method fold ~init ~f =
      Core.Map.fold !forward_map ~init ~f
    method fold_inverse ~init ~f =
      Core.Map.fold !reverse_map ~init ~f

 *)
(*Cannot do these right now--type 'c the compraator is unbound
    method comparator () =
      Core.Map.comparator !forward_map
    method comparator_inverse () =
      Core.Map.comparator !reverse_map
*)
