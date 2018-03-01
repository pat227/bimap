(*do this twice; once with objects/classes and once with modules
  --m1 and m2 are empty maps that need to be provided by client code-- *)
module Bimap_multi = struct
  class ['a,'b] bimap_class m1 m2 = object(self)
    val mutable forward_map = ref m1
    val mutable reverse_map = ref m2 
    method add_multi ~(key:'a) ~(data:'b) =
      let () = forward_map := (Core.Map.add_multi !forward_map ~key ~data) in
      reverse_map := Core.Map.add !reverse_map ~key:data ~data:key
    method add_multi_inverse ~(key:'b) ~(data:'a) =
      let () = forward_map := (Core.Map.add_multi !forward_map ~key:data ~data:key) in
      reverse_map := Core.Map.add !reverse_map ~key ~data
    method private add_inverse_values k v =
      let rec add_values k v =
	match k with
	| h :: t ->
	   let () = reverse_map := (Core.Map.add !reverse_map ~key:h ~data:v) in
	   add_values t v
	| [] -> () in
      add_values k v 		  
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
    method data =
      Core.Map.data !forward_map
    method data_inverse =
      Core.Map.data !reverse_map
    method empty () =
      let () = forward_map := (Core.Map.empty ~comparator:(Core.Map.comparator !forward_map)) in
      reverse_map := (Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map))
    method find ~key =
      Core.Map.find !forward_map key
    method find_inverse ~key =
      Core.Map.find !reverse_map key
    method find_exn ~key =
      Core.Map.find_exn !forward_map key
    method find_exn_inverse ~key =
      Core.Map.find_exn !reverse_map key
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
    method mem key =
      Core.Map.mem !forward_map key
    method mem_inverse key =
      Core.Map.mem !reverse_map key
    method remove ~key =
      let reverse_keys = Core.Map.find_exn !forward_map key in 
      let () = forward_map := (Core.Map.remove !forward_map key) in
      self#remove_inverse_keys reverse_keys
    method private remove_inverse_keys klist = 
      let rec remove_keys k =
	match k with
	| h :: t ->
	   let () = reverse_map := (Core.Map.remove !reverse_map h) in
	   remove_keys t
	| [] -> () in
      remove_keys klist
    method remove_inverse ~key =
      let fwd_key = Core.Map.find_exn !reverse_map key in 
      let () = reverse_map := (Core.Map.remove !reverse_map key) in
      forward_map := (Core.Map.remove !forward_map fwd_key)

(*

    (*Cannot do these right now--type 'c the compraator is unbound
    method comparator () =
      Core.Map.comparator !forward_map
    method comparator_inverse () =
      Core.Map.comparator !reverse_map*)

    method map ~f =
      let () = forward_map := (Core.Map.map !forward_map ~f) in
      let () = reverse_map := Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map) in
      Core.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       Core.Map.add !reverse_map ~key:(Core.Map.find_exn !forward_map k) ~data:k)
    method map_inverse ~f =
      let () = reverse_map := (Core.Map.map !reverse_map ~f) in
      let () = forward_map := Core.Map.empty ~comparator:(Core.Map.comparator !forward_map) in
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.add !forward_map ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method mapi ~f =
      let () = forward_map := (Core.Map.mapi !forward_map ~f) in
      let () = reverse_map := Core.Map.empty ~comparator:(Core.Map.comparator !reverse_map) in
      Core.Map.iter_keys
	!forward_map
	~f:(fun k -> reverse_map :=
		       Core.Map.add !reverse_map ~key:(Core.Map.find_exn !forward_map k) ~data:k)
    method mapi_inverse ~f =
      let () = reverse_map := (Core.Map.mapi !reverse_map ~f) in
      let () = forward_map := Core.Map.empty ~comparator:(Core.Map.comparator !forward_map) in
      Core.Map.iter_keys
	!reverse_map
	~f:(fun k -> forward_map :=
		       Core.Map.add !forward_map ~key:(Core.Map.find_exn !reverse_map k) ~data:k)
    method update key ~f =
      let oldvalue = Core.Map.find_exn !forward_map key in
      let () = forward_map := (Core.Map.update !forward_map key ~f) in
      let newvalue = Core.Map.find_exn !forward_map key in 
      let () = reverse_map := (Core.Map.add !reverse_map newvalue key) in
      reverse_map := (Core.Map.remove !reverse_map oldvalue)*)
  end
end 

(*
    method fold ~init ~f =
      Core.Map.fold !forward_map ~init ~f
    method fold_inverse ~init ~f =
      Core.Map.fold !reverse_map ~init ~f
    method equal f ~other_fwd_map =
      Core.Map.equal f !forward_map !other_fwd_map *)
    (*unfortunately, as a consequence of 'b being a parameter of the class,
      if we use Map.add_multi with 'b as the value of forward map then 'b is a 
      polymorphic list. Then this constrains the keys in the reverse map to be
      of the same type, ie, a polymorphic list, which is wrong. Not sure 
      how to work around this; may be forced to eliminate the mutable members
      to eliminate the type parameters on the class, but then client code
      has to supply forward and reverse maps in order all the time.
    method add_multi ~key ~data =
      let oldlistvalues = Core.Option.value (Core.Map.find !forward_map key) ~default:[] in  
      let () = forward_map := (Core.Map.add !forward_map ~key ~data:(data :: oldlistvalues)) in
      reverse_map := (Core.Map.add !reverse_map ~key:data ~data:key) *)
      (*let rec add__multi k v =
	match v with
	| [] -> ()
	| h :: t ->
	   let () = forward_map := (Core.Map.add_multi !forward_map ~key:k ~data:h) in
	   add__multi k t in*)
      (*let rec add_reverse_keys k v =
	match k with
	| [] -> ()
	| h :: t -> 
	   let () = reverse_map := (Core.Map.add !reverse_map ~key:h ~data:v) in
	   add_reverse_keys t v in
      let () = add__multi key data in
      add_reverse_keys [data] key*)


    (*
    method remove_multi ~key ~data =
      let () = forward_map := (Core.Map.remove_multi !forward_map ~key ~data) in
      reverse_map := (Core.Map.remove !reverse_map ~key:data ~data:key)
 *)
