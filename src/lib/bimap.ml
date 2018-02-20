(*do this twice; once with objects/classes and once with modules
  --m1 and m2 are empty maps that need to be provided by client code-- *)
module Bimap = struct
  class ['a,'b] bimap_class m1 m2 = object
    val mutable forward_map = ref m1
    val mutable reverse_map = ref m2
(* COMPILER WILL NOT WORK WITH THIS METHOD
     method add ~key ~data = 
      let forward_map = Core.Map.add forward_map ~key ~data in 
      let reverse_map = Core.Map.add reverse_map ~key:data ~data:key in ()
BUT DOES WITH THE FOLLOWING VERSION -- seems we cannot have named args?!?
*)
    method add : 'a -> 'b -> unit =
      (fun key data -> 
       let () = forward_map := (Core.Map.add !forward_map ~key ~data) in 
       reverse_map := Core.Map.add !reverse_map ~key:data ~data:key)
    method add_inverse ~key ~data =
      let () = reverse_map := (Core.Map.add !reverse_map ~key:data ~data:key) in
      forward_map := Core.Map.add !forward_map ~key ~data
    method is_empty =
      Core.Map.is_empty !forward_map
    method length =
      Core.Map.length !forward_map
(*    method change ~key ~f =
      let old_value = Core.Map.find_exn forward_map key in 
      let forward_map = Core.Map.change forward_map key ~f in
      let new_value = Core.Map.find_exn forward_map key in 
      let reverse_map = Core.Map.add reverse_map ~key:new_value ~data:key in
      let reverse_map = Core.Map.remove reverse_map old_value in
      ()*)
    method find ~key =
      Core.Map.find !forward_map key
    method find_inverse ~key =
      Core.Map.find !reverse_map key
    method find_exn ~key =
      Core.Map.find_exn !forward_map key
    method find_exn_inverse ~key =
      Core.Map.find_exn !reverse_map key
  end 	    
end 
