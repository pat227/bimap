module type Comparable = Core.Comparable.S
module Bimap_module :
functor (A : Comparable) (B : Comparable) -> sig
	  type t = {
            forward_map : (A.t, B.t, A.comparator_witness) Core.Map.t;
            reverse_map : (B.t, A.t, B.comparator_witness) Core.Map.t;
	  }
	  val add : key:A.t -> data:B.t -> t
	  val add_inverse : key:B.t -> data:A.t -> t
	end 
end
