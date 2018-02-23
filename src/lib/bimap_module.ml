module type Comparable = Core.Comparable.S
module Bimap_module = functor (A:Comparable)(B:Comparable) -> struct
  type t = {
    forward_map : (A.t, B.t, A.comparator_witness) Core.Map.t;
    reverse_map : (B.t, A.t, B.comparator_witness) Core.Map.t
  }
end 
