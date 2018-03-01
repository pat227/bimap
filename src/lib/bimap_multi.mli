module Bimap_multi : sig
  class ['a, 'b] bimap_class :
	  ('a, 'b Core_kernel__.Import.list, 'c) Core.Map.t ->
	  ('b, 'a, 'd) Core.Map.t ->
	  object
            val mutable forward_map :
			  ('a, 'b Core_kernel__.Import.list, 'c) Core.Map.t ref
            val mutable reverse_map : ('b, 'a, 'd) Core.Map.t ref
            method add_multi : key:'a -> data:'b -> unit
	  end
end
