module Bimap :
  sig
    class ['a, 'b] bimap_class :
      ('a, 'b, 'c) Core.Map.t ->
      ('b, 'a, 'd) Core.Map.t ->
      object
        val mutable forward_map : ('a, 'b, 'c) Core.Map.t ref
        val mutable reverse_map : ('b, 'a, 'd) Core.Map.t ref
        method add : 'a -> 'b -> unit
        method add_inverse : key:'a -> data:'b -> unit
	(*method add_multi : key:'a -> data:'b -> unit
	method remove_multi : key:'a -> data:'b -> unit*)
        method change :
          key:'a ->
          f:('b Core_kernel__.Import.option -> 'b Core_kernel__.Import.option) ->
          unit
        method find : key:'a -> 'b Core_kernel__.Import.option
        method find_exn : key:'a -> 'b
        method find_exn_inverse : key:'b -> 'a
        method find_inverse : key:'b -> 'a Core_kernel__.Import.option
        method is_empty : Core_kernel__.Import.bool
        method length : Core_kernel__.Import.int
	method mem : 'a -> bool
	method mem_inverse : 'b -> bool
	method update : 'a -> f:('b option -> 'b) -> unit
      end
  end
