module Bimap :
  sig
    class ['a, 'b] bimap_class :
      ('a, 'b, 'c) Core.Map.t ->
      ('b, 'a, 'd) Core.Map.t ->
      object
        val mutable forward_map : ('a, 'b, 'c) Core.Map.t ref
        val mutable reverse_map : ('b, 'a, 'd) Core.Map.t ref
        method add : 'a -> 'b -> unit
        method add_inverse : key:'b -> data:'a -> unit
	(*method add_multi : key:'a -> data:'b -> unit
	method remove_multi : key:'a -> data:'b -> unit*)
        method change :
		 key:'a ->
		 f:('b Core_kernel__.Import.option -> 'b Core_kernel__.Import.option) ->
		 unit
	method change_inverse :
		 key:'b ->
		 f:('a option -> 'a option) ->
		 unit
	method data : 'b list
        method data_inverse : 'a list
        method find : key:'a -> 'b Core_kernel__.Import.option
        method find_exn : key:'a -> 'b
        method find_exn_inverse : key:'b -> 'a
        method find_inverse : key:'b -> 'a Core_kernel__.Import.option
        method is_empty : Core_kernel__.Import.bool
	method iter : f:('b -> unit) -> unit
	method iter_inverse : f:('a -> unit) -> unit
	method iteri : f:(key:'a -> data:'b -> unit) -> unit
	method iteri_inverse : f:(key:'b -> data:'a -> unit) -> unit
	method iter_keys :
		 f:('a -> unit) -> unit
        method iter_keys_inverse :
		 f:('b -> unit) -> unit
	method keys : 'a list
        method keys_inverse : 'b list
        method length : Core_kernel__.Import.int
	method map : f:('b -> 'b) -> unit
	method map_inverse : f:('a -> 'a) -> unit
	method mapi : f:(key:'a -> data:'b -> 'b) -> unit
	method mapi_inverse : f:(key:'b -> data:'a -> 'a) -> unit
	method mem : 'a -> bool
	method mem_inverse : 'b -> bool
	method remove : key:'a -> unit
	method remove_inverse : key:'b -> unit
	method update : 'a -> f:('b option -> 'b) -> unit
      end
  end

   
(*	method fold : init:'e -> f:(key:'a -> data:'b -> 'a -> 'a) -> 'a
        method fold_inverse :
		 init:'f -> f:(key:'b -> data:'a -> 'b -> 'b) -> 'b*)
(*	method equal : ('b -> 'b -> bool) -> other_forward_map:('a, 'b, 'c) Core.Map.t -> bool*)
