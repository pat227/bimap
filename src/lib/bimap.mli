module Bimap :
  sig
    class ['a, 'b] bimap_class :
      ('a, 'b, 'c) Core.Map.t ->
      ('b, 'a, 'd) Core.Map.t ->
      object
        val mutable forward_map : ('a, 'b, 'c) Core.Map.t ref
        val mutable reverse_map : ('b, 'a, 'd) Core.Map.t ref
        method add : key:'a -> data:'b -> unit
        method add_inverse : key:'b -> data:'a -> unit
        method change :
          key:'a ->
          f:('b option -> 'b option) ->
          unit
        method change_inverse :
          key:'b ->
          f:('a option -> 'a option) ->
          unit
	(*CANNOT WRITE THESE RIGHT NOW -- 
        method comparator : unit -> ('a, 'c) Core_kernel__.Comparator.t
        method comparator_inverse :
		 unit -> ('b, 'd) Core_kernel__.Comparator.t*)
        method data : 'b list
        method data_inverse : 'a list
	method empty : unit -> unit
        method find : key:'a -> 'b option
        method find_exn : key:'a -> 'b
        method find_exn_inverse : key:'b -> 'a
        method find_inverse : key:'b -> 'a option
	method filter : f:('b -> bool) -> unit
	method filter_inverse : f:('a -> bool) -> unit
	method filteri : f:(key:'a -> data:'b -> bool) -> unit
	method filteri_inverse : f:(key:'b -> data:'a -> bool) -> unit
	method filter_keys : f:('a -> bool) -> unit
	method filter_keys_inverse : f:('b -> bool) -> unit
	(*	method forward_map : ('a,'b,_) Core.Map.t*)
        method is_empty : bool
        method iter :
          f:('b -> unit) -> unit
        method iter_inverse :
          f:('a -> unit) -> unit
        method iter_keys :
          f:('a -> unit) -> unit
        method iter_keys_inverse :
          f:('b -> unit) -> unit
        method iteri :
          f:(key:'a -> data:'b -> unit) -> unit
        method iteri_inverse :
          f:(key:'b -> data:'a -> unit) -> unit
        method keys : 'a list
        method keys_inverse : 'b list
        method length : int
        method map : f:('b -> 'b) -> unit
        method map_inverse : f:('a -> 'a) -> unit
        method mapi :
          f:(key:'a -> data:'b -> 'b) -> unit
        method mapi_inverse :
          f:(key:'b -> data:'a -> 'a) -> unit
        method mem : 'a -> bool
        method mem_inverse : 'b -> bool
        method remove : key:'a -> unit
        method remove_inverse : key:'b -> unit
	(*	method reverse_map : ('b, 'a, _) Core.Map.t*)
        method update :
          'a -> f:('b option -> 'b) -> unit
      end
  end
