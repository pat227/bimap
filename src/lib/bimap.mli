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
	method count : f:('b -> bool) -> int
	method count_inverse : f:('a -> bool) -> int
	method counti : f:(key:'a -> data:'b -> bool) -> int
        method data : 'b list
        method data_inverse : 'a list
	method empty : unit -> unit
	method exists : f:('b -> bool) -> bool
	method exists_inverse : f:('a -> bool) -> bool
	method existsi : f:(key:'a -> data:'b -> bool) -> bool
	method existsi_inverse : f:(key:'b -> data:'a -> bool) -> bool
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
	method filter_map : f:('b -> 'b option) -> unit
	method filter_map_inverse : f:('a -> 'a option) -> unit
	(*	method forward_map : ('a,'b,_) Core.Map.t*)
	method fold : init:'a -> f:(key:'a -> data:'b -> 'a -> 'a) -> 'a
	method fold_inverse : init:'b -> f:(key:'b -> data:'a -> 'b -> 'b) -> 'b
	(*method fold_range_inclusive : min:'a -> max:'a -> init:'b -> f:(key:'a -> data:'b -> 'b) -> 'b*)
	method fold_right : init:'a -> f:(key:'a -> data:'b -> 'a -> 'a) -> 'a
	method fold_right_inverse : init:'b -> f:(key:'b -> data:'a -> 'b -> 'b) -> 'b
	method for_all : f:('b -> bool) -> bool
	method for_all_inverse : f:('a -> bool) -> bool
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
	method min_elt : ('a * 'b) option
	method min_elt_exn : ('a * 'b)
	method min_elt_inverse : ('b * 'a) option
	method min_elt_exn_inverse : ('b * 'a)
	method max_elt : ('a * 'b) option
	method max_elt_exn : ('a * 'b)
	method max_elt_inverse : ('b * 'a) option
	method max_elt_exn_inverse : ('b * 'a)
	method nth : int -> ('a * 'b) option
	method nth_inverse : int -> ('b * 'a) option
        method remove : key:'a -> unit
        method remove_inverse : key:'b -> unit
	(*	method reverse_map : ('b, 'a, _) Core.Map.t*)
	method to_alist : ?key_order:[`Increasing | `Decreasing] -> unit -> ('a * 'b) list 
        method update :
          'a -> f:('b option -> 'b) -> unit
      end
  end
