module Bimap_multi : sig
  class ['a, 'b] bimap_multi_class :
	  ('a, 'b list, 'c) Core.Map.t ->
	  ('b, 'a, 'd) Core.Map.t ->
	  object
            val mutable forward_map :
			  ('a, 'b list, 'c) Core.Map.t ref
            val mutable reverse_map : ('b, 'a, 'd) Core.Map.t ref
            method add_multi : key:'a -> data:'b -> unit
            method add_multi_inverse : key:'b -> data:'a -> unit
	    method change : key:'a -> f:('b list option ->
					 'b list option) -> unit
	    method change_inverse : key:'b -> f:('a option ->
						 'a option) -> unit
	    (*===todo===*)
	    method count : f:('b list -> bool) -> int
	    method count_inverse : f:('a -> bool) -> int
	    method counti : f:(key:'a -> data:'b list -> bool) -> int
	    (*----------*)
            method data : 'b list list
            method data_inverse : 'a list
            method empty : unit -> unit
	    (*===todo===*)
	    method exists : f:('b list -> bool) -> bool
	    method exists_inverse : f:('a -> bool) -> bool
	    method existsi : f:(key:'a -> data:'b list -> bool) -> bool
	    method existsi_inverse : f:(key:'b -> data:'a -> bool) -> bool
	    (*----------*)
	    method find :
		     key:'a -> 'b list option
            method find_exn : key:'a -> 'b list
            method find_exn_inverse : key:'b -> 'a
            method find_inverse : key:'b -> 'a option
	    (*===todo===*)
	    method filter : f:('b list -> bool) -> unit
	    method filter_inverse : f:('a -> bool) -> unit
	    method filteri : f:(key:'a -> data:'b list -> bool) -> unit
	    method filteri_inverse : f:(key:'b -> data:'a -> bool) -> unit
	    method filter_keys : f:('a -> bool) -> unit
	    method filter_keys_inverse : f:('b -> bool) -> unit
	    method filter_map : f:('b list -> 'b list option) -> unit
	    method filter_map_inverse : f:('a -> 'a option) -> unit
	    (*----------*)					       
	    method fold : init:'a -> f:(key:'a -> data:'b list -> 'a -> 'a) -> 'a
	    method fold_inverse : init:'b -> f:(key:'b -> data:'a -> 'b -> 'b) -> 'b
	    method fold_right : init:'a -> f:(key:'a -> data:'b list -> 'a -> 'a) -> 'a
	    method fold_right_inverse : init:'b -> f:(key:'b -> data:'a -> 'b -> 'b) -> 'b
	    method for_all : f:('b list -> bool) -> bool
	    method for_all_inverse : f:('a -> bool) -> bool
	    method is_empty : bool
            method iter :
		     f:('b list -> unit) -> unit
            method iter_inverse :
		     f:('a -> unit) -> unit
            method iter_keys :
		     f:('a -> unit) -> unit
            method iter_keys_inverse :
		     f:('b -> unit) -> unit
            method iteri :
		     f:(key:'a -> data:'b list -> unit) -> unit
            method iteri_inverse :
		     f:(key:'b -> data:'a -> unit) -> unit
	    method keys : 'a list
            method keys_inverse : 'b list
            method length : int
	    method map : f:('b list -> 'b list) -> unit
	    method map_inverse : f:('a -> 'a) -> unit
	    method mapi : f:(key:'a -> data:'b list -> 'b list) -> unit
	    method mapi_inverse : f:(key:'b -> data:'a -> 'a) -> unit
	    method mem : 'a -> bool
            method mem_inverse : 'b -> bool
	    (*====todo====*)
	    method min_elt : ('a * 'b list) option
	    method min_elt_exn : ('a * 'b list)
	    method min_elt_inverse : ('b * 'a) option
	    method min_elt_exn_inverse : ('b * 'a)
	    method max_elt : ('a * 'b list) option
	    method max_elt_exn : ('a * 'b list)
	    method max_elt_inverse : ('b * 'a) option
	    method max_elt_exn_inverse : ('b * 'a)
	    method nth : int -> ('a * 'b list) option
	    method nth_inverse : int -> ('b * 'a) option
	    (*------------*)
	    method remove : key:'a -> unit
            method remove_inverse : key:'b -> unit
	    method remove_multi : key:'a -> unit
	    (*===todo===*)
	    method to_alist : ?key_order:[`Increasing | `Decreasing] -> unit -> ('a * 'b list) list 
	    (*------------*)
	    method update : 'a -> f:('b list option -> 'b list) -> unit
	  end
end
