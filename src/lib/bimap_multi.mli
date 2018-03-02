module Bimap_multi : sig
  class ['a, 'b] bimap_class :
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
            method data : 'b list list
            method data_inverse : 'a list
            method empty : unit -> unit
	    method find :
		     key:'a -> 'b list option
            method find_exn : key:'a -> 'b list
            method find_exn_inverse : key:'b -> 'a
            method find_inverse : key:'b -> 'a option
	    (*method fold : init:'e -> (key:'a -> data:'b list -> 'e -> 'e) -> 'e
	    method fold_inverse : init:'e -> (key:'b -> data:'a -> 'e -> 'e) -> 'e*)
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
	    method remove : key:'a -> unit
            method remove_inverse : key:'b -> unit
	    method remove_multi : key:'a -> unit
	    method update : 'a -> f:('b list option -> 'b list) -> unit
	  end
end
