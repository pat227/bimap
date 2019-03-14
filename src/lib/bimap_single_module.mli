module Bimap_single :
functor (ModuleA : Core.Comparable.S) (ModuleB : Core.Comparable.S) ->
sig
  (*better than passing tuples around and possibly getting order 
   backwards somewhere, after which all bets are off; so we saddle 
   client code writers with this type*)
  type t = {
      fwdmap : ModuleB.Map.Key.t ModuleA.Map.t;
      revmap : ModuleA.Map.Key.t ModuleB.Map.t
    }
  val set : t -> key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> t
            
  val set_reverse : t -> key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> t

  val change : t -> key:ModuleA.Map.Key.t ->
	       f:(ModuleB.Map.Key.t option -> ModuleB.Map.Key.t option) -> t

  val change_reverse :
		     t -> key:ModuleB.Map.Key.t ->
		     f:(ModuleA.Map.Key.t option -> ModuleA.Map.Key.t option) -> t

  (*CANNOT WRITE THESE RIGHT NOW -- 
        val comparator : unit -> ('a, 'c) Core_kernel__.Comparator.t
        val comparator_reverse :
		 unit -> ('b, 'd) Core_kernel__.Comparator.t*)
  val count : t -> f:(ModuleB.Map.Key.t -> bool) -> int
  val count_reverse : t -> f:(ModuleA.Map.Key.t -> bool) -> int
  val counti : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> bool) -> int
  val data : t -> ModuleB.Map.Key.t list
  val data_reverse : t -> ModuleA.Map.Key.t list
  val empty : unit -> t
  val exists : forward_map:ModuleB.Map.Key.t ModuleA.Map.t -> f:(ModuleB.Map.Key.t -> bool) -> bool
  val exists_reverse : reverse_map:ModuleA.Map.Key.t ModuleB.Map.t -> f:(ModuleA.Map.Key.t -> bool) -> bool
  val existsi : forward_map:ModuleB.Map.Key.t ModuleA.Map.t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> bool) -> bool
  val existsi_reverse : reverse_map:ModuleA.Map.Key.t ModuleB.Map.t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> bool) -> bool
  val find : forward_map:ModuleB.Map.Key.t ModuleA.Map.t -> key:ModuleA.Map.Key.t -> ModuleB.Map.Key.t option
  val find_reverse : reverse_map:ModuleA.Map.Key.t ModuleB.Map.t -> key:ModuleB.Map.Key.t -> ModuleA.Map.Key.t option

(*            val find_exn : key:'a -> 'b
            val find_exn_reverse : key:'b -> 'a

	    val filter : f:('b -> bool) -> unit
	    val filter_reverse : f:('a -> bool) -> unit
	    val filteri : f:(key:'a -> data:'b -> bool) -> unit
	    val filteri_reverse : f:(key:'b -> data:'a -> bool) -> unit
	    val filter_keys : f:('a -> bool) -> unit
	    val filter_keys_reverse : f:('b -> bool) -> unit
	    val filter_map : f:('b -> 'b option) -> unit
	    val filter_map_reverse : f:('a -> 'a option) -> unit
	    (*	val forward_map : ('a,'b,_) Core.Map.t*)
	    (*val fold : init:'a -> f:(key:'a -> data:'b -> 'a -> 'a) -> 'a*)
            val fold : init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e
	    val fold_reverse : init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e
	    (*val fold_range_inclusive : min:'a -> max:'a -> init:'b -> f:(key:'a -> data:'b -> 'b) -> 'b*)
	    val fold_right : init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e
	    val fold_right_reverse : init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e
	    val for_all : f:('b -> bool) -> bool
	    val for_all_reverse : f:('a -> bool) -> bool
	    val is_empty : bool
            val iter :
		     f:('b -> unit) -> unit
            val iter_reverse :
		     f:('a -> unit) -> unit  
            val iter_keys :
		     f:(ModuleA.Map.Key.t -> unit) -> unit
            val iter_keys_reverse :
		     f:(ModuleB.Map.Key.t -> unit) -> unit
            val iteri :
		     f:(key:'a -> data:'b -> unit) -> unit
            val iteri_reverse :
		     f:(key:'b -> data:'a -> unit) -> unit
            val keys : 'a list
            val keys_reverse : 'b list
            val length : int
            val map : f:('b -> 'b) -> unit
            val map_reverse : f:('a -> 'a) -> unit
            val mapi :
		     f:(key:'a -> data:'b -> 'b) -> unit
            val mapi_reverse :
		     f:(key:'b -> data:'a -> 'a) -> unit
            val mem : 'a -> bool
            val mem_reverse : 'b -> bool
	    val min_elt : ('a * 'b) option
	    val min_elt_exn : ('a * 'b)
	    val min_elt_reverse : ('b * 'a) option
	    val min_elt_exn_reverse : ('b * 'a)
	    val max_elt : ('a * 'b) option
	    val max_elt_exn : ('a * 'b)
	    val max_elt_reverse : ('b * 'a) option
	    val max_elt_exn_reverse : ('b * 'a)
	    val nth : int -> ('a * 'b) option
	    val nth_reverse : int -> ('b * 'a) option
            val remove : key:'a -> unit
            val remove_reverse : key:'b -> unit
	    (*	val reverse_map : ('b, 'a, _) Core.Map.t*)
            val to_alist : ?key_order:[`Increasing | `Decreasing] -> unit -> ('a * 'b) list 
            val update : key:'a -> f:('b option -> 'b) -> unit
	  
(*  type ('a, 'b) t = ('a, 'b) bimap_class
  val make : ('a, 'b, 'c) Core.Map.t -> ('b, 'a, 'd) Core.Map.t -> ('a, 'b) t*)
 *)
end
