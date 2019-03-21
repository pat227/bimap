module Bimap_multi_module :
functor (ModuleA : Core.Comparable.S) (ModuleB : Core.Comparable.S) ->
sig
  type t
  val empty : t
  val add_multi : t -> key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t -> t
  val add_multi_reverse : t -> key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t -> t
  val change : t -> key:ModuleA.Map.Key.t ->
               f:(ModuleB.Map.Key.t list option -> ModuleB.Map.Key.t list option) -> t
  val change_reverse : t -> key:ModuleB.Map.Key.t ->
                       f:(ModuleA.Map.Key.t list option -> ModuleA.Map.Key.t list option) -> t
  val count : t -> f:(ModuleB.Map.Key.t list -> bool) -> int
  val count_reverse : t -> f:(ModuleA.Map.Key.t list -> bool) -> int
  val counti : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> bool) -> int
(*        method private create_forward_map_from_reverse_map : unit -> unit
          method private create_reverse_map_from_forward_map : unit -> unit *)
  val data : t -> ModuleB.Map.Key.t list list
  val data_reverse : t-> ModuleA.Map.Key.t list list
  val empty : t
            (*        method private empty_forward_map : unit -> unit
          method private empty_reverse_map : unit -> unit *)
  val exists : t -> f:(ModuleB.Map.Key.t list -> bool) -> bool
  val exists_reverse : t -> f:(ModuleA.Map.Key.t list -> bool) -> bool
  val existsi : t ->  f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> bool) -> bool
  val existsi_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> bool) -> bool
  val filter : t -> f:(ModuleB.Map.Key.t list -> bool) -> t 
  val filter_keys : t -> f:(ModuleA.Map.Key.t -> bool) -> t
  val filter_reverse : t -> f:(ModuleA.Map.Key.t list -> bool) -> t
  val filter_keys_reverse : t -> f:(ModuleB.Map.Key.t -> bool) -> t
  val filter_map : t -> f:(ModuleB.Map.Key.t Core.List.t -> ModuleB.Map.Key.t list option) -> t
  val filter_map_reverse : t -> f:(ModuleA.Map.Key.t list -> ModuleA.Map.Key.t list option) -> t
  val filteri : t -> f:(key:ModuleA.Map.Key.t -> data:ModuleB.Map.Key.t list -> bool) -> t
  val filteri_reverse : t -> f:(key:ModuleB.Map.Key.t -> data:ModuleA.Map.Key.t list -> bool) -> t
  val find : t -> key:ModuleA.Map.Key.t -> 'b Core.List.t option
  val find_exn : t -> key:ModuleA.Map.Key.t -> 'b Core.List.t
  (*          method find_exn_reverse : key:ModuleB.Map.Key.t -> 'a Core.List.t
            method find_reverse :
                     key:ModuleB.Map.Key.t -> 'a Core.List.t option
            method fold :
                     init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e
            method fold_reverse :
                     init:'e -> f:(key:'b -> data:'a list -> 'e -> 'e) -> 'e
            method fold_right :
                     init:'e -> f:(key:'a -> data:'b list -> 'e -> 'e) -> 'e
            method fold_right_reverse :
                     init:'e -> f:(key:'b -> data:'a list -> 'e -> 'e) -> 'e
            method for_all : f:('b Core.List.t -> bool) -> bool
            method for_all_reverse : f:('a Core.List.t -> bool) -> bool
            method is_empty : bool
            method iter : f:('b Core.List.t -> unit) -> unit
            method iter_keys : f:(ModuleA.Map.Key.t -> unit) -> unit
            method iter_keys_reverse : f:(ModuleB.Map.Key.t -> unit) -> unit
            method iter_reverse : f:('a Core.List.t -> unit) -> unit
            method iteri :
                     f:(key:ModuleA.Map.Key.t -> data:'b Core.List.t -> unit) -> unit
            method iteri_reverse :
                     f:(key:ModuleB.Map.Key.t -> data:'a Core.List.t -> unit) -> unit
            method keys : ModuleA.Map.Key.t list
            method keys_reverse : ModuleB.Map.Key.t list
            method length : int
            method map : f:('b Core.List.t -> 'b Core.List.t) -> unit
            method map_reverse : f:('a Core.List.t -> 'a Core.List.t) -> unit
            method mapi :
                     f:(key:ModuleA.Map.Key.t -> data:'b Core.List.t -> 'b Core.List.t) ->
                     unit
            method mapi_reverse :
                     f:(key:ModuleB.Map.Key.t -> data:'a Core.List.t -> 'a Core.List.t) ->
                     unit
            method max_elt :
                     (ModuleA.Map.Key.t * 'b Core.List.t) Core_kernel__.Import.option
            method max_elt_exn : ModuleA.Map.Key.t * 'b Core.List.t
            method max_elt_exn_reverse : ModuleB.Map.Key.t * 'a Core.List.t
            method max_elt_reverse :
                     (ModuleB.Map.Key.t * 'a Core.List.t) Core_kernel__.Import.option
            method mem : ModuleA.Map.Key.t -> Core_kernel__.Import.bool
            method mem_reverse : ModuleB.Map.Key.t -> Core_kernel__.Import.bool
            method min_elt :
                     (ModuleA.Map.Key.t * 'b Core.List.t) Core_kernel__.Import.option
            method min_elt_exn : ModuleA.Map.Key.t * 'b Core.List.t
            method min_elt_exn_reverse : ModuleB.Map.Key.t * 'a Core.List.t
            method min_elt_reverse :
                     (ModuleB.Map.Key.t * 'a Core.List.t) Core_kernel__.Import.option
            method nth :
                     Core_kernel__.Import.int ->
                     (ModuleA.Map.Key.t * 'b Core.List.t) Core_kernel__.Import.option
            method nth_reverse :
                     Core_kernel__.Import.int ->
                     (ModuleB.Map.Key.t * 'a Core.List.t) Core_kernel__.Import.option
            method remove : key:ModuleA.Map.Key.t -> unit
            (*        method private remove_fwd_key_from_reverse_map :
            fwd_values_list:'b Core.List.t -> key:'a -> unit *)
            method remove_multi : key:'a -> unit
            (*        method private remove_rev_key_from_forward_map :
            rev_values_list:'a Core.List.t -> key:'b -> unit  *)
            method remove_reverse : key:'b -> unit
            (*        method private remove_reverse_keys : 'b Core.List.t -> unit *)
            method remove_reverse_multi : key:'b -> unit
            method to_alist :
                     ?key_order:[ `Decreasing | `Increasing ] ->
                     unit ->
                     (ModuleA.Map.Key.t * 'b Core.List.t) Core_kernel__.Import.list
            method update :
                     key:'a -> f:('b Core.List.t option -> 'b Core.List.t) -> unit
 *)
end
