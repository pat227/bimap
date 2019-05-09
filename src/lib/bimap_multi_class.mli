module Bimap_multi_class :
functor (ModuleA : Map.S) (ModuleB : Map.S) ->
sig
  class bimap_multi_class :
  object
    val mutable forward_map : ModuleB.key list ModuleA.t ref
    val mutable reverse_map : ModuleA.key list ModuleB.t ref
    method add_multi : key:ModuleA.key -> data:ModuleB.key -> unit
    method add_multi_reverse :
             key:ModuleB.key -> data:ModuleA.key -> unit
    method cardinal : unit -> int
    method cardinal_reverse : unit -> int
    method private create_forward_map_from_reverse_map : unit -> unit
    method private create_reverse_map_from_forward_map : unit -> unit
    method private empty_forward_map : unit -> unit
    method private empty_reverse_map : unit -> unit
    method private remove_fwd_key_from_reverse_map :
                     fwd_values_list:ModuleB.key list -> key:ModuleA.key -> unit
    method private remove_rev_key_from_forward_map :
                     rev_values_list:ModuleA.key list -> key:ModuleB.key -> unit
    method empty : unit -> unit
    method mem : ModuleA.key -> bool
    method mem_reverse : ModuleB.key -> bool
    method update : key:ModuleA.key -> f:(ModuleB.key list option -> ModuleB.key list option) -> unit                                          
(*                                                
            method counti :
                     f:(key:'a -> data:'b list -> bool) -> int
            (*        method private create_forward_map_from_reverse_map : unit -> unit
          method private create_reverse_map_from_forward_map : unit -> unit *)
            method data : 'b Core.List.t list
            method data_reverse : 'a Core.List.t list
            method empty : unit -> unit
            (*        method private empty_forward_map : unit -> unit
          method private empty_reverse_map : unit -> unit *)
            method exists : f:('b Core.List.t -> bool) -> bool
            method exists_reverse : f:('a Core.List.t -> bool) -> bool
            method existsi :
                     f:(key:ModuleA.Map.Key.t -> data:'b Core.List.t -> bool) -> bool
            method existsi_reverse :
                     f:(key:ModuleB.Map.Key.t -> data:'a Core.List.t -> bool) -> bool
            method filter : f:('b Core.List.t -> bool) -> unit -> unit
            method filter_keys : f:(ModuleA.Map.Key.t -> bool) -> unit -> unit
            method filter_keys_reverse :
                     f:(ModuleB.Map.Key.t -> bool) -> unit -> unit
            method filter_map :
                     f:('b Core.List.t -> 'b Core.List.t option) -> unit -> unit
            method filter_map_reverse :
                     f:('a Core.List.t -> 'a Core.List.t option) -> unit -> unit
            method filter_reverse : f:('a Core.List.t -> bool) -> unit -> unit
            method filteri :
                     f:(key:ModuleA.Map.Key.t -> data:'b Core.List.t -> bool) ->
                     unit -> unit
            method filteri_reverse :
                     f:(key:ModuleB.Map.Key.t -> data:'a Core.List.t -> bool) ->
                     unit -> unit
            method find : key:ModuleA.Map.Key.t -> 'b Core.List.t option
            method find_exn : key:'a -> 'b Core.List.t
            method find_exn_reverse : key:ModuleB.Map.Key.t -> 'a Core.List.t
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

                                                                              *)
          end
end
