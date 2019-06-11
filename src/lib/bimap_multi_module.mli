module Bimap_multi_module :
functor (MapModule1 : Map.S) (MapModule2 : Map.S) ->
sig
  type t
  val create_t : fwdmap:MapModule2.key list MapModule1.t -> revmap:MapModule1.key list MapModule2.t -> t
  val get_forward_map : t -> MapModule2.key list MapModule1.t
  val get_reverse_map : t -> MapModule1.key list MapModule2.t
  val empty : unit -> t
  val is_empty : t -> bool
  val is_empty_reverse : t -> bool
  val mem : t-> key:MapModule1.key -> bool
  val mem_reverse : t -> key:MapModule2.key -> bool
  val mem_data_of_fwd_map :
    MapModule2.key list MapModule1.t -> key:MapModule1.key -> data:MapModule2.key -> bool
  val mem_data_of_rev_map :
    MapModule1.key list MapModule2.t -> key:MapModule2.key -> data:MapModule1.key -> bool
  val add : t -> key:MapModule1.key -> data:MapModule2.key -> t
  val add_reverse : t -> key:MapModule2.key -> data:MapModule1.key -> t
  val singleton : key:MapModule1.key -> data:MapModule2.key -> t
  val singleton_reverse :
    key:MapModule2.key -> data:MapModule1.key -> t
  (*val create_reverse_map_from_forward_map : unit -> unit
      val create_forward_map_from_reverse_map : unit -> unit*)
  val remove : t -> key:MapModule1.key -> t
  val remove_reverse : t -> key:MapModule2.key -> t
  val merge :
    t -> f:(MapModule1.key ->
       MapModule2.key list option ->
       'a option -> MapModule2.key list option) ->
    othermap:'a MapModule1.t -> t
  val merge_reverse :
    t -> f:(MapModule2.key ->
       MapModule1.key list option ->
       'a option -> MapModule1.key list option) ->
    othermap:'a MapModule2.t -> t
  val union :
    t -> f:(MapModule1.key ->
       MapModule2.key list ->
       MapModule2.key list -> MapModule2.key list option) ->
    othermap:MapModule2.key list MapModule1.t -> t
  val union_reverse :
    t -> f:(MapModule2.key ->
       MapModule1.key list ->
       MapModule1.key list -> MapModule1.key list option) ->
    othermap:MapModule1.key list MapModule2.t -> t
  val compare :
    t -> f:(MapModule2.key list -> MapModule2.key list -> int) ->
    othermap:MapModule2.key list MapModule1.t -> int
  val compare_reverse :
    t -> f:(MapModule1.key list -> MapModule1.key list -> int) ->
    othermap:MapModule1.key list MapModule2.t -> int
  val equal :
    t -> f:(MapModule2.key list -> MapModule2.key list -> bool) ->
    othermap:MapModule2.key list MapModule1.t -> bool
  val equal_reverse :
    t -> f:(MapModule1.key list -> MapModule1.key list -> bool) ->
    othermap:MapModule1.key list MapModule2.t -> bool
  val filter : t -> f:(MapModule1.key -> MapModule2.key list -> bool) -> t
  val filter_reverse :
    t -> f:(MapModule2.key -> MapModule1.key list -> bool) -> t
  val iter : t -> f:(MapModule1.key -> MapModule2.key list -> unit) -> unit
  val iter_reverse :
    t -> f:(MapModule2.key -> MapModule1.key list -> unit) -> unit
  val fold :
    t -> f:(MapModule1.key -> MapModule2.key list -> 'a -> 'a) -> 'a -> 'a
  val fold_reverse :
    t -> f:(MapModule2.key -> MapModule1.key list -> 'a -> 'a) -> 'a -> 'a
  val for_all : t -> f:(MapModule1.key -> MapModule2.key list -> bool) -> bool
  val for_all_reverse :
    t -> f:(MapModule2.key -> MapModule1.key list -> bool) -> bool
  val exists : t -> f:(MapModule1.key -> MapModule2.key list -> bool) -> bool
  val exists_reverse :
    t -> f:(MapModule2.key -> MapModule1.key list -> bool) -> bool
  val partition :
    t -> f:(MapModule1.key -> MapModule2.key list -> bool) ->
    MapModule2.key list MapModule1.t * MapModule2.key list MapModule1.t
  val partition_reverse :
    t -> f:(MapModule2.key -> MapModule1.key list -> bool) ->
    MapModule1.key list MapModule2.t * MapModule1.key list MapModule2.t
  val cardinal : t -> int
  val cardinal_reverse : t -> int
  val bindings : t -> (MapModule1.key * MapModule2.key list) list
  val bindings_reverse :
    t -> (MapModule2.key * MapModule1.key list) list
  val min_binding : t -> MapModule1.key * MapModule2.key list
  val min_binding_reverse : t -> MapModule2.key * MapModule1.key list
  val max_binding : t -> MapModule1.key * MapModule2.key list
  val max_binding_reverse : t -> MapModule2.key * MapModule1.key list
  val choose : t -> MapModule1.key * MapModule2.key list
  val choose_reverse : t -> MapModule2.key * MapModule1.key list
  val split :
    t -> key:MapModule1.key ->
    MapModule2.key list MapModule1.t * MapModule2.key list option *
      MapModule2.key list MapModule1.t
  val split_reverse :
    t -> key:MapModule2.key ->
    MapModule1.key list MapModule2.t * MapModule1.key list option *
      MapModule1.key list MapModule2.t
  val find_exn : t -> key:MapModule1.key -> MapModule2.key list
  val find_reverse_exn : t -> key:MapModule2.key -> MapModule1.key list
  val find : t -> key:MapModule1.key -> MapModule2.key list option
  val find_reverse : t -> key:MapModule2.key -> MapModule1.key list option
  val map : t -> f:(MapModule2.key list -> MapModule2.key list) -> t
  val map_reverse :
    t -> f:(MapModule1.key list -> MapModule1.key list) -> t
  val mapi :
    t -> f:(MapModule1.key -> MapModule2.key list -> MapModule2.key list) -> t
  val mapi_reverse :
    t -> f:(MapModule2.key -> MapModule1.key list -> MapModule1.key list) -> t
  val set_forward_map : t -> map:MapModule2.key list MapModule1.t -> t 
end
