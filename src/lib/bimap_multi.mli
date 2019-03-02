module Bimap_multi :
  functor (MapModule1 : Map.S) (MapModule2 : Map.S) ->
    sig
      val get_forward_map : unit -> MapModule2.key list MapModule1.t
      val get_reverse_map : unit -> MapModule1.key list MapModule2.t
      val empty : unit -> unit
      val is_empty : unit -> bool
      val is_empty_reverse : unit -> bool
      val mem : key:MapModule1.key -> bool
      val mem_reverse : key:MapModule2.key -> bool
      val mem_data_of_fwd_map :
        key:MapModule1.key -> data:MapModule2.key -> bool
      val mem_data_of_rev_map :
        key:MapModule2.key -> data:MapModule1.key -> bool
      val add : key:MapModule1.key -> data:MapModule2.key -> unit
      val add_reverse : key:MapModule2.key -> data:MapModule1.key -> unit
      val singleton : key:MapModule1.key -> data:MapModule2.key -> unit
      val singleton_reverse :
        key:MapModule2.key -> data:MapModule1.key -> unit
      (*val create_reverse_map_from_forward_map : unit -> unit
      val create_forward_map_from_reverse_map : unit -> unit*)
      val remove : key:MapModule1.key -> unit
      val remove_reverse : key:MapModule2.key -> unit
      val merge :
        f:(MapModule1.key ->
           MapModule2.key list option ->
           'a option -> MapModule2.key list option) ->
        othermap:'a MapModule1.t -> unit
      val merge_reverse :
        f:(MapModule2.key ->
           MapModule1.key list option ->
           'a option -> MapModule1.key list option) ->
        othermap:'a MapModule2.t -> unit
      val union :
        f:(MapModule1.key ->
           MapModule2.key list ->
           MapModule2.key list -> MapModule2.key list option) ->
        othermap:MapModule2.key list MapModule1.t -> unit
      val union_reverse :
        f:(MapModule2.key ->
           MapModule1.key list ->
           MapModule1.key list -> MapModule1.key list option) ->
        othermap:MapModule1.key list MapModule2.t -> unit
      val compare :
        f:(MapModule2.key list -> MapModule2.key list -> int) ->
        othermap:MapModule2.key list MapModule1.t -> int
      val compare_reverse :
        f:(MapModule1.key list -> MapModule1.key list -> int) ->
        othermap:MapModule1.key list MapModule2.t -> int
      val equal :
        f:(MapModule2.key list -> MapModule2.key list -> bool) ->
        othermap:MapModule2.key list MapModule1.t -> bool
      val equal_reverse :
        f:(MapModule1.key list -> MapModule1.key list -> bool) ->
        othermap:MapModule1.key list MapModule2.t -> bool
      val filter : f:(MapModule1.key -> MapModule2.key list -> bool) -> unit
      val filter_reverse :
        f:(MapModule2.key -> MapModule1.key list -> bool) -> unit
      val iter : f:(MapModule1.key -> MapModule2.key list -> unit) -> unit
      val iter_reverse :
        f:(MapModule2.key -> MapModule1.key list -> unit) -> unit
      val fold :
        f:(MapModule1.key -> MapModule2.key list -> 'a -> 'a) -> 'a -> 'a
      val fold_reverse :
        f:(MapModule2.key -> MapModule1.key list -> 'a -> 'a) -> 'a -> 'a
      val for_all : f:(MapModule1.key -> MapModule2.key list -> bool) -> bool
      val for_all_reverse :
        f:(MapModule2.key -> MapModule1.key list -> bool) -> bool
      val exists : f:(MapModule1.key -> MapModule2.key list -> bool) -> bool
      val exists_reverse :
        f:(MapModule2.key -> MapModule1.key list -> bool) -> bool
      val partition :
        f:(MapModule1.key -> MapModule2.key list -> bool) ->
        MapModule2.key list MapModule1.t * MapModule2.key list MapModule1.t
      val partition_reverse :
        f:(MapModule2.key -> MapModule1.key list -> bool) ->
        MapModule1.key list MapModule2.t * MapModule1.key list MapModule2.t
      val cardinal : unit -> int
      val cardinal_reverse : unit -> int
      val bindings : unit -> (MapModule1.key * MapModule2.key list) list
      val bindings_reverse :
        unit -> (MapModule2.key * MapModule1.key list) list
      val min_binding : unit -> MapModule1.key * MapModule2.key list
      val min_binding_reverse : unit -> MapModule2.key * MapModule1.key list
      val max_binding : unit -> MapModule1.key * MapModule2.key list
      val max_binding_reverse : unit -> MapModule2.key * MapModule1.key list
      val choose : unit -> MapModule1.key * MapModule2.key list
      val choose_reverse : unit -> MapModule2.key * MapModule1.key list
      val split :
        key:MapModule1.key ->
        MapModule2.key list MapModule1.t * MapModule2.key list option *
        MapModule2.key list MapModule1.t
      val split_reverse :
        key:MapModule2.key ->
        MapModule1.key list MapModule2.t * MapModule1.key list option *
        MapModule1.key list MapModule2.t
      val find_exn : key:MapModule1.key -> MapModule2.key list
      val find_reverse_exn : key:MapModule2.key -> MapModule1.key list
      val find : key:MapModule1.key -> MapModule2.key list option
      val find_reverse : key:MapModule2.key -> MapModule1.key list option
      val map : f:(MapModule2.key list -> MapModule2.key list) -> unit
      val map_reverse :
        f:(MapModule1.key list -> MapModule1.key list) -> unit
      val mapi :
        f:(MapModule1.key -> MapModule2.key list -> MapModule2.key list) ->
        unit
      val mapi_reverse :
        f:(MapModule2.key -> MapModule1.key list -> MapModule1.key list) ->
        unit
      val set_forward_map : map:MapModule2.key list MapModule1.t -> unit
    end
