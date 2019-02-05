module Bimap_class :
  functor (MapModule1 : Map.S) (MapModule2 : Map.S) ->
    sig
      class bimap_class :
        object
          val mutable forward_map : MapModule2.key MapModule1.t ref
          val mutable reverse_map : MapModule1.key MapModule2.t ref
          method add : key:MapModule1.key -> data:MapModule2.key -> unit
          method add_reverse :
            key:MapModule2.key -> data:MapModule1.key -> unit
          method bindings : unit -> (MapModule1.key * MapModule2.key) list
          method bindings_reverse :
            unit -> (MapModule2.key * MapModule1.key) list
          method cardinal : unit -> int
          method cardinal_reverse : unit -> int
          method choose : unit -> MapModule1.key * MapModule2.key
          method choose_reverse : unit -> MapModule2.key * MapModule1.key
          method compare :
            f:(MapModule2.key -> MapModule2.key -> int) ->
            othermap:MapModule2.key MapModule1.t -> int
          method compare_reverse :
            f:(MapModule1.key -> MapModule1.key -> int) ->
            othermap:MapModule1.key MapModule2.t -> int
          method private create_forward_map_from_reverse_map : unit -> unit
          method private create_reverse_map_from_forward_map : unit -> unit
          method empty : unit -> unit
          method private empty_forward_map : unit -> unit
          method private empty_reverse_map : unit -> unit
          method equal :
            f:(MapModule2.key -> MapModule2.key -> bool) ->
            othermap:MapModule2.key MapModule1.t -> bool
          method equal_reverse :
            f:(MapModule1.key -> MapModule1.key -> bool) ->
            othermap:MapModule1.key MapModule2.t -> bool
          method exists :
            f:(MapModule1.key -> MapModule2.key -> bool) -> bool
          method exists_reverse :
            f:(MapModule2.key -> MapModule1.key -> bool) -> bool
          method filter :
            f:(MapModule1.key -> MapModule2.key -> bool) ->
            MapModule2.key MapModule1.t
          method filter_reverse :
            f:(MapModule2.key -> MapModule1.key -> bool) ->
            MapModule1.key MapModule2.t
          method find : key:MapModule1.key -> MapModule2.key
          method find_reverse : key:MapModule2.key -> MapModule1.key
          method fold :
            f:(MapModule1.key ->
               MapModule2.key -> MapModule2.key -> MapModule2.key) ->
            init:MapModule2.key -> MapModule2.key
          method fold_reverse :
            f:(MapModule2.key ->
               MapModule1.key -> MapModule1.key -> MapModule1.key) ->
            init:MapModule1.key -> MapModule1.key
          method for_all :
            f:(MapModule1.key -> MapModule2.key -> bool) -> bool
          method for_all_reverse :
            f:(MapModule2.key -> MapModule1.key -> bool) -> bool
          method is_empty : unit -> bool
          method is_empty_reverse : unit -> bool
          method iter : f:(MapModule1.key -> MapModule2.key -> unit) -> unit
          method iter_reverse :
            f:(MapModule2.key -> MapModule1.key -> unit) -> unit
          method map :
            f:(MapModule2.key -> MapModule2.key) ->
            MapModule2.key MapModule1.t
          method map_reverse :
            f:(MapModule1.key -> MapModule1.key) ->
            MapModule1.key MapModule2.t
          method mapi :
            f:(MapModule2.key -> MapModule2.key) ->
            MapModule2.key MapModule1.t
          method mapi_reverse :
            f:(MapModule1.key -> MapModule1.key) ->
            MapModule1.key MapModule2.t
          method max_binding : unit -> MapModule1.key * MapModule2.key
          method max_binding_reverse :
            unit -> MapModule2.key * MapModule1.key
          method mem : MapModule1.key -> bool
          method mem_reverse : MapModule2.key -> bool
          method merge :
            f:(MapModule1.key ->
               MapModule2.key option ->
               MapModule2.key option -> MapModule2.key option) ->
            othermap:MapModule2.key MapModule1.t -> unit
          method merge_reverse :
            f:(MapModule2.key ->
               MapModule1.key option ->
               MapModule1.key option -> MapModule1.key option) ->
            othermap:MapModule1.key MapModule2.t -> unit
          method min_binding : unit -> MapModule1.key * MapModule2.key
          method min_binding_reverse :
            unit -> MapModule2.key * MapModule1.key
          method partition :
            f:(MapModule1.key -> MapModule2.key -> bool) ->
            MapModule2.key MapModule1.t * MapModule2.key MapModule1.t
          method partition_reverse :
            f:(MapModule2.key -> MapModule1.key -> bool) ->
            MapModule1.key MapModule2.t * MapModule1.key MapModule2.t
          method remove : key:MapModule1.key -> unit
          method remove_reverse : key:MapModule2.key -> unit
          method singleton :
            key:MapModule1.key -> data:MapModule2.key -> unit
          method singleton_reverse :
            key:MapModule2.key -> data:MapModule1.key -> unit
          method split :
            key:MapModule1.key ->
            MapModule2.key MapModule1.t * MapModule2.key option *
            MapModule2.key MapModule1.t
          method split_reverse :
            key:MapModule2.key ->
            MapModule1.key MapModule2.t * MapModule1.key option *
            MapModule1.key MapModule2.t
          method union :
            f:(MapModule1.key ->
               MapModule2.key -> MapModule2.key -> MapModule2.key option) ->
            othermap:MapModule2.key MapModule1.t -> unit
          method union_reverse :
            f:(MapModule2.key ->
               MapModule1.key -> MapModule1.key -> MapModule1.key option) ->
            othermap:MapModule1.key MapModule2.t -> unit
        end
    end
