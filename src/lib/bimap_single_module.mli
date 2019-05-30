(*===TODO===redo this to be immutable, functional, ie, return a value not unit for all functions*)
module Bimap_single_module :
functor (MapModule1 : Map.S) (MapModule2 : Map.S) ->
sig
  type t
  val empty : unit -> t
  val is_empty : t -> bool
  val mem : t -> key:MapModule1.key -> bool
  val mem_reverse : t -> key:MapModule2.key -> bool
  val add : t -> key:MapModule1.key -> data:MapModule2.key -> t
  val add_reverse : t -> key:MapModule2.key -> data:MapModule1.key -> t
  val singleton : t -> key:MapModule1.key -> data:MapModule2.key -> t
  val singleton_reverse : t -> key:MapModule2.key -> data:MapModule1.key -> t
  val remove : t -> key:MapModule1.key -> t
  val remove_reverse : t-> key:MapModule2.key -> t
  (*Needed to get access to the underlying map for use by 
        merge function. Better way to do this?*)
  val get_forward_map : t -> MapModule2.key MapModule1.t
  val get_reverse_map : t -> MapModule1.key MapModule2.t
  val merge : t ->
              f:(MapModule1.key ->
                 MapModule2.key option -> 'a option -> MapModule2.key option) ->
              othermap:'a MapModule1.t -> t
  val merge_reverse : t ->
                      f:(MapModule2.key ->
                         MapModule1.key option -> 'a option -> MapModule1.key option) ->
                      othermap:'a MapModule2.t -> t
  (*val union : (ModuleA.key -> ModuleB.key -> ModuleB.key -> ModuleB.key option) ->
                  MapModule2.key MapModule1.t -> MapModule2.key MapModule1.t -> unit*)
  val compare :
    t -> f:(MapModule2.key -> MapModule2.key -> int) ->
    othermap:MapModule2.key MapModule1.t -> int
  val compare_reverse :
    t -> f:(MapModule1.key -> MapModule1.key -> int) ->
    othermap:MapModule1.key MapModule2.t -> int
  val equal :
    t -> f:(MapModule2.key -> MapModule2.key -> bool) ->
    othermap:MapModule2.key MapModule1.t -> bool
  val equal_reverse :
    t -> f:(MapModule1.key -> MapModule1.key -> bool) ->
    othermap:MapModule1.key MapModule2.t -> bool
  val filter : t -> f:(MapModule1.key -> MapModule2.key -> bool) -> t
  val filter_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> bool) -> t
  (*---------------Not testing these 2 right now---------------*)
  val iter : t -> f:(MapModule1.key -> MapModule2.key -> unit) -> t
  val iter_reverse : t -> f:(MapModule2.key -> MapModule1.key -> unit) -> t
  (*-----------------------------------------------------------*)
  val fold : t -> f:(MapModule1.key -> MapModule2.key -> 'a -> 'a) -> 'a -> 'a
  val fold_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> 'a -> 'a) -> 'a -> 'a
  val for_all : t -> f:(MapModule1.key -> MapModule2.key -> bool) -> bool
  val for_all_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> bool) -> bool
  val exists : t -> f:(MapModule1.key -> MapModule2.key -> bool) -> bool
  val exists_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> bool) -> bool
  val partition :
    t -> f:(MapModule1.key -> MapModule2.key -> bool) ->
    MapModule2.key MapModule1.t * MapModule2.key MapModule1.t
  val partition_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> bool) ->
    MapModule1.key MapModule2.t * MapModule1.key MapModule2.t
  val cardinal : t -> int
  val cardinal_reverse : t -> int
  val bindings : t -> (MapModule1.key * MapModule2.key) list
  val bindings_reverse : t -> (MapModule2.key * MapModule1.key) list
  val min_binding_exn : t -> MapModule1.key * MapModule2.key
  val max_binding_exn : t -> MapModule1.key * MapModule2.key
  val min_binding : t -> (MapModule1.key * MapModule2.key) option
  val max_binding : t -> (MapModule1.key * MapModule2.key) option
  val min_binding_reverse_exn : t -> MapModule2.key * MapModule1.key
  val max_binding_reverse_exn : t -> MapModule2.key * MapModule1.key
  val min_binding_reverse : t -> (MapModule2.key * MapModule1.key) option
  val max_binding_reverse : t -> (MapModule2.key * MapModule1.key) option
  (*-----------------------untested-3-functions-------------------*)
  val choose : t -> MapModule1.key * MapModule2.key
  val choose_reverse : t -> MapModule2.key * MapModule1.key
  val split_reverse :
    t -> key:MapModule2.key ->
    MapModule1.key MapModule2.t * MapModule1.key option *
      MapModule1.key MapModule2.t
  (*--------------------------------------------------------------*)
  val split :
    t -> key:MapModule1.key ->
    MapModule2.key MapModule1.t * MapModule2.key option *
      MapModule2.key MapModule1.t
  val find_exn : t -> key:MapModule1.key -> MapModule2.key
  val find_reverse_exn : t -> key:MapModule2.key -> MapModule1.key
  val find : t -> key:MapModule1.key -> MapModule2.key option
  val find_reverse : t -> key:MapModule2.key -> MapModule1.key option
  val map : t -> f:(MapModule2.key -> MapModule2.key) -> t
  val map_reverse : t -> f:(MapModule1.key -> MapModule1.key) -> t
  (*---------------------untested-functions----------------*)
  val mapi :
    t -> f:(MapModule1.key -> MapModule2.key -> MapModule2.key) -> t
  val mapi_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> MapModule1.key) -> t
  (*---------------------------------------------------------*)
end
