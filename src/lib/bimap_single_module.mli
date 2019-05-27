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
(*  val remove : key:MapModule1.key -> unit
  val remove_reverse : key:MapModule2.key -> unit
  (*Needed to get access to the underlying map for use by 
        merge function. Better way to do this?*)
  val get_forward_map : unit -> MapModule2.key MapModule1.t
  val get_reverse_map : unit -> MapModule1.key MapModule2.t
  val merge :
    f:(MapModule1.key ->
       MapModule2.key option -> 'a option -> MapModule2.key option) ->
    othermap:'a MapModule1.t -> unit
  val merge_reverse :
    f:(MapModule2.key ->
       MapModule1.key option -> 'a option -> MapModule1.key option) ->
    othermap:'a MapModule2.t -> unit
  (*val union : (ModuleA.key -> ModuleB.key -> ModuleB.key -> ModuleB.key option) ->
                  MapModule2.key MapModule1.t -> MapModule2.key MapModule1.t -> unit*)
  val compare :
    f:(MapModule2.key -> MapModule2.key -> int) ->
    othermap:MapModule2.key MapModule1.t -> int
  val compare_reverse :
    f:(MapModule1.key -> MapModule1.key -> int) ->
    othermap:MapModule1.key MapModule2.t -> int
  val equal :
    f:(MapModule2.key -> MapModule2.key -> bool) ->
    othermap:MapModule2.key MapModule1.t -> bool
  val equal_reverse :
    f:(MapModule1.key -> MapModule1.key -> bool) ->
    othermap:MapModule1.key MapModule2.t -> bool
  val filter : f:(MapModule1.key -> MapModule2.key -> bool) -> unit
  val filter_reverse :
    f:(MapModule2.key -> MapModule1.key -> bool) -> unit
  (*---------------Not testing these 2 right now---------------*)
  val iter : f:(MapModule1.key -> MapModule2.key -> unit) -> unit
  val iter_reverse : f:(MapModule2.key -> MapModule1.key -> unit) -> unit
  (*-----------------------------------------------------------*)
  val fold : f:(MapModule1.key -> MapModule2.key -> 'a -> 'a) -> 'a -> 'a
  val fold_reverse :
    f:(MapModule2.key -> MapModule1.key -> 'a -> 'a) -> 'a -> 'a
  val for_all : f:(MapModule1.key -> MapModule2.key -> bool) -> bool
  val for_all_reverse :
    f:(MapModule2.key -> MapModule1.key -> bool) -> bool
  val exists : f:(MapModule1.key -> MapModule2.key -> bool) -> bool
  val exists_reverse :
    f:(MapModule2.key -> MapModule1.key -> bool) -> bool
  val partition :
    f:(MapModule1.key -> MapModule2.key -> bool) ->
    MapModule2.key MapModule1.t * MapModule2.key MapModule1.t
  val partition_reverse :
    f:(MapModule2.key -> MapModule1.key -> bool) ->
    MapModule1.key MapModule2.t * MapModule1.key MapModule2.t
  val cardinal : unit -> int
  val cardinal_reverse : unit -> int
  val bindings : unit -> (MapModule1.key * MapModule2.key) list
  val bindings_reverse : unit -> (MapModule2.key * MapModule1.key) list
  val min_binding_exn : unit -> MapModule1.key * MapModule2.key
  val max_binding_exn : unit -> MapModule1.key * MapModule2.key
  val min_binding : unit -> (MapModule1.key * MapModule2.key) option
  val max_binding : unit -> (MapModule1.key * MapModule2.key) option
  val min_binding_reverse_exn : unit -> MapModule2.key * MapModule1.key
  val max_binding_reverse_exn : unit -> MapModule2.key * MapModule1.key
  val min_binding_reverse : unit -> (MapModule2.key * MapModule1.key) option
  val max_binding_reverse : unit -> (MapModule2.key * MapModule1.key) option
  (*-----------------------untested-3-functions-------------------*)
  val choose : unit -> MapModule1.key * MapModule2.key
  val choose_reverse : unit -> MapModule2.key * MapModule1.key
  val split_reverse :
    key:MapModule2.key ->
    MapModule1.key MapModule2.t * MapModule1.key option *
      MapModule1.key MapModule2.t
  (*--------------------------------------------------------------*)
  val split :
    key:MapModule1.key ->
    MapModule2.key MapModule1.t * MapModule2.key option *
      MapModule2.key MapModule1.t
  val find_exn : key:MapModule1.key -> MapModule2.key
  val find_reverse_exn : key:MapModule2.key -> MapModule1.key
  val find : key:MapModule1.key -> MapModule2.key option
  val find_reverse : key:MapModule2.key -> MapModule1.key option
  val map : f:(MapModule2.key -> MapModule2.key) -> unit
  val map_reverse : f:(MapModule1.key -> MapModule1.key) -> unit
  (*---------------------untested-3-functions----------------*)
  val mapi :
    f:(MapModule1.key -> MapModule2.key -> MapModule2.key) -> unit
  val mapi_reverse :
    f:(MapModule2.key -> MapModule1.key -> MapModule1.key) -> unit
  val set_forward_map : map:(MapModule2.key MapModule1.t) -> unit
                                                               (*---------------------------------------------------------*)
 *)
end
