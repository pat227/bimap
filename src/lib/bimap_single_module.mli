module Bimap_single_module :
functor (MapModule1 : Map.S) (MapModule2 : Map.S) ->
sig
  type t
  val create_t : fwdmap:MapModule2.key MapModule1.t -> revmap:MapModule1.key MapModule2.t -> t
  val empty : unit -> t
  val is_empty : t -> bool

  val add : t -> key:MapModule1.key -> data:MapModule2.key -> t
  val add_reverse : t -> key:MapModule2.key -> data:MapModule1.key -> t
  val bindings : t -> (MapModule1.key * MapModule2.key) list
  val bindings_reverse : t -> (MapModule2.key * MapModule1.key) list
  val cardinal : t -> int
  val cardinal_reverse : t -> int
  (*-----------untested-------------*)
  val choose : t -> MapModule1.key * MapModule2.key
  val choose_reverse : t -> MapModule2.key * MapModule1.key
  (*--------------------------------*)
  val compare :
    t -> f:(MapModule2.key -> MapModule2.key -> int) ->
    othermap:MapModule2.key MapModule1.t -> int
  val compare_reverse :
    t -> f:(MapModule1.key -> MapModule1.key -> int) ->
    othermap:MapModule1.key MapModule2.t -> int
  val create_reverse_map_from_forward_map :
    forward_map:MapModule2.key MapModule1.t -> MapModule1.key MapModule2.t
  val create_forward_map_from_reverse_map :
    reverse_map:MapModule1.key MapModule2.t -> MapModule2.key MapModule1.t
  (*Needed to get access to the underlying map for use by 
        merge function, and others.*)
  val equal :
    t -> f:(MapModule2.key -> MapModule2.key -> bool) ->
    othermap:MapModule2.key MapModule1.t -> bool
  val equal_reverse :
    t -> f:(MapModule1.key -> MapModule1.key -> bool) ->
    othermap:MapModule1.key MapModule2.t -> bool
  val exists : t -> f:(MapModule1.key -> MapModule2.key -> bool) -> bool
  val exists_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> bool) -> bool
  val filter : t -> f:(MapModule1.key -> MapModule2.key -> bool) -> t
  val filter_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> bool) -> t
  val find : t -> key:MapModule1.key -> MapModule2.key
  val find_reverse : t -> key:MapModule2.key -> MapModule1.key
  val find_opt : t -> key:MapModule1.key -> MapModule2.key option
  val find_reverse_opt : t -> key:MapModule2.key -> MapModule1.key option
  val fold : t -> f:(MapModule1.key -> MapModule2.key -> 'a -> 'a) -> 'a -> 'a
  val fold_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> 'a -> 'a) -> 'a -> 'a
  val for_all : t -> f:(MapModule1.key -> MapModule2.key -> bool) -> bool
  val for_all_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> bool) -> bool
  val get_forward_map : t -> MapModule2.key MapModule1.t
  val get_reverse_map : t -> MapModule1.key MapModule2.t
  (*---------------Not testing these 2 right now---------------*)
  val iter : t -> f:(MapModule1.key -> MapModule2.key -> unit) -> unit
  val iter_reverse : t -> f:(MapModule2.key -> MapModule1.key -> unit) -> unit
  (*-----------------------------------------------------------*)
  val map : t -> f:(MapModule2.key -> MapModule2.key) -> t
  val map_reverse : t -> f:(MapModule1.key -> MapModule1.key) -> t
  (*---------------------untested-functions----------------*)
  val mapi :
    t -> f:(MapModule1.key -> MapModule2.key -> MapModule2.key) -> t
  val mapi_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> MapModule1.key) -> t
  (*---------------------------------------------------------*)
  val min_binding_exn : t -> MapModule1.key * MapModule2.key
  val max_binding_exn : t -> MapModule1.key * MapModule2.key
  val min_binding : t -> (MapModule1.key * MapModule2.key) option
  val max_binding : t -> (MapModule1.key * MapModule2.key) option
  val min_binding_reverse_exn : t -> MapModule2.key * MapModule1.key
  val max_binding_reverse_exn : t -> MapModule2.key * MapModule1.key
  val min_binding_reverse : t -> (MapModule2.key * MapModule1.key) option
  val max_binding_reverse : t -> (MapModule2.key * MapModule1.key) option
  val mem : t -> key:MapModule1.key -> bool
  val mem_reverse : t -> key:MapModule2.key -> bool
  val merge : t ->
              f:(MapModule1.key ->
                 MapModule2.key option -> 'a option -> MapModule2.key option) ->
              othermap:'a MapModule1.t -> t
  val merge_reverse : t ->
                      f:(MapModule2.key ->
                         MapModule1.key option -> 'a option -> MapModule1.key option) ->
                      othermap:'a MapModule2.t -> t
  val partition :
    t -> f:(MapModule1.key -> MapModule2.key -> bool) ->
    MapModule2.key MapModule1.t * MapModule2.key MapModule1.t
  val partition_reverse :
    t -> f:(MapModule2.key -> MapModule1.key -> bool) ->
    MapModule1.key MapModule2.t * MapModule1.key MapModule2.t
  val remove : t -> key:MapModule1.key -> t
  val remove_reverse : t-> key:MapModule2.key -> t
  val singleton : t -> key:MapModule1.key -> data:MapModule2.key -> t
  val singleton_reverse : t -> key:MapModule2.key -> data:MapModule1.key -> t
  (*-----------------------untested-------------------*)
  val split_reverse :
    t -> key:MapModule2.key ->
    MapModule1.key MapModule2.t * MapModule1.key option *
      MapModule1.key MapModule2.t
  (*--------------------------------------------------------------*)
  val split :
    t -> key:MapModule1.key ->
    MapModule2.key MapModule1.t * MapModule2.key option *
      MapModule2.key MapModule1.t
  val union : t -> f:(MapModule1.key -> MapModule2.key -> MapModule2.key -> MapModule2.key option) ->
              othermap:MapModule2.key MapModule1.t -> t

end
