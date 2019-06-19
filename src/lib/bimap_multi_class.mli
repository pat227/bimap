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
    method bindings : unit -> (ModuleA.key * ModuleB.key list) list
    method bindings_reverse : unit -> (ModuleB.key * ModuleA.key list) list
    method choose : unit -> ModuleA.key * ModuleB.key list
    method choose_reverse : unit -> ModuleB.key * ModuleA.key list
    method cardinal : unit -> int
    method cardinal_reverse : unit -> int
    method compare : (ModuleB.key list -> ModuleB.key list -> int) -> othermap:ModuleB.key list ModuleA.t -> int
    method compare_reverse : (ModuleA.key list -> ModuleA.key list -> int) -> othermap:ModuleA.key list ModuleB.t -> int
    method private create_forward_map_from_reverse_map : unit -> unit
    method private create_reverse_map_from_forward_map : unit -> unit
    method private empty_forward_map : unit -> unit
    method private empty_reverse_map : unit -> unit
    method empty : unit -> unit
    method equal : (ModuleB.key list -> ModuleB.key list -> bool) -> othermap:ModuleB.key list ModuleA.t -> bool
    method equal_reverse : (ModuleA.key list -> ModuleA.key list -> bool) -> othermap:ModuleA.key list ModuleB.t -> bool
    method exists : f:(ModuleA.key -> ModuleB.key list -> bool) -> bool
    method exists_reverse : f:(ModuleB.key -> ModuleA.key list -> bool) -> bool
    method filter : f:(ModuleA.key -> ModuleB.key list -> bool) -> unit
    method filter_reverse : f:(ModuleB.key -> ModuleA.key list -> bool) -> unit
    method find_opt : key:ModuleA.key -> ModuleB.key list option
    method find_reverse_opt :
             key:ModuleB.key -> ModuleA.key list option
    method find : key:ModuleA.key -> ModuleB.key list
    method find_reverse : key:ModuleB.key -> ModuleA.key list
    method fold : (ModuleA.key -> ModuleB.key list -> 'e -> 'e) -> 'e  -> 'e
    method fold_reverse : (ModuleB.key -> ModuleA.key list -> 'e -> 'e) -> 'e -> 'e
    method for_all : f:(ModuleA.key -> ModuleB.key list -> bool) -> bool
    method for_all_reverse :
      f:(ModuleB.key -> ModuleA.key list -> bool) -> bool
    method iter : f:(ModuleA.key -> ModuleB.key list -> unit) -> unit
    method iter_reverse : f:(ModuleB.key -> ModuleA.key list -> unit) -> unit                                                          
    method is_empty : bool
    method map : f:(ModuleB.key list -> ModuleB.key list) -> unit
    method map_reverse : f:(ModuleA.key list -> ModuleA.key list) -> unit
    method mapi : f:(ModuleA.key -> ModuleB.key list -> ModuleB.key list) -> unit
    method mapi_reverse : f:(ModuleB.key -> ModuleA.key list -> ModuleA.key list) -> unit
    method max_binding : ModuleA.key * ModuleB.key list
    method max_binding_reverse : (ModuleB.key * ModuleA.key list)
    method min_binding : (ModuleA.key * ModuleB.key list)
    method min_binding_reverse : (ModuleB.key * ModuleA.key list)
    method mem : ModuleA.key -> bool
    method mem_reverse : ModuleB.key -> bool
    method merge : (ModuleA.key -> ModuleB.key list option -> ModuleB.key list option -> ModuleB.key list option) ->
                   othermap:ModuleB.key list ModuleA.t -> unit
    method merge_reverse : (ModuleB.key -> ModuleA.key list option -> ModuleA.key list option -> ModuleA.key list option) ->
                           othermap:ModuleA.key list ModuleB.t -> unit
    method partition :
             f:(ModuleA.key -> ModuleB.key list -> bool) ->
             ModuleB.key list ModuleA.t * ModuleB.key list ModuleA.t
    method partition_reverse :
             f:(ModuleB.key -> ModuleA.key list -> bool) ->
             ModuleA.key list ModuleB.t * ModuleA.key list ModuleB.t
    method remove : key:ModuleA.key -> unit
    method remove_reverse : key:ModuleB.key -> unit
    method remove_multi_opt : key:ModuleA.key -> ModuleB.key option
    method remove_reverse_multi_opt : key:ModuleB.key -> ModuleA.key option
    method singleton : key:ModuleA.key -> data:ModuleB.key -> unit
    method singleton_reverse : key:ModuleB.key -> data:ModuleA.key -> unit
    method split :
             key:ModuleA.key ->
             ModuleB.key list ModuleA.t * ModuleB.key list option *
               ModuleB.key list ModuleA.t
    method split_reverse :
             key:ModuleB.key ->
             ModuleA.key list ModuleB.t * ModuleA.key list option *
               ModuleA.key list ModuleB.t
    method update : key:ModuleA.key -> f:(ModuleB.key list option -> ModuleB.key list option) -> unit
    method update_reverse : key:ModuleB.key -> f:(ModuleA.key list option -> ModuleA.key list option) -> unit
    method union : (ModuleA.key -> ModuleB.key list -> ModuleB.key list -> ModuleB.key list option) ->
                   othermap:ModuleB.key list ModuleA.t -> unit
    method union_reverse : (ModuleB.key -> ModuleA.key list -> ModuleA.key list -> ModuleA.key list option) ->
                           othermap:ModuleA.key list ModuleB.t -> unit
  end
end
