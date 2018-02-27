module Bimap :
  sig
    class ['a, 'b] bimap_class :
      ('a, 'b, 'c) Core.Map.t ->
      ('b, 'a, 'd) Core.Map.t ->
      object
        constraint 'b = 'e list
        val mutable forward_map : ('a, 'b, 'c) Core.Map.t ref
        val mutable reverse_map : ('b, 'a, 'd) Core.Map.t ref
        method add : 'a -> 'b -> unit
        method add_inverse : key:'b -> data:'a -> unit
        method add_multi : key:'a -> data:'e -> unit
        method change :
          key:'a ->
          f:('b Core_kernel__.Import.option -> 'b Core_kernel__.Import.option) ->
          unit
        method change_inverse :
          key:'b ->
          f:('a Core_kernel__.Import.option -> 'a Core_kernel__.Import.option) ->
          unit
        method data : 'b Core_kernel__.Import.list
        method data_inverse : 'a Core_kernel__.Import.list
        method find : key:'a -> 'b Core_kernel__.Import.option
        method find_exn : key:'a -> 'b
        method find_exn_inverse : key:'b -> 'a
        method find_inverse : key:'b -> 'a Core_kernel__.Import.option
        method is_empty : Core_kernel__.Import.bool
        method iter :
          f:('b -> Core_kernel__.Import.unit) -> Core_kernel__.Import.unit
        method iter_inverse :
          f:('a -> Core_kernel__.Import.unit) -> Core_kernel__.Import.unit
        method iter_keys :
          f:('a -> Core_kernel__.Import.unit) -> Core_kernel__.Import.unit
        method iter_keys_inverse :
          f:('b -> Core_kernel__.Import.unit) -> Core_kernel__.Import.unit
        method iteri :
          f:(key:'a -> data:'b -> Core_kernel__.Import.unit) ->
          Core_kernel__.Import.unit
        method iteri_inverse :
          f:(key:'b -> data:'a -> Core_kernel__.Import.unit) ->
          Core_kernel__.Import.unit
        method keys : 'a Core_kernel__.Import.list
        method keys_inverse : 'b Core_kernel__.Import.list
        method length : Core_kernel__.Import.int
        method map : f:('b -> 'b) -> Core_kernel__.Import.unit
        method map_inverse : f:('a -> 'a) -> Core_kernel__.Import.unit
        method mapi :
          f:(key:'a -> data:'b -> 'b) -> Core_kernel__.Import.unit
        method mapi_inverse :
          f:(key:'b -> data:'a -> 'a) -> Core_kernel__.Import.unit
        method mem : 'a -> Core_kernel__.Import.bool
        method mem_inverse : 'b -> Core_kernel__.Import.bool
        method remove : key:'a -> unit
        method remove_inverse : key:'b -> unit
        method update :
          'a -> f:('b Core_kernel__.Import.option -> 'b) -> unit
      end
  end
