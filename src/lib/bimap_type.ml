module Bimap_type : sig 
  type ('a, 'b) t =
      <add : key:'a -> data:'b -> unit;
      add_inverse : key:'b -> data:'a -> unit;
      change : key:'a -> f:('b option -> 'b option) -> unit;
      change_inverse : key:'b -> f:('a option -> 'a option) -> unit;
      count : f:('b -> bool) -> int;
      count_inverse : f:('a -> bool) -> int;
      counti : f:(key:'a -> data:'b -> bool) -> int;
      data : 'b list;
      data_inverse : 'a list;
      empty : unit -> unit;
      exists : f:('b -> bool) -> bool;
      exists_inverse : f:('a -> bool) -> bool;
      existsi : f:(key:'a -> data:'b -> bool) -> bool;
      existsi_inverse : f:(key:'b -> data:'a -> bool) -> bool;
      find : key:'a -> 'b option;
      find_exn : key:'a -> 'b;
      find_exn_inverse : key:'b -> 'a;
      find_inverse : key:'b -> 'a option;
      filter : f:('b -> bool) -> unit;
      filter_inverse : f:('a -> bool) -> unit;
      filteri : f:(key:'a -> data:'b -> bool) -> unit;
      filteri_inverse : f:(key:'b -> data:'a -> bool) -> unit;
      filter_keys : f:('a -> bool) -> unit;
      filter_keys_inverse : f:('b -> bool) -> unit;
      filter_map : f:('b -> 'b option) -> unit;
      filter_map_inverse : f:('a -> 'a option) -> unit;
      (*fold : init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e;
      fold_inverse : init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e;
      fold_right : init:'e -> f:(key:'a -> data:'b -> 'e -> 'e) -> 'e;
      fold_right_inverse : init:'e -> f:(key:'b -> data:'a -> 'e -> 'e) -> 'e;*)
      for_all : f:('b -> bool) -> bool;
      for_all_inverse : f:('a -> bool) -> bool;
      is_empty : bool;
      iter : f:('b -> unit) -> unit;
      iter_inverse : f:('a -> unit) -> unit;
      iter_keys : f:('a -> unit) -> unit;
      iter_keys_inverse : f:('b -> unit) -> unit;
      iteri : f:(key:'a -> data:'b -> unit) -> unit;
      iteri_inverse : f:(key:'b -> data:'a -> unit) -> unit;
      keys : 'a list;
      keys_inverse : 'b list;
      length : int;
      map : f:('b -> 'b) -> unit;
      map_inverse : f:('a -> 'a) -> unit;
      mapi : f:(key:'a -> data:'b -> 'b) -> unit;
      mapi_inverse : f:(key:'b -> data:'a -> 'a) -> unit;
      mem : 'a -> bool;
      mem_inverse : 'b -> bool;
      min_elt : ('a * 'b) option;
      min_elt_exn : ('a * 'b);
      min_elt_inverse : ('b * 'a) option;
      min_elt_exn_inverse : ('b * 'a);
      max_elt : ('a * 'b) option;
      max_elt_exn : ('a * 'b);
      max_elt_inverse : ('b * 'a) option;
      max_elt_exn_inverse : ('b * 'a);
      nth : int -> ('a * 'b) option;
      nth_inverse : int -> ('b * 'a) option;
      remove : key:'a -> unit;
      remove_inverse : key:'b -> unit;
      to_alist : ?key_order:[`Increasing | `Decreasing] -> unit -> ('a * 'b) list;
      update : 'a -> f:('b option -> 'b) -> unit>
(*  val make : ('a, 'b, 'c) Core.Map.t -> ('b, 'a, 'd) Core.Map.t -> ('a, 'b) t*)
end = Bimap
