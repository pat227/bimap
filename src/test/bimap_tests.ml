module Bimap = Bimap.Bimap_single_module
module Bimap_multi = Bimap_multi_module.Bimap_multi_module
open OUnit2
module Bimap_tests = struct

  let oc = stdout
  let print_n_flush s =
    (output_string oc s;
     flush oc);;
  
  let rec print_n_flush_alist ~sep ~to_string_func l =
    match l with
    | [] -> ()
    | h :: t ->
       let s = to_string_func h in 
       let () = print_n_flush (s ^ sep) in
       print_n_flush_alist ~sep ~to_string_func t;;

  let rec print_n_flush_alistlist ~sep ~to_string_func l =
    match l with
    | [] -> ()
    | h :: t ->
       let () = print_n_flush " <|> " in 
       let () = print_n_flush_alist ~sep ~to_string_func h in
       print_n_flush_alistlist ~sep ~to_string_func t;;
(*
  let rec list_list_mem l v =
    match l with
    | [] -> false
    | h::t ->
       if Core.List.mem ~equal:(fun x y -> Core.String.equal x y) h v then true else
	 list_list_mem t v

  let rec key_list_list_mem l v =
    match l with
    | [] -> false
    | h::t ->
       if Core.List.mem ~equal:(fun x y -> Core.String.equal x y) h v then true else
	 list_list_mem t v
 *)
 

  let test1 text_ctx =
    let module IntMap = Map.Make(Int64) in
    let module StrMap = Map.Make(String) in
    let module Bimap_test1 = Bimap(IntMap)(StrMap) in
    begin
      Bimap_test1.add ~key:Int64.one ~data:"one";
      Bimap_test1.add ~key:(Int64.of_int 2) ~data:"two";
      (* 1 -> one | 2 -> two *)
      assert_equal 2 (Bimap_test1.cardinal ());
      assert_equal 2 (Bimap_test1.cardinal_reverse ());
      assert_equal 2 (Bimap_test1.cardinal ());
      assert_equal 2 (Bimap_test1.cardinal_reverse ());
      assert_equal false (Bimap_test1.is_empty ());
      assert_equal "one" (Bimap_test1.find_exn ~key:(Int64.of_int 1));
      assert_equal "two" (Bimap_test1.find_exn ~key:(Int64.of_int 2));
      assert_equal (Int64.of_int 1) (Bimap_test1.find_reverse_exn ~key:"one");
      assert_equal (Int64.of_int 2) (Bimap_test1.find_reverse_exn ~key:"two");
      Bimap_test1.add_reverse ~key:"three" ~data:(Int64.of_int 3);
      (* 1 -> one | 2 -> two | 3 -> three *)
      assert_equal 3 (Bimap_test1.cardinal ());
      assert_equal 3 (Bimap_test1.cardinal_reverse ());
      assert_equal "three" (Bimap_test1.find_exn ~key:(Int64.of_int 3));
      assert_equal (Int64.of_int 3) (Bimap_test1.find_reverse_exn ~key:"three");
      assert_equal [((Int64.of_int 1),"one");((Int64.of_int 2),"two");((Int64.of_int 3),"three")] (Bimap_test1.bindings ());
      Bimap_test1.add ~key:(Int64.of_int 3) ~data:"tri";
      (* 1 -> one | 2 -> two | 3 -> tri *)
      assert_equal "tri" (Bimap_test1.find_exn ~key:(Int64.of_int 3));
      assert_equal (Int64.of_int 3) (Bimap_test1.find_reverse_exn ~key:"tri");      
      Bimap_test1.add_reverse ~key:"tri" ~data:(Int64.of_int 4);
      (* 1 -> one | 2 -> two | 4 -> tri *)
      assert_equal (Int64.of_int 4) (Bimap_test1.find_reverse_exn ~key:"tri");
      assert_equal "tri" (Bimap_test1.find_exn ~key:(Int64.of_int 4));
      Bimap_test1.filter ~f:(fun x _ -> x <> (Int64.of_int 4));
      (* 1 -> one | 2 -> two *)
      assert_equal 2 (Bimap_test1.cardinal ());
      Bimap_test1.filter_reverse ~f:(fun y _ -> String.compare y "two" <> 0);
      (* 1 -> one *)
      assert_equal 1 (Bimap_test1.cardinal ());
      Bimap_test1.filter ~f:(fun x _ -> x <> (Int64.of_int 3));
      (* 1 -> one*)
      assert_equal [((Int64.of_int 1),"one")] (Bimap_test1.bindings ());
      assert_equal 1 (Bimap_test1.cardinal ());
      Bimap_test1.filter_reverse ~f:(fun y _ -> String.compare y "one" <> 0);
      assert_equal 0 (Bimap_test1.cardinal ());
    end

  let test2 text_ctx =
    let module IntMap = Map.Make(Int64) in
    let module StrMap = Map.Make(String) in
    let module Bimap_test2 = Bimap(IntMap)(StrMap) in
    begin
      Bimap_test2.add ~key:(Int64.of_int 1) ~data:"one";
      Bimap_test2.add ~key:(Int64.of_int 2) ~data:"two";
      (* 1 -> one | 2 -> two *)
      assert_equal 2 (Bimap_test2.cardinal ());
      assert_equal false (Bimap_test2.is_empty ());
      assert_equal (Some "one") (Bimap_test2.find ~key:(Int64.of_int 1));
      assert_equal (Some "two") (Bimap_test2.find ~key:(Int64.of_int 2));
      assert_equal (Some (Int64.of_int 1)) (Bimap_test2.find_reverse ~key:"one");
      assert_equal (Some (Int64.of_int 2)) (Bimap_test2.find_reverse ~key:"two");
      assert_equal None (Bimap_test2.find ~key:(Int64.of_int 3));
      assert_equal None (Bimap_test2.find_reverse ~key:"three");
      Bimap_test2.add_reverse ~key:"three" ~data:(Int64.of_int 3);
      (* 1 -> one | 2 -> two | 3 -> three *)
      assert_equal (Some "three") (Bimap_test2.find ~key:(Int64.of_int 3));
      assert_equal (Some (Int64.of_int 3)) (Bimap_test2.find_reverse ~key:"three");
      Bimap_test2.add ~key:(Int64.of_int 3) ~data:"tri";
      (* 1 -> one | 2 -> two | 3 -> tri *)
      assert_equal (Int64.of_int 3) (Bimap_test2.find_reverse_exn ~key:"tri");
      Bimap_test2.add_reverse ~key:"tri" ~data:(Int64.of_int 4);
      (* 1 -> one | 2 -> two | 4 -> tri *)
      assert_equal (Some (Int64.of_int 4)) (Bimap_test2.find_reverse ~key:"tri");
      assert_equal (Some "tri") (Bimap_test2.find ~key:(Int64.of_int 4));
      assert_equal true (Bimap_test2.mem (Int64.of_int 1));
      assert_equal true (Bimap_test2.mem (Int64.of_int 2));
      assert_equal true (Bimap_test2.mem_reverse "tri");
      assert_equal (Some ((Int64.of_int 1),"one")) (Bimap_test2.min_binding ());
      assert_equal (Some ((Int64.of_int 4),"tri")) (Bimap_test2.max_binding ());
      assert_equal ((Int64.of_int 1),"one") (Bimap_test2.min_binding_exn ());
      assert_equal ((Int64.of_int 4),"tri") (Bimap_test2.max_binding_exn ());
      assert_equal (Some ("one",(Int64.of_int 1))) (Bimap_test2.min_binding_reverse ());
      assert_equal (Some ("two",(Int64.of_int 2))) (Bimap_test2.max_binding_reverse ());
      assert_equal ("one",(Int64.of_int 1)) (Bimap_test2.min_binding_reverse_exn ());
      assert_equal ("two",(Int64.of_int 2)) (Bimap_test2.max_binding_reverse_exn ());
    end

  let test3 text_ctx =
    let module IntMap = Map.Make(Int64) in
    let module StrMap = Map.Make(String) in
    let module Bimap_test3 = Bimap(IntMap)(StrMap) in
    begin
      Bimap_test3.add ~key:(Int64.of_int 1) ~data:"single";
      Bimap_test3.add ~key:(Int64.of_int 2) ~data:"double";
      Bimap_test3.add ~key:(Int64.of_int 3) ~data:"triple";
      assert_equal 3 (Bimap_test3.cardinal ());
      assert_equal [((Int64.of_int 1),"single");((Int64.of_int 2),"double");((Int64.of_int 3),"triple")] (Bimap_test3.bindings ());
      assert_equal true (List.mem ((Int64.of_int 1),"single") (Bimap_test3.bindings ()));
      assert_equal true (List.mem ("double",(Int64.of_int 2)) (Bimap_test3.bindings_reverse ()));
      assert_equal true (List.mem ((Int64.of_int 3),"triple") (Bimap_test3.bindings ()));
      assert_equal true (Bimap_test3.exists ~f:(fun _ y -> (String.compare y "double") = 0));
      assert_equal true (Bimap_test3.exists ~f:(fun _ y -> y = "triple"));
      assert_equal false (Bimap_test3.exists ~f:(fun _ y -> y = "quadruple"));
      assert_equal true (Bimap_test3.exists_reverse ~f:(fun _ y -> y = (Int64.of_int 2)));
      assert_equal false (Bimap_test3.exists_reverse ~f:(fun _ y -> y = (Int64.of_int 4)));
      Bimap_test3.empty ();
      assert_equal 0 (Bimap_test3.cardinal ());
      assert_equal true (Bimap_test3.is_empty ());
      Bimap_test3.add ~key:(Int64.of_int 1) ~data:"one";
      assert_equal "one" (Bimap_test3.find_exn ~key:(Int64.of_int 1));
      Bimap_test3.add ~key:(Int64.of_int 2) ~data:"two";
      Bimap_test3.add ~key:(Int64.of_int 3) ~data:"three";
      Bimap_test3.add ~key:(Int64.of_int 1) ~data:"newentry";
      
      assert_equal false (Bimap_test3.is_empty ());
      assert_equal "newentry" (Bimap_test3.find_exn ~key:(Int64.of_int 1));
      Bimap_test3.remove ~key:(Int64.of_int 1);
      assert_equal 2 (Bimap_test3.cardinal ());
      
      (*print_n_flush "Reverse cardinal is:";
      print_n_flush (string_of_int (Bimap_test3.cardinal_reverse ()));*)
      assert_equal 2 (Bimap_test3.cardinal_reverse ());
      assert_equal false (Bimap_test3.mem ~key:(Int64.of_int 1));
      assert_equal true (Bimap_test3.mem ~key:(Int64.of_int 2));
      assert_equal true (Bimap_test3.mem ~key:(Int64.of_int 3));
      assert_equal false (Bimap_test3.mem_reverse ~key:"newentry");
      assert_equal true (Bimap_test3.mem_reverse ~key:"two");
      assert_equal true (Bimap_test3.mem_reverse ~key:"three");
      assert_equal None (Bimap_test3.find (Int64.of_int 1));
      assert_equal None (Bimap_test3.find_reverse "newentry");

      Bimap_test3.singleton ~key:(Int64.of_int 1) ~data:"uno";
      assert_equal 1 (Bimap_test3.cardinal ());
      assert_equal true (Bimap_test3.mem ~key:(Int64.of_int 1));
      assert_equal (Some "uno") (Bimap_test3.find (Int64.of_int 1));
      assert_equal 1 (Bimap_test3.cardinal_reverse ());
      assert_equal (Some (Int64.of_int 1)) (Bimap_test3.find_reverse ("uno"));
    end 

  let test4 text_ctx =
    let module IntMap = Map.Make(Int64) in
    let module StrMap = Map.Make(String) in
    let module Bimap_test4 = Bimap(IntMap)(StrMap) in
    let module Bimap_test4_other = Bimap(IntMap)(StrMap) in
    begin
      Bimap_test4.add ~key:(Int64.of_int 1) ~data:"single";
      Bimap_test4.add ~key:(Int64.of_int 2) ~data:"double";
      Bimap_test4.add ~key:(Int64.of_int 3) ~data:"triple";
      (*1 -> single | 2 -> double | 3 -> triple *)      
      Bimap_test4_other.add ~key:(Int64.of_int 3) ~data:"tri";
      Bimap_test4_other.add ~key:(Int64.of_int 5) ~data:"pentuple";
      Bimap_test4_other.add ~key:(Int64.of_int 6) ~data:"sextuple";
      (*3 -> tri | 5 -> pentuple | 6 -> sextuple *)
      assert_equal 3 (Bimap_test4.cardinal ());
      assert_equal 3 (Bimap_test4_other.cardinal ());

      assert_equal false (0 = (Bimap_test4.compare ~f:(fun x y -> String.compare x y)
                                 ~othermap:(Bimap_test4_other.get_forward_map ())));
      assert_equal false (0 = (Bimap_test4.compare_reverse ~f:(fun x y -> Int64.compare x y)
                                 ~othermap:(Bimap_test4_other.get_reverse_map ())));

      assert_equal false (Bimap_test4.equal ~f:(fun x y -> (String.compare x y) = 0)
                            ~othermap:(Bimap_test4_other.get_forward_map ()));
      assert_equal false (Bimap_test4.equal_reverse ~f:(fun x y -> (Int64.compare x y) = 0)
                            ~othermap:(Bimap_test4_other.get_reverse_map ()));

      Bimap_test4.merge
        ~f:(fun k v1 v2->
          match v1, v2 with
          | None, None -> None
          | Some v1, None -> Some v1
          | None, Some v2 -> Some v2
          | Some v1, Some v2 -> Some v1
        )
        ~othermap:(Bimap_test4_other.get_forward_map ());
      (*1 -> single | 2 -> double | 3 -> triple | 5 -> pentuple | 6 -> sextuple *)
      assert_equal 5 (Bimap_test4.cardinal ());
      assert_equal 5 (Bimap_test4.cardinal_reverse ());
      assert_equal (Some "triple") (Bimap_test4.find (Int64.of_int 3));

      Bimap_test4_other.merge_reverse
        ~f:(fun k v1 v2->
          match v1, v2 with
          | None, None -> None
          | Some v1, None -> Some v1
          | None, Some v2 -> Some v2
          | Some v1, Some v2 -> Some v1
        )
        ~othermap:(Bimap_test4.get_reverse_map ());
      (*1 <- single | 2 <- double | 3 <- triple | 3 <- tri | 5 <- pentuple | 6 <- sextuple 
        1 -> single | 2 -> double | 3 -> triple | 5 <- pentuple | 6 <- sextuple  *)
      assert_equal 5 (Bimap_test4_other.cardinal ());
      (*6! B/c while adding to forward_map...we only retain the 3 key once, unlike reverse mapping
      assert_equal 6 (Bimap_test4_other.cardinal_reverse ());
      assert_equal (Some (Int64.of_int 3)) (Bimap_test4_other.find_reverse "triple");
      assert_equal (Some (Int64.of_int 3)) (Bimap_test4_other.find_reverse "tri");
      assert_equal (Some "triple") (Bimap_test4_other.find (Int64.of_int 3));
      *)
      assert_equal 5 (Bimap_test4_other.cardinal_reverse ());
      assert_equal (Some (Int64.of_int 3)) (Bimap_test4_other.find_reverse "triple");
      assert_equal None (Bimap_test4_other.find_reverse "tri");
      assert_equal (Some "triple") (Bimap_test4_other.find (Int64.of_int 3));

      (*---good double check of merge and merge_reverse functions as well---*)
      assert_equal true (0 = (Bimap_test4.compare ~f:(fun x y -> String.compare x y) ~othermap:(Bimap_test4_other.get_forward_map ())));
      assert_equal true (0 = (Bimap_test4.compare_reverse ~f:(fun x y -> Int64.compare x y) ~othermap:(Bimap_test4_other.get_reverse_map ())));

      assert_equal true (Bimap_test4.equal ~f:(fun x y -> (String.compare x y) = 0) ~othermap:(Bimap_test4_other.get_forward_map ()));
      assert_equal true (Bimap_test4.equal_reverse ~f:(fun x y -> (Int64.compare x y) = 0) ~othermap:(Bimap_test4_other.get_reverse_map ()));

      assert_equal (Int64.of_int 17) (Bimap_test4.fold_reverse ~f:(fun _k v accum -> Int64.add v accum) Int64.zero);
      assert_equal 34 (Bimap_test4.fold ~f:(fun _k v accum -> (String.length v) + accum) 0);

      assert_equal true (Bimap_test4.for_all ~f:(fun k v -> ((String.length v) >= 6) && ((Int64.compare k Int64.zero) > 0)));
      assert_equal true (Bimap_test4.for_all_reverse ~f:(fun k v -> ((String.length k) >= 6) && ((Int64.compare v Int64.zero) > 0)));

      let m1,m2 = Bimap_test4.partition ~f:(fun k _v -> (Int64.compare k (Int64.of_int 4) < 0)) in 
      let () = (assert_equal true (IntMap.mem (Int64.of_int 1) m1)) in
      let () = (assert_equal true (IntMap.mem (Int64.of_int 2) m1)) in
      let () = (assert_equal true (IntMap.mem (Int64.of_int 3) m1)) in 
      let () = (assert_equal true (IntMap.mem (Int64.of_int 5) m2)) in
      assert_equal true (IntMap.mem (Int64.of_int 6) m2);
      let m3,m4 = Bimap_test4.partition_reverse ~f:(fun k _v -> (String.length k > 6)) in
      let () = (assert_equal true (StrMap.mem "pentuple" m3)) in 
      assert_equal true (StrMap.mem "sextuple" m3);                                                    

      let k,v = Bimap_test4.min_binding_exn () in
      let () = assert_equal k (Int64.of_int 1) in
      assert_equal v "single";
      let k,v = Bimap_test4.max_binding_exn () in
      let () = assert_equal k (Int64.of_int 6) in
      assert_equal v "sextuple";
      
      let r = Bimap_test4.min_binding () in
      (match r with
       | Some (k2,v2) -> 
          let () = assert_equal k2 (Int64.of_int 1) in
          assert_equal v2 "single";
       | None -> assert_equal true false);
      
      let r = Bimap_test4.max_binding () in
      (match r with
       | Some (k3,v3) -> 
          let () = assert_equal k3 (Int64.of_int 6) in
          assert_equal v3 "sextuple"
       | None -> assert_equal true false
      );

      let k,v = Bimap_test4.min_binding_reverse_exn () in
      let () = assert_equal k "double" in
      assert_equal v (Int64.of_int 2);
      let k,v = Bimap_test4.max_binding_reverse_exn () in
      let () = assert_equal k "triple" in
      assert_equal v (Int64.of_int 3);

      let l,vopt,r = Bimap_test4.split ~key:(Int64.of_int 3) in
      let () = assert_equal vopt (Some ("triple")) in
      let () = assert_equal 2 (IntMap.cardinal l) in
      assert_equal 2 (IntMap.cardinal r);

      (*--mutate the map--check the reverse bindings!*)
      let () = Bimap_test4.map ~f:(fun v -> String.concat "+" [v;v]) in
      let () = assert_equal 5 (Bimap_test4.cardinal ()) in
      let () = assert_equal 5 (Bimap_test4.cardinal_reverse ()) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 1)) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 2)) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 3)) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 5)) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 6)) in
      let () = assert_equal (Some "triple+triple") (Bimap_test4.find (Int64.of_int 3)) in
      let () = assert_equal (Some "double+double") (Bimap_test4.find (Int64.of_int 2)) in
      let () = assert_equal (Some "single+single") (Bimap_test4.find (Int64.of_int 1)) in
      let () = assert_equal (Some "pentuple+pentuple") (Bimap_test4.find (Int64.of_int 5)) in
      let () = assert_equal (Some "sextuple+sextuple") (Bimap_test4.find (Int64.of_int 6)) in
      let () = assert_equal true (Bimap_test4.mem_reverse "double+double") in
      let () = assert_equal true (Bimap_test4.mem_reverse "triple+triple") in
      let () = assert_equal true (Bimap_test4.mem_reverse "pentuple+pentuple") in
      let () = assert_equal true (Bimap_test4.mem_reverse "sextuple+sextuple") in
      let () = assert_equal (Some (Int64.of_int 3)) (Bimap_test4.find_reverse "triple+triple") in
      let () = assert_equal (Some (Int64.of_int 2)) (Bimap_test4.find_reverse "double+double") in
      let () = assert_equal (Some (Int64.of_int 1)) (Bimap_test4.find_reverse "single+single") in
      let () = assert_equal (Some (Int64.of_int 5)) (Bimap_test4.find_reverse "pentuple+pentuple") in
      assert_equal (Some (Int64.of_int 6)) (Bimap_test4.find_reverse "sextuple+sextuple");

      let () = Bimap_test4.map_reverse ~f:(fun x -> (Int64.mul x (Int64.of_int 2))) in
      let () = assert_equal 5 (Bimap_test4.cardinal ()) in
      let () = assert_equal 5 (Bimap_test4.cardinal_reverse ()) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 2)) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 4)) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 6)) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 10)) in
      let () = assert_equal true (Bimap_test4.mem (Int64.of_int 12)) in
      let () = assert_equal (Some "triple+triple") (Bimap_test4.find (Int64.of_int 6)) in
      let () = assert_equal (Some "double+double") (Bimap_test4.find (Int64.of_int 4)) in
      let () = assert_equal (Some "single+single") (Bimap_test4.find (Int64.of_int 2)) in
      let () = assert_equal (Some "pentuple+pentuple") (Bimap_test4.find (Int64.of_int 10)) in
      let () = assert_equal (Some "sextuple+sextuple") (Bimap_test4.find (Int64.of_int 12)) in
      let () = assert_equal true (Bimap_test4.mem_reverse "double+double") in
      let () = assert_equal true (Bimap_test4.mem_reverse "triple+triple") in
      let () = assert_equal true (Bimap_test4.mem_reverse "pentuple+pentuple") in
      let () = assert_equal true (Bimap_test4.mem_reverse "sextuple+sextuple") in
      let () = assert_equal (Some (Int64.of_int 6)) (Bimap_test4.find_reverse "triple+triple") in
      let () = assert_equal (Some (Int64.of_int 4)) (Bimap_test4.find_reverse "double+double") in
      let () = assert_equal (Some (Int64.of_int 2)) (Bimap_test4.find_reverse "single+single") in
      let () = assert_equal (Some (Int64.of_int 10)) (Bimap_test4.find_reverse "pentuple+pentuple") in
      assert_equal (Some (Int64.of_int 12)) (Bimap_test4.find_reverse "sextuple+sextuple");
    end

  let test5 text_ctx =
    let module IntMap = Map.Make(Int64) in
    let module StrMap = Map.Make(String) in
    let module Bimap_test5 = Bimap_multi(IntMap)(StrMap) in
    let module Bimap_test5_clone = Bimap_multi(IntMap)(StrMap) in
    begin
      Bimap_test5.add ~key:(Int64.of_int 1) ~data:"one";
      Bimap_test5.add ~key:(Int64.of_int 2) ~data:"two";
      Bimap_test5.add ~key:(Int64.of_int 3) ~data:"three";
      Bimap_test5.add ~key:(Int64.of_int 2) ~data:"dos";
      Bimap_test5.add ~key:(Int64.of_int 3) ~data:"tres";
      Bimap_test5.add ~key:(Int64.of_int 3) ~data:"triple";
      Bimap_test5.add_reverse ~key:"tri" ~data:(Int64.of_int 3);
      Bimap_test5.add_reverse ~key:"four" ~data:(Int64.of_int 4);

      assert_equal 4 (Bimap_test5.cardinal ());

      assert_equal 8 (Bimap_test5.cardinal_reverse ());
      assert_equal 1 (List.length (Bimap_test5.find_exn (Int64.of_int 1)));
      assert_equal 2 (List.length (Bimap_test5.find_exn (Int64.of_int 2)));
      assert_equal 4 (List.length (Bimap_test5.find_exn (Int64.of_int 3)));
      assert_equal 1 (List.length (Bimap_test5.find_exn (Int64.of_int 4)));

      assert_equal true (List.mem "one" (Bimap_test5.find_exn (Int64.of_int 1)));
      assert_equal true (List.mem "two" (Bimap_test5.find_exn (Int64.of_int 2)));
      assert_equal true (List.mem "dos" (Bimap_test5.find_exn (Int64.of_int 2)));
      assert_equal true (List.mem "three" (Bimap_test5.find_exn (Int64.of_int 3)));
      assert_equal true (List.mem "tres" (Bimap_test5.find_exn (Int64.of_int 3)));
      assert_equal true (List.mem "triple" (Bimap_test5.find_exn (Int64.of_int 3)));
      assert_equal true (List.mem "tri" (Bimap_test5.find_exn (Int64.of_int 3)));

      assert_equal true (List.mem (Int64.of_int 1) (Bimap_test5.find_reverse_exn "one"));
      assert_equal true (List.mem (Int64.of_int 2) (Bimap_test5.find_reverse_exn "two"));
      assert_equal true (List.mem (Int64.of_int 2) (Bimap_test5.find_reverse_exn "dos"));
      assert_equal true (List.mem (Int64.of_int 3) (Bimap_test5.find_reverse_exn "three"));
      assert_equal true (List.mem (Int64.of_int 3) (Bimap_test5.find_reverse_exn "tres"));
      assert_equal true (List.mem (Int64.of_int 3) (Bimap_test5.find_reverse_exn "triple"));
      assert_equal true (List.mem (Int64.of_int 3) (Bimap_test5.find_reverse_exn "tri"));

      assert_equal true ( (fun lo -> match lo with
                                     | Some l ->  List.mem (Int64.of_int 1) l
                                     | None -> false
                          ) (Bimap_test5.find_reverse "one"));
      assert_equal true ( (fun lo -> match lo with
                                     | Some l ->  List.mem (Int64.of_int 2) l
                                     | None -> false
                          ) (Bimap_test5.find_reverse "two"));
      assert_equal true ( (fun lo -> match lo with
                                     | Some l ->  List.mem (Int64.of_int 2) l
                                     | None -> false
                          ) (Bimap_test5.find_reverse "dos"));
      assert_equal true ( (fun lo -> match lo with
                                     | Some l ->  List.mem (Int64.of_int 3) l
                                     | None -> false
                          ) (Bimap_test5.find_reverse "three"));
                          
      assert_equal true ( (fun lo -> match lo with
                                     | Some l ->  List.mem (Int64.of_int 3) l
                                     | None -> false
                          ) (Bimap_test5.find_reverse "tres"));
      assert_equal true ( (fun lo -> match lo with
                                     | Some l ->  List.mem (Int64.of_int 3) l
                                     | None -> false
                          ) (Bimap_test5.find_reverse "triple"));
      assert_equal true ( (fun lo -> match lo with
                                     | Some l ->  List.mem (Int64.of_int 3) l
                                     | None -> false
                          ) (Bimap_test5.find_reverse "tri"));

      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);*)
      assert_equal ([ ((Int64.of_int 1),["one"]) ;
                      (Int64.of_int 2, ["dos";"two"]) ;
                      ((Int64.of_int 3), ["tri";"triple";"tres";"three"]) ;
                      ((Int64.of_int 4), ["four"]) ])
        (Bimap_test5.bindings ());
      (*print_n_flush_alist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_inverse);*)
      Bimap_test5_clone.set_forward_map ~map:(Bimap_test5.get_forward_map ());
      Bimap_test5_clone.filter
        ~f:(fun x _v -> (Int64.compare (Int64.of_int 1) x) = 0);
      assert_equal 1 (List.length (Bimap_test5_clone.bindings ()));

      Bimap_test5_clone.set_forward_map ~map:(Bimap_test5.get_forward_map ());
      Bimap_test5_clone.filter
        ~f:(fun x _v -> (Int64.compare (Int64.of_int 2) x) = 0);
      assert_equal 1 (List.length (Bimap_test5_clone.bindings ()));
      assert_equal 2 (List.length (snd (List.hd (Bimap_test5_clone.bindings ()))));
      assert_equal 2 (List.length (Bimap_test5_clone.bindings_reverse ()));
      assert_equal 1 (List.length (snd (List.hd (Bimap_test5_clone.bindings_reverse ()))));

      Bimap_test5_clone.set_forward_map ~map:(Bimap_test5.get_forward_map ());
      Bimap_test5_clone.filter
        ~f:(fun x _v -> (Int64.compare (Int64.of_int 3) x) = 0);
      assert_equal 1 (List.length (Bimap_test5_clone.bindings ()));
      assert_equal 4 (List.length (snd (List.hd (Bimap_test5_clone.bindings ()))));
      assert_equal 4 (List.length (Bimap_test5_clone.bindings_reverse ()));
      assert_equal 1 (List.length (snd (List.hd (Bimap_test5_clone.bindings_reverse ()))));

      Bimap_test5_clone.set_forward_map ~map:(Bimap_test5.get_forward_map ());
      Bimap_test5_clone.filter
        ~f:(fun x _v -> (Int64.compare (Int64.of_int 4) x) = 0);
      assert_equal 1 (List.length (Bimap_test5_clone.bindings ()));

      assert_equal true (Bimap_test5.exists
                           ~f:(fun _x vs ->
                             List.length vs = 1 && (String.compare (List.nth vs 0) "one") = 0));
      assert_equal true (Bimap_test5.exists_reverse
                           ~f:(fun _x vs ->
                             List.exists (fun v -> (Int64.compare v (Int64.of_int 2)) = 0) vs));
    end 
(*
  let test7 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter ~f:(fun v -> (List.length v) < 3);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 2 (bim#length);

      bim#filter_inverse ~f:(fun k -> k < 2);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);
    end

  let test8 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filteri ~f:(fun ~key ~data -> key < 3 && (List.length data) > 1);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 2 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filteri_inverse ~f:(fun ~key ~data -> key.[0] = 't' && data < 3);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_keys ~f:(fun k -> k > 2);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_keys_inverse ~f:(fun k -> (String.length k) > 3);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_map ~f:(fun vlist -> if List.length vlist < 3 then Some vlist else None);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 2 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_map_inverse ~f:(fun invk -> if invk < 3 then Some (invk+1) else None);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 2 (bim#length);
      assert_equal 1 (List.length (bim#find_exn ~key:2));
      assert_equal 2 (List.length (bim#find_exn ~key:3));
      assert_equal true (List.mem "one" (bim#find_exn ~key:2));
      assert_equal true (List.mem "two" (bim#find_exn ~key:3)); 
      assert_equal true (List.mem "dos" (bim#find_exn ~key:3));
      assert_equal 2 (bim#find_exn_inverse ~key:"one");
      assert_equal 3 (bim#find_exn_inverse ~key:"two");
      assert_equal 3 (bim#find_exn_inverse ~key:"dos");
      assert_equal None (bim#find ~key:1);
    end 


  let test9 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      let total_chars =
	bim#fold ~init:0 ~f:(fun ~key ~data accum ->
			     (Core.List.fold data ~init:accum
					     ~f:(fun accum data -> (String.length data) + accum))) in
      assert_equal 24 total_chars;
      let keys_times_num_values =
	bim#fold_inverse ~init:0 ~f:(fun ~key ~data accum ->
				     accum + data) in
      assert_equal 14 keys_times_num_values;
      (*fold over keys in decreasing order instead of increasing--in this case reverse alphabetical order*)
      let folded_right =
	bim#fold_right ~init:None
		       ~f:(fun ~key ~data accum ->
			   match accum with
			   | None -> Some key
			   | Some i -> Some (i - key)
			  ) in
      assert_equal (Some 0) folded_right;
      let folded_right_inverse = bim#fold_right_inverse
				       ~init:[]
				       ~f:(fun ~key ~data accum ->
					   key::accum
					  ) in
      (*print_n_flush_alist ~sep:"|" ~to_string_func:(fun x -> x) folded_right_inverse;*)
      assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_inverse) 0);
      let forall = bim#for_all
			 ~f:(fun l ->
			     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 3))
			    ) in
      assert_equal forall false;
      assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_inverse) 0);
      let forall = bim#for_all
			 ~f:(fun l ->
			     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 2))
			    ) in
      assert_equal forall true;
      let forallinverse = bim#for_all_inverse ~f:(fun x -> x < 4) in
      assert_equal forallinverse true;
      let forallinverse = bim#for_all_inverse ~f:(fun x -> x < 3) in
      assert_equal forallinverse false;
      assert_equal false (bim#is_empty);
      assert_equal [1;2;3] (bim#keys);
      (*let () print_n_flush_alist ~sep:"," ~to_string_func:(fun x -> x) inverse_keys in *)
      assert_equal true (List.mem "dos" (bim#keys_inverse));
      assert_equal 1 (List.length (List.filter (fun x -> String.equal x "dos") (bim#keys_inverse)));
      assert_equal true (List.mem "one" (bim#keys_inverse));
      assert_equal true (List.mem "three" (bim#keys_inverse));
      assert_equal true (List.mem "tres" (bim#keys_inverse));
      assert_equal true (List.mem "triple" (bim#keys_inverse));
      assert_equal true (List.mem "two" (bim#keys_inverse));

      assert_equal 3 (bim#length);

      bim#map ~f:(fun l -> let len = List.length l in
			   match len with
			   | 1 -> ["1"]@l
			   | 2 -> ["2"]@l
			   | 3 -> ["3"]@l
			   | _ -> ["unexpectedlen"]@l);
      assert_equal 2 (List.length (bim#find_exn 1));
      assert_equal true (List.mem "1" (bim#find_exn 1));
      assert_equal 3 (List.length (bim#find_exn 2));
      assert_equal true (List.mem "2" (bim#find_exn 2));
      assert_equal 4 (List.length (bim#find_exn 3));
      assert_equal true (List.mem "3" (bim#find_exn 3));
      assert_equal 1 (bim#find_exn_inverse "1");
      assert_equal 2 (bim#find_exn_inverse "2");
      assert_equal 3 (bim#find_exn_inverse "3");
      assert_equal 1 (bim#find_exn_inverse "one");
      assert_equal 2 (bim#find_exn_inverse "two");
      assert_equal 3 (bim#find_exn_inverse "three");
    end

  let test10 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap_multi.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      assert_equal true (bim#mem 1);
      assert_equal true (bim#mem 2);
      assert_equal true (bim#mem 3);
      assert_equal true (bim#mem_inverse "one");
      assert_equal true (bim#mem_inverse "two");
      assert_equal true (bim#mem_inverse "three");
      
      bim#map_inverse ~f:(fun x -> x *10);
      assert_equal 10 (bim#find_exn_inverse "one");
      assert_equal 20 (bim#find_exn_inverse "two");
      assert_equal 30 (bim#find_exn_inverse "three");
      assert_equal None (bim#find 1);
      assert_equal None (bim#find 2);
      assert_equal None (bim#find 3);
      assert_equal true (List.mem "one" (bim#find_exn 10));
      assert_equal true (List.mem "two" (bim#find_exn 20));
      assert_equal true (List.mem "three" (bim#find_exn 30));

      assert_equal (Some (10, ["one"])) (bim#min_elt);
      assert_equal (10, ["one"]) (bim#min_elt_exn);
      assert_equal (Some (30, ["triple";"tres";"three"])) (bim#max_elt);
      assert_equal (30,["triple";"tres";"three"]) (bim#max_elt_exn);

      assert_equal (Some ("dos", 20)) (bim#min_elt_inverse);
      assert_equal ("dos", 20) (bim#min_elt_exn_inverse);
      assert_equal (Some ("two", 20)) (bim#max_elt_inverse);
      assert_equal ("two",20) (bim#max_elt_exn_inverse);

      assert_equal (Some (10, ["one"])) (bim#nth 0);
      assert_equal (Some (20, ["two";"dos"])) (bim#nth 1);
      assert_equal (Some (30, ["triple";"tres";"three"])) (bim#nth 2);
      assert_equal (Some ("dos",20)) (bim#nth_inverse 0);
      assert_equal (Some ("one",10)) (bim#nth_inverse 1);
      assert_equal (Some ("three",30)) (bim#nth_inverse 2);
      assert_equal (Some ("tres",30)) (bim#nth_inverse 3);
      assert_equal (Some ("triple",30)) (bim#nth_inverse 4);
      assert_equal (Some ("two",20)) (bim#nth_inverse 5);

      bim#remove ~key:20;
      assert_equal None (bim#find 20);
      assert_equal None (bim#find_inverse "two");

      bim#remove_inverse ~key:"triple";
      assert_equal false (List.mem "triple" (bim#find_exn 30));
      assert_equal true (List.mem "three" (bim#find_exn 30));
      assert_equal true (List.mem "tres" (bim#find_exn 30));

      bim#remove_multi ~key:30;
      assert_equal false (List.mem "triple" (bim#find_exn 30));
      assert_equal false (List.mem "tres" (bim#find_exn 30));
      assert_equal true (List.mem "three" (bim#find_exn 30));
      assert_equal 1 (List.length (bim#find_exn 30));
      assert_equal None (bim#find_inverse "tres");
      assert_equal (Some 30) (bim#find_inverse "three");

      bim#update 30 ~f:(fun l ->
			match l with
			| Some elems ->
			   let elems2 = if not (List.mem "three" elems) then
					  (*let () = print_n_flush "\nAdding three..." in *)
					  ("three" :: elems)
					else elems in
			   let elems3 = if not (List.mem "tres" elems2) then
					  (*let () = print_n_flush "\nAdding tres..." in *)
					  ("tres" :: elems2)
					else elems2 in
			   let elems4 = if not (List.mem "triple" elems3) then
					  (*let () = print_n_flush "\nAdding triple..." in*)
					  ("triple" :: elems3)
					else elems3 in
			   elems4
			| None -> ["three";"tres";"triple"]
		       );
      assert_equal true (List.mem "triple" (bim#find_exn 30));
      assert_equal true (List.mem "tres" (bim#find_exn 30));
      assert_equal true (List.mem "three" (bim#find_exn 30));
      assert_equal (Some 30) (bim#find_inverse "tres");
      assert_equal (Some 30) (bim#find_inverse "triple");
      assert_equal (Some 30) (bim#find_inverse "three");
    end 
 *)
  let suite =
    "suite">:::
      ["test1">:: test1;
       "test2">:: test2;
       "test3">:: test3;
       "test4">:: test4;
       "test5">:: test5;
       (*"test6">:: test6;
       "test7">:: test7;
       "test8">:: test8;
       "test9">:: test9;
       "test10">:: test10*)
];;

  let () =
    run_test_tt_main suite
	
end 
