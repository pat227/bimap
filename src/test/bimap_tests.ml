module Bimap = Bimap.Bimap
open OUnit2
module Bimap_tests = struct

  let test1 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"one";
      bim#add ~key:2 ~data:"two";
      assert_equal false (bim#is_empty);
      assert_equal "one" (bim#find_exn ~key:1);
      assert_equal "two" (bim#find_exn ~key:2);
      assert_equal 1 (bim#find_exn_inverse ~key:"one");
      assert_equal 2 (bim#find_exn_inverse ~key:"two");
      bim#add_inverse ~key:"three" ~data:3;
      assert_equal "three" (bim#find_exn ~key:3);
      assert_equal 3 (bim#find_exn_inverse ~key:"three");
      bim#change ~key:3 ~f:(fun x -> (Some "tri"));
      assert_equal "tri" (bim#find_exn ~key:3);
      assert_equal 3 (bim#find_exn_inverse ~key:"tri");      
      bim#change_inverse ~key:"tri" ~f:(fun x -> (Some 4));
      assert_equal 4 (bim#find_exn_inverse ~key:"tri");
      assert_equal "tri" (bim#find_exn ~key:4);
    end

  let test2 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"one";
      bim#add ~key:2 ~data:"two";
      assert_equal false (bim#is_empty);
      assert_equal (Some "one") (bim#find ~key:1);
      assert_equal (Some "two") (bim#find ~key:2);
      assert_equal (Some 1) (bim#find_inverse ~key:"one");
      assert_equal (Some 2) (bim#find_inverse ~key:"two");
      assert_equal None (bim#find ~key:3);
      assert_equal None (bim#find_inverse ~key:"three");
      bim#add_inverse ~key:"three" ~data:3;
      assert_equal (Some "three") (bim#find ~key:3);
      assert_equal (Some 3) (bim#find_inverse ~key:"three");
      bim#change ~key:3 ~f:(fun x -> (Some "tri"));
      assert_equal 3 (bim#find_exn_inverse ~key:"tri");
      bim#change_inverse ~key:"tri" ~f:(fun x -> (Some 4));
      assert_equal (Some 4) (bim#find_inverse ~key:"tri");
      assert_equal (Some "tri") (bim#find ~key:4);
    end

  let test3 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"single";
      bim#add ~key:2 ~data:"double";
      bim#add ~key:3 ~data:"triple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      assert_equal 3 (bim#count_inverse ~f:(fun x -> true));
      assert_equal 2 (bim#counti ~f:(fun ~key ~data -> if key <= 2 then true else false));
      assert_equal ["single";"double";"triple"] (bim#data);
      assert_equal true (List.mem 1 (bim#data_inverse));
      assert_equal true (List.mem 2 (bim#data_inverse));
      assert_equal true (List.mem 3 (bim#data_inverse));
      assert_equal true (bim#exists ~f:(fun x -> x = "double"));
      assert_equal true (bim#exists ~f:(fun x -> x = "triple"));
      assert_equal false (bim#exists ~f:(fun x -> x = "quadruple"));
      assert_equal true (bim#exists_inverse ~f:(fun x -> x = 2));
      assert_equal false (bim#exists_inverse ~f:(fun x -> x = 4));
      assert_equal true (bim#existsi ~f:(fun ~key ~data -> key = 3 && data = "triple"));
      assert_equal true (bim#existsi_inverse ~f:(fun ~key ~data -> key = "double" && data = 2));
      bim#empty ();
      assert_equal 0 (bim#count ~f:(fun x -> true));
      assert_equal true (bim#is_empty);
    end 

  let test3 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filter ~f:(fun v -> String.length v > 8);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));

      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filter_inverse ~f:(fun v -> v > 4);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      bim#add ~key:4 ~data:"quadruple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filter_keys ~f:(fun k -> k < 5);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filter_keys_inverse ~f:(fun v -> String.length v > 8);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));

      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun x -> true));
      bim#filteri ~f:(fun ~key ~data -> String.length data > 8 || key <6);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal (Some "quadruple") (bim#find ~key:4);
      assert_equal (Some "pentuple") (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      
      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      bim#filteri_inverse ~f:(fun ~key ~data -> String.length key > 8 || data <6);
      assert_equal 2 (bim#count ~f:(fun x -> true));
      assert_equal 2 (bim#count_inverse ~f:(fun x -> true));
      assert_equal (Some "quadruple") (bim#find ~key:4);
      assert_equal (Some "pentuple") (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some 4) (bim#find_inverse ~key:"quadruple");
      assert_equal (Some 5) (bim#find_inverse ~key:"pentuple");
      assert_equal None (bim#find_inverse ~key:"sextuple");

      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      bim#filter_map ~f:(fun v -> if String.length v > 8 then (Some "survivor") else None);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));
      assert_equal (Some "survivor") (bim#find ~key:4);
      assert_equal None (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some 4) (bim#find_inverse ~key:"survivor");
      assert_equal None (bim#find_inverse ~key:"pentuple");
      assert_equal None (bim#find_inverse ~key:"sextuple");

      bim#add ~key:4 ~data:"quadruple";
      bim#add ~key:5 ~data:"pentuple";
      bim#add ~key:6 ~data:"sextuple";
      bim#filter_map_inverse ~f:(fun v -> if v > 5 then (Some (v * 2)) else None);
      assert_equal 1 (bim#count ~f:(fun x -> true));
      assert_equal 1 (bim#count_inverse ~f:(fun x -> true));
      assert_equal (Some 12) (bim#find_inverse ~key:"sextuple");
      assert_equal None (bim#find_inverse ~key:"quadruple");
      assert_equal None (bim#find_inverse ~key:"pentuple");
      assert_equal None (bim#find ~key:4);
      assert_equal None (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some "sextuple") (bim#find ~key:12);      
    end

  let test4 text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in 
    let bim = new Bimap.bimap_class int_map string_map in
    begin
      bim#add ~key:1 ~data:"single";
      bim#add ~key:2 ~data:"double";
      bim#add ~key:3 ~data:"triple";
      let concat = bim#fold ~init:"" ~f:(fun ~key ~data init -> if key < 3 then (init ^ data) else init) in
      assert_equal "singledouble" concat;
      let sum = bim#fold_inverse
		      ~init:0
		      ~f:(fun ~key ~data init ->
			  if key.[0] = 't' || key.[0] = 'd'
			  then (data + init) else init) in
      assert_equal 5 sum;
      let dividend = bim#fold_inverse
		       ~init:1
		       ~f:(fun ~key ~data init ->
			   if key.[0] = 't' || key.[0] = 'd'
			   then (data / init) else init) in
      assert_equal 1 dividend;
      let concat2 =
	bim#fold_right
	      ~init:"" ~f:(fun ~key ~data init ->
			   if key < 3 then (init ^ data) else init) in
      assert_equal "doublesingle" concat2;
      let dividend2 =
	bim#fold_right_inverse
			 ~init:1 ~f:(fun ~key ~data init ->
				     if key.[0] ='t' || key.[0]='d'
				     then (data / init) else init) in
      assert_equal 0 dividend2;
      assert_equal true (bim#for_all ~f:(fun v -> if (String.length v) > 0 then true else false));
    end
      
  let suite =
    "suite">:::
      ["test1">:: test1;
       "test2">:: test2;
       "test3">:: test3;
       "test4">:: test4];;

  let () =
    run_test_tt_main suite
	
end 
