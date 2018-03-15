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
      assert_equal (Some "one") (bim#find ~key:1);
      assert_equal (Some "two") (bim#find ~key:2);
      assert_equal (Some 1) (bim#find_inverse ~key:"one");
      assert_equal (Some 2) (bim#find_inverse ~key:"two");
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
      assert_equal [1;2;3] (bim#data_inverse);
      assert_equal true (bim#exists ~f:(fun x -> x = "double"));
      assert_equal true (bim#exists ~f:(fun ~key ~data -> key = 2 && data = "double"));
      assert_equal false (bim#exists ~f:(fun ~key ~data -> key = 2 && data = "triple"));
      bim#empty ();
      assert_equal 0 (bim#count ~f:(fun x -> true));      
    end 
      
  let suite =
    "suite">:::
      ["test1">:: test1;
       "test2">:: test2;
       "test3">:: test3];;

  let () =
    run_test_tt_main suite
	
end 
