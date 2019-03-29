(*===Tests 1 - 5 target the bimap implemented as a class with single values per key.
     Tests 6 - 10 target the bimap implemented as a class with multiple values per key.
     Tests 11 -15 target the bimap implemented as a module with single values per key.
     Tests 16  20 target the bimap implemented as a module with multiple values per key.
*)
module Bimap=Bimap
open OUnit2
module Bimap_tests = struct

  let oc = Core.Out_channel.stdout;;    
  let print_n_flush s =
    Core.Out_channel.output_string oc s;
    Core.Out_channel.flush oc;;
  
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
  let test1 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single = Bimap.Bimap_single(Core.Int)(Core.String) in
    let bim = new Bimap_single.bimap_class int_map string_map in
    begin
      bim#set ~key:1 ~data:"one";
      bim#set ~key:2 ~data:"two";
      assert_equal 2 (bim#length);
      assert_equal false (bim#is_empty);
      assert_equal "one" (bim#find_exn ~key:1);
      assert_equal "two" (bim#find_exn ~key:2);
      assert_equal 1 (bim#find_exn_reverse ~key:"one");
      assert_equal 2 (bim#find_exn_reverse ~key:"two");

      bim#set_reverse ~key:"three" ~data:3;

      assert_equal "three" (bim#find_exn ~key:3);
      assert_equal 3 (bim#find_exn_reverse ~key:"three");
      assert_equal [(1,"one");(2,"two");(3,"three")] (bim#to_alist ());
      assert_equal [(3,"three");(2,"two");(1,"one")] (bim#to_alist ~key_order:`Decreasing ());
      
      bim#change ~key:3 ~f:(fun _x -> (Some "tri"));

      assert_equal "tri" (bim#find_exn ~key:3);
      assert_equal 3 (bim#find_exn_reverse ~key:"tri");      
      bim#change_reverse ~key:"tri" ~f:(fun _x -> (Some 4));
      assert_equal 4 (bim#find_exn_reverse ~key:"tri");
      assert_equal "tri" (bim#find_exn ~key:4);
      bim#remove ~key:1;
      assert_equal 2 (bim#length);
      bim#remove_reverse ~key:"two";
      assert_equal 1 (bim#length);
      bim#remove ~key:4;
      assert_equal 0 (bim#length);     
    end

  let test2 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single = Bimap.Bimap_single(Core.Int)(Core.String) in
    let bim = new Bimap_single.bimap_class int_map string_map in 
    begin
      bim#set ~key:1 ~data:"one";
      bim#set ~key:2 ~data:"two";
      assert_equal 2 (bim#length);
      assert_equal false (bim#is_empty);
      assert_equal (Some "one") (bim#find ~key:1);
      assert_equal (Some "two") (bim#find ~key:2);
      assert_equal (Some 1) (bim#find_reverse ~key:"one");
      assert_equal (Some 2) (bim#find_reverse ~key:"two");
      assert_equal None (bim#find ~key:3);
      assert_equal None (bim#find_reverse ~key:"three");
      bim#set_reverse ~key:"three" ~data:3;
      assert_equal (Some "three") (bim#find ~key:3);
      assert_equal (Some 3) (bim#find_reverse ~key:"three");

      bim#change ~key:3 ~f:(fun _x -> (Some "tri"));

      assert_equal 3 (bim#find_exn_reverse ~key:"tri");

      bim#change_reverse ~key:"tri" ~f:(fun _x -> (Some 4));

      assert_equal (Some 4) (bim#find_reverse ~key:"tri");
      assert_equal (Some "tri") (bim#find ~key:4);
      assert_equal true (bim#mem 1);
      assert_equal true (bim#mem 2);
      assert_equal true (bim#mem_reverse "tri");
      assert_equal (Some (1,"one")) (bim#min_elt);
      assert_equal (Some (4,"tri")) (bim#max_elt);
      assert_equal (1,"one") (bim#min_elt_exn);
      assert_equal (4,"tri") (bim#max_elt_exn);
      assert_equal (Some ("one",1)) (bim#min_elt_reverse);
      assert_equal (Some ("two",2)) (bim#max_elt_reverse);
      assert_equal ("one",1) (bim#min_elt_exn_reverse);
      assert_equal ("two",2) (bim#max_elt_exn_reverse);
      assert_equal (Some (2,"two")) (bim#nth 1);
      assert_equal (Some (4,"tri")) (bim#nth 2);
      assert_equal (Some ("one",1)) (bim#nth_reverse 0);
      assert_equal (Some ("tri",4)) (bim#nth_reverse 1);
      assert_equal (Some ("two",2)) (bim#nth_reverse 2);
      assert_equal None (bim#nth_reverse 3);
    end

  let test3 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single = Bimap.Bimap_single(Core.Int)(Core.String) in
    let bim = new Bimap_single.bimap_class int_map string_map in   
    begin
      bim#set ~key:1 ~data:"single";
      bim#set ~key:2 ~data:"double";
      bim#set ~key:3 ~data:"triple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#length);
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 2 (bim#counti ~f:(fun ~key ~data:_data -> if key <= 2 then true else false));
      assert_equal ["single";"double";"triple"] (bim#data);
      assert_equal true (List.mem 1 (bim#data_reverse));
      assert_equal true (List.mem 2 (bim#data_reverse));
      assert_equal true (List.mem 3 (bim#data_reverse));
      assert_equal true (bim#exists ~f:(fun x -> x = "double"));
      assert_equal true (bim#exists ~f:(fun x -> x = "triple"));
      assert_equal false (bim#exists ~f:(fun x -> x = "quadruple"));
      assert_equal true (bim#exists_reverse ~f:(fun x -> x = 2));
      assert_equal false (bim#exists_reverse ~f:(fun x -> x = 4));
      assert_equal true (bim#existsi ~f:(fun ~key ~data -> key = 3 && data = "triple"));
      assert_equal true (bim#existsi_reverse ~f:(fun ~key ~data -> key = "double" && data = 2));
      bim#empty ();
      assert_equal 0 (bim#length);
      assert_equal 0 (bim#count ~f:(fun _x -> true));
      assert_equal true (bim#is_empty);

      bim#set ~key:1 ~data:"one";
      bim#set ~key:2 ~data:"two";
      bim#set ~key:3 ~data:"three";
      bim#update ~key:1 ~f:(fun x -> match x with
				  Some s -> Core.String.rev s
				 | None -> "newentry");
      assert_equal "eno" (bim#find_exn ~key:1);
    end 

  let test3b _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single = Bimap.Bimap_single(Core.Int)(Core.String) in
    let bim = new Bimap_single.bimap_class int_map string_map in
    begin
      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#length);
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      bim#filter ~f:(fun v -> String.length v > 8);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));

      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      bim#filter_reverse ~f:(fun v -> v > 4);
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      bim#set ~key:4 ~data:"quadruple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      bim#filter_keys ~f:(fun k -> k < 5);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      bim#filter_keys_reverse ~f:(fun v -> String.length v > 8);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));

      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      assert_equal 3 (bim#count ~f:(fun _x -> true));
      bim#filteri ~f:(fun ~key ~data -> String.length data > 8 || key <6);
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal (Some "quadruple") (bim#find ~key:4);
      assert_equal (Some "pentuple") (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      
      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      bim#filteri_reverse ~f:(fun ~key ~data -> String.length key > 8 || data <6);
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 2 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal (Some "quadruple") (bim#find ~key:4);
      assert_equal (Some "pentuple") (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some 4) (bim#find_reverse ~key:"quadruple");
      assert_equal (Some 5) (bim#find_reverse ~key:"pentuple");
      assert_equal None (bim#find_reverse ~key:"sextuple");

      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      bim#filter_map ~f:(fun v -> if String.length v > 8 then (Some "survivor") else None);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal (Some "survivor") (bim#find ~key:4);
      assert_equal None (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some 4) (bim#find_reverse ~key:"survivor");
      assert_equal None (bim#find_reverse ~key:"pentuple");
      assert_equal None (bim#find_reverse ~key:"sextuple");

      bim#set ~key:4 ~data:"quadruple";
      bim#set ~key:5 ~data:"pentuple";
      bim#set ~key:6 ~data:"sextuple";
      bim#filter_map_reverse ~f:(fun v -> if v > 5 then (Some (v * 2)) else None);
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal (Some 12) (bim#find_reverse ~key:"sextuple");
      assert_equal None (bim#find_reverse ~key:"quadruple");
      assert_equal None (bim#find_reverse ~key:"pentuple");
      assert_equal None (bim#find ~key:4);
      assert_equal None (bim#find ~key:5);
      assert_equal None (bim#find ~key:6);
      assert_equal (Some "sextuple") (bim#find ~key:12);      
    end

  let test4 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single = Bimap.Bimap_single(Core.Int)(Core.String) in
    let bim = new Bimap_single.bimap_class int_map string_map in
    begin
      bim#set ~key:1 ~data:"single";
      bim#set ~key:2 ~data:"double";
      bim#set ~key:3 ~data:"triple";
      let concat = bim#fold ~init:"" ~f:(fun ~key ~data init -> if key < 3 then (init ^ data) else init) in
      assert_equal "singledouble" concat;
      let sum = bim#fold_reverse
		      ~init:0
		      ~f:(fun ~key ~data init ->
			  if key.[0] = 't' || key.[0] = 'd'
			  then (data + init) else init) in
      assert_equal 5 sum;
      let dividend = bim#fold_reverse
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
	bim#fold_right_reverse
			 ~init:1 ~f:(fun ~key ~data init ->
				     if key.[0] ='t' || key.[0]='d'
				     then (data / init) else init) in
      assert_equal 0 dividend2;
      assert_equal true (bim#for_all ~f:(fun v -> if (String.length v) > 0 then true else false));
    end

  let test5 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module Bimap_single = Bimap.Bimap_single(Core.Int)(Core.String) in
    let bim = new Bimap_single.bimap_class int_map string_map in
    begin
      bim#set ~key:1 ~data:"uno";
      bim#set ~key:2 ~data:"dos";
      bim#set ~key:3 ~data:"tres";
      assert_equal true (bim#for_all ~f:(fun v -> (String.length v) > 1));
      assert_equal false (bim#for_all ~f:(fun v -> (String.length v) > 4));
      assert_equal true (bim#for_all_reverse ~f:(fun v -> v < 4));
      assert_equal false (bim#for_all_reverse ~f:(fun v -> v > 3));
      assert_equal [1;2;3] (bim#keys);
      assert_equal ["uno";"dos";"tres"] (bim#data);
      bim#map ~f:(fun v -> Core.String.concat [v;v;]);
      assert_equal ["unouno";"dosdos";"trestres"] (bim#data);
      bim#mapi ~f:(fun ~key ~data -> Core.String.concat [(Core.Int.to_string key);"->";data]);
      assert_equal ["1->unouno";"2->dosdos";"3->trestres"] (bim#data);
      assert_equal "1->unouno" (bim#find_exn ~key:1);
      assert_equal "2->dosdos" (bim#find_exn ~key:2);
      assert_equal 1 (bim#find_exn_reverse ~key:"1->unouno");
      assert_equal 2 (bim#find_exn_reverse ~key:"2->dosdos");

      bim#map_reverse ~f:(fun x -> x*2);
      assert_equal [2;4;6] (bim#keys);
      bim#mapi_reverse ~f:(fun ~key ~data -> if key.[0]='1' then
					       data*1
					     else
					       if key.[0]='2' then
						 data*2
					       else
						 data*3);
      assert_equal [2;8;18] (bim#keys);
    end

  let test6 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti = Bimap.Bimap_multi(Core.Int)(Core.String) in
    let bim = new BimapMulti.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";
      bim#add_multi_reverse ~key:"tri" ~data:3;
      bim#add_multi_reverse ~key:"iii" ~data:3;
      bim#add_multi_reverse ~key:"four" ~data:4;

      assert_equal 4 (bim#count ~f:(fun _l -> true));
      assert_equal 4 (bim#counti ~f:(fun ~key:_key ~data:_data -> true));
      assert_equal 1 (bim#counti ~f:(fun ~key ~data:_data -> key > 3));
      assert_equal 9 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 4 (bim#length);
      assert_equal 1 (Core.List.length (bim#find_exn ~key:1));
      assert_equal 2 (Core.List.length (bim#find_exn ~key:2));
      assert_equal 5 (Core.List.length (bim#find_exn ~key:3));
      assert_equal 1 (Core.List.length (bim#find_exn ~key:4));
      assert_equal true (List.mem "one" (bim#find_exn ~key:1));
      assert_equal true (List.mem "two" (bim#find_exn ~key:2));
      assert_equal true (List.mem "dos" (bim#find_exn ~key:2));
      assert_equal true (List.mem "three" (bim#find_exn ~key:3));
      assert_equal true (List.mem "tres" (bim#find_exn ~key:3));
      assert_equal true (List.mem "triple" (bim#find_exn ~key:3));
      assert_equal true (List.mem "tri" (bim#find_exn ~key:3));
      assert_equal true (List.mem "iii" (bim#find_exn ~key:3));
      
      assert_equal [1] (bim#find_exn_reverse ~key:"one");
      assert_equal [2] (bim#find_exn_reverse ~key:"two");
      assert_equal [2] (bim#find_exn_reverse ~key:"dos");
      assert_equal [3] (bim#find_exn_reverse ~key:"three");
      assert_equal [3] (bim#find_exn_reverse ~key:"tres");
      assert_equal [3] (bim#find_exn_reverse ~key:"triple");
      assert_equal [3] (bim#find_exn_reverse ~key:"tri");
      assert_equal [3] (bim#find_exn_reverse ~key:"iii");

      assert_equal (Some [1]) (bim#find_reverse ~key:"one");
      assert_equal (Some [2]) (bim#find_reverse ~key:"two");
      assert_equal (Some [2]) (bim#find_reverse ~key:"dos");
      assert_equal (Some [3]) (bim#find_reverse ~key:"three");
      assert_equal (Some [3]) (bim#find_reverse ~key:"tres");
      assert_equal (Some [3]) (bim#find_reverse ~key:"triple");
      assert_equal [3] (bim#find_exn_reverse ~key:"tri");
      
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);*)
      assert_equal ([["one"];["dos";"two"];["iii";"tri";"triple";"tres";"three"];["four"]]) (bim#data);
      (*print_n_flush_alist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
      assert_equal 1 (Core.List.length (Core.List.filter (bim#data_reverse) ~f:(fun x -> if x = [1] then true else false)));
      assert_equal 2 (Core.List.length (Core.List.filter (bim#data_reverse) ~f:(fun x -> if x = [2] then true else false)));
      assert_equal 5 (Core.List.length (Core.List.filter (bim#data_reverse) ~f:(fun x -> if x = [3] then true else false)));
      assert_equal 1 (Core.List.length (Core.List.filter (bim#data_reverse) ~f:(fun x -> if x = [4] then true else false)));

      bim#empty ();
      assert_equal 0 (bim#count ~f:(fun _l -> true));
      assert_equal 0 (bim#counti ~f:(fun ~key:_key ~data:_data -> true));
      assert_equal 0 (bim#counti ~f:(fun ~key ~data:_data -> key > 3));
      assert_equal 0 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 0 (bim#length);

      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";
      bim#add_multi_reverse ~key:"tri" ~data:3;

      assert_equal true (bim#exists ~f:(fun x -> Core.List.length x = 1 && Core.String.equal (Core.List.nth_exn x 0) "one"));
      assert_equal true (bim#exists_reverse ~f:(fun x -> x = [2]));
      assert_equal true (bim#existsi ~f:(fun ~key ~data -> Core.List.length data = 2 && key = 2 && List.mem "two" data));
      assert_equal false (bim#existsi ~f:(fun ~key ~data -> List.mem "one" data && key = 2));
      assert_equal true (bim#existsi_reverse ~f:(fun ~key ~data -> key = "tri" && data = [3]));
 
      bim#change ~key:3 ~f:(fun _x -> (Some ["iii";"tri";"triple";"tres";"three"]));
      assert_equal (Some [3]) (bim#find_reverse ~key:"three");
      assert_equal (Some [3]) (bim#find_reverse ~key:"tres");
      assert_equal (Some [3]) (bim#find_reverse ~key:"tri");
      assert_equal [3] (bim#find_exn_reverse ~key:"triple");
      assert_equal [3] (bim#find_exn_reverse ~key:"iii");
      assert_equal 5 (Core.List.length (bim#find_exn ~key:3));

      bim#change_reverse ~key:"two" ~f:(fun _x -> (Some [4]));
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);
      print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
      assert_equal 1 (Core.List.length (bim#find_exn ~key:4));
      assert_equal true (List.mem "two" (bim#find_exn ~key:4)); 
      assert_equal false (List.mem "dos" (bim#find_exn ~key:4));
      assert_equal [4] (bim#find_exn_reverse ~key:"two");
      assert_equal [2] (bim#find_exn_reverse ~key:"dos");
      assert_equal ["dos"] (bim#find_exn ~key:2);

      bim#change_reverse ~key:"two" ~f:(fun _x -> (Some [2;4]));
      (*print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> x) (bim#data);
      print_n_flush_alistlist ~sep:" | " ~to_string_func:(fun x -> Core.Int.to_string x) (bim#data_reverse);*)
      assert_equal 1 (Core.List.length (bim#find_exn ~key:4));
      assert_equal true (List.mem "two" (bim#find_exn ~key:4));
      assert_equal true (List.mem "two" (bim#find_exn ~key:2));
      assert_equal false (List.mem "dos" (bim#find_exn ~key:4));
      assert_equal true (List.mem 2 (bim#find_exn_reverse ~key:"two"));
      assert_equal true (List.mem 4 (bim#find_exn_reverse ~key:"two"));
      assert_equal [2] (bim#find_exn_reverse ~key:"dos");
      assert_equal true (List.mem "two" (bim#find_exn ~key:2));
      assert_equal true (List.mem "dos" (bim#find_exn ~key:2));
    end 

  let test7 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti = Bimap.Bimap_multi(Core.Int)(Core.String) in
    let bim = new BimapMulti.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter ~f:(fun v -> (Core.List.length v) < 3) ();
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 2 (bim#length);

      bim#filter_reverse ~f:(fun kl ->
          let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) kl in
          max < 2
        ) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);
    end

  let test8 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti = Bimap.Bimap_multi(Core.Int)(Core.String) in
    let bim = new BimapMulti.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filteri ~f:(fun ~key ~data -> key < 3 && (Core.List.length data) > 1) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 2 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filteri_reverse ~f:(fun ~key ~data ->
          let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) data in
          key.[0] = 't' && max < 3) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 1 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_keys ~f:(fun k -> k > 2) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_keys_reverse ~f:(fun k -> (String.length k) > 3) ();
      assert_equal 1 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 1 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_map ~f:(fun vlist -> if Core.List.length vlist < 3 then Some vlist else None) ();
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 2 (bim#length);

      bim#empty ();
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      bim#filter_map_reverse
        ~f:(fun invk ->
          let max = Core.List.fold ~init:0 ~f:(fun accum v -> if v > accum then v else accum) invk in
          if max < 3 then
            let newlist = Core.List.map invk ~f:(fun k -> k+1) in 
            Some (newlist)
          else
            None
        ) ();
      assert_equal 2 (bim#count ~f:(fun _x -> true));
      assert_equal 3 (bim#count_reverse ~f:(fun _x -> true));
      assert_equal 2 (bim#length);
      assert_equal 1 (Core.List.length (bim#find_exn ~key:2));
      assert_equal 2 (Core.List.length (bim#find_exn ~key:3));
      assert_equal true (List.mem "one" (bim#find_exn ~key:2));
      assert_equal true (List.mem "two" (bim#find_exn ~key:3)); 
      assert_equal true (List.mem "dos" (bim#find_exn ~key:3));
      assert_equal [2] (bim#find_exn_reverse ~key:"one");
      assert_equal [3] (bim#find_exn_reverse ~key:"two");
      assert_equal [3] (bim#find_exn_reverse ~key:"dos");
      assert_equal None (bim#find ~key:1);
    end 


  let test9 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti = Bimap.Bimap_multi(Core.Int)(Core.String) in
    let bim = new BimapMulti.bimap_multi_class int_map string_map in
    begin
      bim#add_multi ~key:1 ~data:"one";
      bim#add_multi ~key:2 ~data:"two";
      bim#add_multi ~key:3 ~data:"three";
      bim#add_multi ~key:2 ~data:"dos";
      bim#add_multi ~key:3 ~data:"tres";
      bim#add_multi ~key:3 ~data:"triple";

      let total_chars =
	bim#fold ~init:0 ~f:(fun ~key:_key ~data accum ->
			     (Core.List.fold data ~init:accum
					     ~f:(fun accum data -> (String.length data) + accum))) in
      assert_equal 24 total_chars;
      let keys_times_num_values =
	bim#fold_reverse ~init:0 ~f:(fun ~key:_key ~data accum ->
            let subtotal = Core.List.fold data ~init:0 ~f:(fun accum x -> accum + x) in 
	    accum + subtotal) in
      assert_equal 14 keys_times_num_values;
      (*fold over keys in decreasing order instead of increasing--in this case reverse alphabetical order*)
      let folded_right =
	bim#fold_right ~init:None
		       ~f:(fun ~key ~data:_data accum ->
			   match accum with
			   | None -> Some key
			   | Some i -> Some (i - key)
			  ) in
      assert_equal (Some 0) folded_right;
      let folded_right_reverse = bim#fold_right_reverse
				       ~init:[]
				       ~f:(fun ~key ~data:_data accum ->
					   key::accum
					  ) in
      (*print_n_flush_alist ~sep:"|" ~to_string_func:(fun x -> x) folded_right_reverse;*)
      assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_reverse) 0);
      let forall = bim#for_all
			 ~f:(fun l ->
			     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 3))
			    ) in
      assert_equal forall false;
      assert_equal "two" (Core.List.nth_exn (Core.List.rev folded_right_reverse) 0);
      let forall = bim#for_all
			 ~f:(fun l ->
			     (Core.List.for_all l ~f:(fun x -> Core.String.length x > 2))
			    ) in
      assert_equal forall true;
      let forallreverse =
        bim#for_all_reverse
          ~f:(fun x ->
            let max = Core.List.fold x ~init:0 ~f:(fun accum x -> if x > accum then x else accum) in
            max < 4) in
      assert_equal forallreverse true;
      let forallreverse =
        bim#for_all_reverse
          ~f:(fun x ->
            let max = Core.List.fold x ~init:0 ~f:(fun accum x -> if x > accum then x else accum) in
            max < 3) in
      assert_equal forallreverse false;
      assert_equal false (bim#is_empty);
      assert_equal [1;2;3] (bim#keys);
      (*let () print_n_flush_alist ~sep:"," ~to_string_func:(fun x -> x) reverse_keys in *)
      assert_equal true (List.mem "dos" (bim#keys_reverse));
      assert_equal 1 (Core.List.length (Core.List.filter (bim#keys_reverse) ~f:(fun x -> String.equal x "dos")));
      assert_equal true (List.mem "one" (bim#keys_reverse));
      assert_equal true (List.mem "three" (bim#keys_reverse));
      assert_equal true (List.mem "tres" (bim#keys_reverse));
      assert_equal true (List.mem "triple" (bim#keys_reverse));
      assert_equal true (List.mem "two" (bim#keys_reverse));

      assert_equal 3 (bim#length);

      bim#map ~f:(fun l -> let len = Core.List.length l in
			   match len with
			   | 1 -> ["1"]@l
			   | 2 -> ["2"]@l
			   | 3 -> ["3"]@l
			   | _ -> ["unexpectedlen"]@l);
      assert_equal 2 (Core.List.length (bim#find_exn ~key:1));
      assert_equal true (List.mem "1" (bim#find_exn ~key:1));
      assert_equal 3 (Core.List.length (bim#find_exn ~key:2));
      assert_equal true (List.mem "2" (bim#find_exn ~key:2));
      assert_equal 4 (Core.List.length (bim#find_exn ~key:3));
      assert_equal true (List.mem "3" (bim#find_exn ~key:3));
      assert_equal [1] (bim#find_exn_reverse ~key:"1");
      assert_equal [2] (bim#find_exn_reverse ~key:"2");
      assert_equal [3] (bim#find_exn_reverse ~key:"3");
      assert_equal [1] (bim#find_exn_reverse ~key:"one");
      assert_equal [2] (bim#find_exn_reverse ~key:"two");
      assert_equal [3] (bim#find_exn_reverse ~key:"three");
    end

  let test10 _text_ctx =
    let string_map = Core.String.Map.empty in
    let int_map = Core.Int.Map.empty in
    let module BimapMulti = Bimap.Bimap_multi(Core.Int)(Core.String) in
    let bim = new BimapMulti.bimap_multi_class int_map string_map in
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
      assert_equal true (bim#mem_reverse "one");
      assert_equal true (bim#mem_reverse "two");
      assert_equal true (bim#mem_reverse "three");
      
      bim#map_reverse ~f:(fun x ->
          Core.List.map x ~f:(fun v -> v * 10));
      assert_equal [10] (bim#find_exn_reverse ~key:"one");
      assert_equal [20] (bim#find_exn_reverse ~key:"two");
      assert_equal [30] (bim#find_exn_reverse ~key:"three");
      assert_equal None (bim#find ~key:1);
      assert_equal None (bim#find ~key:2);
      assert_equal None (bim#find ~key:3);
      assert_equal true (List.mem "one" (bim#find_exn ~key:10));
      assert_equal true (List.mem "two" (bim#find_exn ~key:20));
      assert_equal true (List.mem "three" (bim#find_exn ~key:30));

      assert_equal (Some (10, ["one"])) (bim#min_elt);
      assert_equal (10, ["one"]) (bim#min_elt_exn);
      assert_equal (Some (30, ["triple";"tres";"three"])) (bim#max_elt);
      assert_equal (30,["triple";"tres";"three"]) (bim#max_elt_exn);

      assert_equal (Some ("dos", [20])) (bim#min_elt_reverse);
      assert_equal ("dos", [20]) (bim#min_elt_exn_reverse);
      assert_equal (Some ("two", [20])) (bim#max_elt_reverse);
      assert_equal ("two",[20]) (bim#max_elt_exn_reverse);

      assert_equal (Some (10, ["one"])) (bim#nth 0);
      assert_equal (Some (20, ["two";"dos"])) (bim#nth 1);
      assert_equal (Some (30, ["triple";"tres";"three"])) (bim#nth 2);
      assert_equal (Some ("dos",[20])) (bim#nth_reverse 0);
      assert_equal (Some ("one",[10])) (bim#nth_reverse 1);
      assert_equal (Some ("three",[30])) (bim#nth_reverse 2);
      assert_equal (Some ("tres",[30])) (bim#nth_reverse 3);
      assert_equal (Some ("triple",[30])) (bim#nth_reverse 4);
      assert_equal (Some ("two",[20])) (bim#nth_reverse 5);

      bim#remove ~key:20;
      assert_equal None (bim#find ~key:20);
      assert_equal None (bim#find_reverse ~key:"two");

      bim#remove_reverse ~key:"triple";
      assert_equal false (List.mem "triple" (bim#find_exn ~key:30));
      assert_equal true (List.mem "three" (bim#find_exn ~key:30));
      assert_equal true (List.mem "tres" (bim#find_exn ~key:30));

      bim#remove_multi ~key:30;
      assert_equal false (List.mem "triple" (bim#find_exn ~key:30));
      assert_equal false (List.mem "tres" (bim#find_exn ~key:30));
      assert_equal true (List.mem "three" (bim#find_exn ~key:30));
      assert_equal 1 (Core.List.length (bim#find_exn ~key:30));
      assert_equal None (bim#find_reverse ~key:"tres");
      assert_equal (Some [30]) (bim#find_reverse ~key:"three");

      bim#update ~key:30 ~f:(fun l ->
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
      assert_equal true (List.mem "triple" (bim#find_exn ~key:30));
      assert_equal true (List.mem "tres" (bim#find_exn ~key:30));
      assert_equal true (List.mem "three" (bim#find_exn ~key:30));
      assert_equal (Some [30]) (bim#find_reverse ~key:"tres");
      assert_equal (Some [30]) (bim#find_reverse ~key:"triple");
      assert_equal (Some [30]) (bim#find_reverse ~key:"three");
    end 

  let test11 _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = { Bimap_single_module.fwdmap=Core.Int.Map.empty; revmap=Core.String.Map.empty } in 
    let t1 = Bimap_single_module.set t ~key:1 ~data:"one" in 
    let t2 = Bimap_single_module.set t1 ~key:2 ~data:"two" in 
    let () = assert_equal 2 (Bimap_single_module.length t2) in 
    let () = assert_equal false (Bimap_single_module.is_empty t2) in 
    let () = assert_equal "one" (Bimap_single_module.find_exn t2 ~key:1) in 
    let () = assert_equal "two" (Bimap_single_module.find_exn t2 ~key:2) in
    let () = assert_equal 1 (Bimap_single_module.find_exn_reverse t2 ~key:"one") in
    let () = assert_equal 2 (Bimap_single_module.find_exn_reverse t2 ~key:"two") in 
    let t3 = Bimap_single_module.set_reverse t2 ~key:"three" ~data:3 in 
    let () = assert_equal "three" (Bimap_single_module.find_exn t3 ~key:3) in
    let () = assert_equal 3 (Bimap_single_module.find_exn_reverse t3 ~key:"three") in
    let () = assert_equal [(1,"one");(2,"two");(3,"three")] (Bimap_single_module.to_alist t3) in 
    let () = assert_equal [(3,"three");(2,"two");(1,"one")] (Bimap_single_module.to_alist t3 ~key_order:`Decreasing) in
    let t4 = Bimap_single_module.change t3 ~key:3 ~f:(fun _x -> (Some "tri")) in
    let () = assert_equal "tri" (Bimap_single_module.find_exn t4 ~key:3) in 
    let () = assert_equal 3 (Bimap_single_module.find_exn_reverse t4 ~key:"tri") in
    let t5 = Bimap_single_module.change_reverse t4 ~key:"tri" ~f:(fun _x -> (Some 4)) in
    let () = assert_equal 4 (Bimap_single_module.find_exn_reverse t5 ~key:"tri") in 
    let () = assert_equal "tri" (Bimap_single_module.find_exn t5 ~key:4) in
    let t6 = Bimap_single_module.remove t5 ~key:1 in
    let () = assert_equal 2 (Bimap_single_module.length t6) in 
    let t7 = Bimap_single_module.remove_reverse t6 ~key:"two" in 
    let () = assert_equal 1 (Bimap_single_module.length t7) in
    let t8 = Bimap_single_module.remove t7 ~key:4 in
    assert_equal 0 (Bimap_single_module.length t8);;

  let test12 _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = { Bimap_single_module.fwdmap=Core.Int.Map.empty; revmap=Core.String.Map.empty } in 
    let t2 = Bimap_single_module.set t ~key:1 ~data:"one" in 
    let t3 = Bimap_single_module.set t2 ~key:2 ~data:"two" in
    let () = assert_equal 2 (Bimap_single_module.length t3) in
    let () = assert_equal false (Bimap_single_module.is_empty t3) in
    let () = assert_equal (Some "one") (Bimap_single_module.find t3 ~key:1) in
    let () = assert_equal (Some "two") (Bimap_single_module.find t3 ~key:2) in
    let () = assert_equal (Some 1) (Bimap_single_module.find_reverse t3 ~key:"one") in
    let () = assert_equal (Some 2) (Bimap_single_module.find_reverse t3 ~key:"two") in
    let () = assert_equal None (Bimap_single_module.find t3 ~key:3) in
    let () = assert_equal None (Bimap_single_module.find_reverse t3 ~key:"three") in
    let t4 = Bimap_single_module.set_reverse t3 ~key:"three" ~data:3 in
    let () = assert_equal (Some "three") (Bimap_single_module.find t4 ~key:3) in
    let () = assert_equal (Some 3) (Bimap_single_module.find_reverse t4 ~key:"three") in 
    let t5 = Bimap_single_module.change t4 ~key:3 ~f:(fun _x -> (Some "tri")) in 
    let () = assert_equal 3 (Bimap_single_module.find_exn_reverse t5 ~key:"tri") in
    let t6 = Bimap_single_module.change_reverse t5 ~key:"tri" ~f:(fun _x -> (Some 4)) in 
    let () = assert_equal (Some 4) (Bimap_single_module.find_reverse t6 ~key:"tri") in 
    let () = assert_equal (Some "tri") (Bimap_single_module.find t6 ~key:4) in 
    let () = assert_equal true (Bimap_single_module.mem t6 1) in 
    let () = assert_equal true (Bimap_single_module.mem t6 2) in 
    let () = assert_equal true (Bimap_single_module.mem_reverse t6 "tri") in
    let () = assert_equal (Some (1,"one")) (Bimap_single_module.min_elt t6) in
    let () = assert_equal (Some (4,"tri")) (Bimap_single_module.max_elt t6) in
    let () = assert_equal (1,"one") (Bimap_single_module.min_elt_exn t6) in
    let () = assert_equal (4,"tri") (Bimap_single_module.max_elt_exn t6) in
    let () = assert_equal (Some ("one",1)) (Bimap_single_module.min_elt_reverse t6) in
    let () = assert_equal (Some ("two",2)) (Bimap_single_module.max_elt_reverse t6) in
    let () = assert_equal ("one",1) (Bimap_single_module.min_elt_exn_reverse t6) in
    let () = assert_equal ("two",2) (Bimap_single_module.max_elt_exn_reverse t6) in
    let () = assert_equal (Some (2,"two")) (Bimap_single_module.nth t6 1) in
    let () = assert_equal (Some (4,"tri")) (Bimap_single_module.nth t6 2) in
    let () = assert_equal (Some ("one",1)) (Bimap_single_module.nth_reverse t6 0) in
    let () = assert_equal (Some ("tri",4)) (Bimap_single_module.nth_reverse t6 1) in 
    let () = assert_equal (Some ("two",2)) (Bimap_single_module.nth_reverse t6 2) in
    assert_equal None (Bimap_single_module.nth_reverse t6 3);;


  let test13 _text_ctx =
    let module Bimap_single_module = Bimap.Bimap_single_module(Core.Int)(Core.String) in
    let t = { Bimap_single_module.fwdmap=Core.Int.Map.empty; revmap=Core.String.Map.empty } in 
    let t2 = Bimap_single_module.set t ~key:1 ~data:"single" in 
    let t3 = Bimap_single_module.set t2 ~key:2 ~data:"double" in 
    let t4 = Bimap_single_module.set t3 ~key:3 ~data:"triple" in
    let () = assert_equal 3 (Bimap_single_module.count t4 ~f:(fun _x -> true)) in 
    let () = assert_equal 3 (Bimap_single_module.length t4) in 
    let () = assert_equal 3 (Bimap_single_module.count_reverse t4 ~f:(fun _x -> true)) in 
    let () = assert_equal 2 (Bimap_single_module.counti t4 ~f:(fun ~key ~data:_data -> if key <= 2 then true else false)) in
    let () = assert_equal ["single";"double";"triple"] (Bimap_single_module.data t4) in 
    let () = assert_equal true (List.mem 1 (Bimap_single_module.data_reverse t4)) in
    let () = assert_equal true (List.mem 2 (Bimap_single_module.data_reverse t4)) in 
    let () = assert_equal true (List.mem 3 (Bimap_single_module.data_reverse t4)) in
    let () = assert_equal true (Bimap_single_module.exists t4 ~f:(fun x -> x = "double")) in 
    let () = assert_equal true (Bimap_single_module.exists t4 ~f:(fun x -> x = "triple")) in
    let () = assert_equal false (Bimap_single_module.exists t4 ~f:(fun x -> x = "quadruple")) in
    let () = assert_equal true (Bimap_single_module.exists_reverse t4 ~f:(fun x -> x = 2)) in 
    let () = assert_equal false (Bimap_single_module.exists_reverse t4 ~f:(fun x -> x = 4)) in 
    let () = assert_equal true (Bimap_single_module.existsi t4 ~f:(fun ~key ~data -> key = 3 && data = "triple")) in 
    let () = assert_equal true (Bimap_single_module.existsi_reverse t4 ~f:(fun ~key ~data -> key = "double" && data = 2)) in 
    let t5 = Bimap_single_module.empty () in
    let () = assert_equal 0 (Bimap_single_module.length t5) in 
    let () = assert_equal 0 (Bimap_single_module.count t5 ~f:(fun _x -> true)) in 
    let () = assert_equal true (Bimap_single_module.is_empty t5) in 
    let t6 = Bimap_single_module.set t5 ~key:1 ~data:"one" in 
    let t7 = Bimap_single_module.set t6 ~key:2 ~data:"two" in 
    let t8 = Bimap_single_module.set t7 ~key:3 ~data:"three" in
    let t9 = Bimap_single_module.update t8 ~key:1
               ~f:(fun x -> match x with
			      Some s -> Core.String.rev s
			    | None -> "newentry") in 
    assert_equal "eno" (Bimap_single_module.find_exn t9 ~key:1);;

  let suite =
    "suite">:::
      ["test-1">:: test1;
       "test-2">:: test2;
       "test-3">:: test3;
       "test-3b">:: test3b;
       "test-4">:: test4;
       "test-5">:: test5;
       "test-6">:: test6;
       "test-7">:: test7;
       "test-8">:: test8;
       "test-9">:: test9;
       "test-10">:: test10;
       "test-11">:: test11;
       "test-12">:: test12];;

  let () =
    run_test_tt_main suite
	
end 
